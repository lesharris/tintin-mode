;;; tintin-mode.el --- Major mode for editing TinTin++ config files -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Les Harris

;; Author: Les Harris <les@lesharris.com>
;; Version: 0.3
;; URL: https://github.com/lesharris/tintin-mode
;; Package-Requires: ((emacs "24.3"))

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;;; Commentary:

;; This package provides a major mode for editing TinTin++ configuration
;; files. TinTin++ is a MUD client that uses scripting files with .tin
;; and .tt extensions.
;;
;; Example TinTin++ code this mode highlights:
;;
;;   #nop This is a comment
;;   #variable {hp} {100}
;;   #action {%0 tells you '%1'} {
;;       #if {"%1" == "hello"} {
;;           tell %0 Hello there!
;;       }
;;   }
;;   #function {heal} {
;;       #if {$hp < 50} {
;;           cast heal
;;       }
;;   }
;;
;; Features:
;; - Syntax highlighting for TinTin++ commands, variables, and functions
;; - Proper indentation with customizable offset
;; - Comment support (#nop commands)
;; - Imenu integration for navigation to functions and variables
;; - Electric pair insertion for braces and parentheses
;; - Outline mode support for code folding
;; - Command completion for TinTin++ keywords
;; - Which-function-mode support
;; - Eldoc integration for command documentation in the echo area

;;; Code:

(defgroup tintin nil
  "Major mode for editing TinTin++ config files."
  :group 'languages
  :prefix "tintin-")

(defcustom tintin-indent-offset 2
  "Number of spaces for each indentation level in TinTin++ mode."
  :type 'integer
  :safe 'integerp
  :group 'tintin)

(defvar tintin-mode-hook nil
  "Hook run when entering TinTin++ mode.")

(defvar tintin-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") 'tintin-newline-and-indent)
    (define-key map "\C-j" 'tintin-newline-and-indent)
    (define-key map "\C-c\C-c" 'comment-region)
    (define-key map "\C-c\C-u" 'uncomment-region)
    (define-key map "\C-c\C-f" 'imenu) ; Quick function/variable navigation
    (define-key map "\C-c\C-o" 'outline-toggle-children) ; Code folding
    map)
  "Keymap for TinTin++ major mode.")

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.tin\\'" . tintin-mode))
;;;###autoload
(add-to-list 'auto-mode-alist '("\\.tt\\'" . tintin-mode))

;; Face definitions - inherit from standard font-lock faces for theme compatibility
(defface tintin-ansi-face
  '((t :inherit font-lock-constant-face))
  "Face for ANSI color codes."
  :group 'tintin)

(defface tintin-symbol-face
  '((t :inherit default))
  "Face for symbols."
  :group 'tintin)

(defface tintin-var-face
  '((t :inherit font-lock-variable-name-face))
  "Face for variables."
  :group 'tintin)

(defface tintin-var-def-face
  '((t :inherit font-lock-variable-name-face :weight bold))
  "Face for variable definitions."
  :group 'tintin)

(defface tintin-conditional-face
  '((t :inherit font-lock-keyword-face))
  "Face for conditionals and loops."
  :group 'tintin)

(defface tintin-comment-face
  '((t :inherit font-lock-comment-face))
  "Face for comments."
  :group 'tintin)

(defface tintin-function-face
  '((t :inherit font-lock-function-name-face))
  "Face for user functions."
  :group 'tintin)

(defface tintin-function-def-face
  '((t :inherit font-lock-function-name-face :weight bold))
  "Face for user function definitions."
  :group 'tintin)

(defface tintin-hash-face
  '((t :inherit font-lock-builtin-face))
  "Face for hash commands."
  :group 'tintin)

(defvar tintin-commands
  '("act" "action" "alias" "all" "bell" "break" "buffer" "button"
    "case" "cat" "chat" "class" "config" "cr" "cursor"
    "daemon" "debug" "default" "delay" "draw"
    "echo" "edit" "else" "elseif" "end" "event"
    "foreach" "format" "func" "function"
    "gag" "greeting" "grep"
    "help" "highlight" "history"
    "if" "ignore" "info" "introduction"
    "keypad" "kill"
    "line" "list" "lists" "local" "log" "loop"
    "macro" "map" "math" "message" "mouse"
    "nop"
    "parse" "path" "pathdir" "port" "prompt"
    "read" "regexp" "repeat" "replace" "return"
    "save" "scan" "screen" "script" "send" "session" "show" "showme"
    "snoop" "split" "ssl" "sub" "substitute" "switch" "system"
    "tab" "textin" "ticker" "trigger"
    "ungag" "unhighlight" "unvar" "unvariable"
    "var" "variable"
    "while" "write"
    "zap")
  "List of TinTin++ commands for completion.")

(defvar tintin-command-annotations
  '(("action" . " trigger")
    ("alias" . " shortcut")
    ("variable" . " var")
    ("var" . " var")
    ("func" . " fn")
    ("function" . " fn")
    ("if" . " conditional")
    ("loop" . " loop")
    ("while" . " loop")
    ("foreach" . " loop")
    ("echo" . " output")
    ("showme" . " output")
    ("math" . " calc")
    ("format" . " format")
    ("replace" . " replace")
    ("regexp" . " regex"))
  "Annotations for TinTin++ commands shown in completion.")

(defconst tintin-font-lock-keywords
  `(
    ;; Comments
    (,(rx line-start (zero-or-more space)
          "#" (or "nop" "NOP" "no" "No" "NO")
          (zero-or-more nonl))
     . 'tintin-comment-face)

    ;; Strings (single line only)
    (,(rx "\"" (*? (not (any "\"\n"))) "\"")
     . 'font-lock-string-face)
    (,(rx "'" (*? (not (any "'\n"))) "'")
     . 'font-lock-string-face)

    ;; Variable definitions - #var, #variable
    (,(rx "#" (or "var" "VAR" "variable" "VARIABLE" "local" "LOCAL")
          (one-or-more space)
          (group (or (seq "{" (*? (not "}")) "}")
                     (one-or-more (not space)))))
     (1 'tintin-var-def-face))

    ;; Function definitions
    (,(rx "#" (or "function" "FUNCTION" "func" "FUNC")
          (one-or-more space)
          (group (or (seq "{" (*? (not "}")) "}")
                     (one-or-more (not space)))))
     (1 'tintin-function-def-face))

    ;; Conditionals and loops (before catch-all)
    (,(rx "#" (group (or "if" "IF" "else" "ELSE" "elseif" "ELSEIF"
                         "loop" "LOOP" "while" "WHILE" "foreach" "FOREACH"))
          (or space eol "{"))
     (1 'tintin-conditional-face))

    ;; Function calls @function{args}
    (,(rx "@" (group (one-or-more (or alnum "_"))))
     (1 'tintin-function-face))

    ;; Variables with array syntax
    ;; *VAR[index], $VAR[index], %VAR[index], &VAR[index]
    (,(rx (or "*" "%" "$" "&")
          (group (one-or-more (or alnum "_"))
                 (* "[" (*? (not (any "]\n"))) "]")))
     (1 'tintin-var-face))

    ;; ANSI color codes
    (,(rx "<" (group (or
                      ;; 24-bit truecolor: F000000-FFFFFFF, B000000-BFFFFFF
                      (seq (any "FB") (repeat 6 7 hex-digit))
                      ;; 12-bit truecolor: F000-FFFF, B000-BFFF
                      (seq (any "FB") (repeat 3 4 hex-digit))
                      ;; Grayscale: g00-g23, G00-G23
                      (seq (any "gG") (repeat 2 digit))
                      ;; RGB hex: aaa-fff, AAA-FFF (3 hex chars)
                      (repeat 3 hex-digit)
                      ;; Basic VT100: 1-3 digits
                      (repeat 1 3 digit))) ">")
     (1 'tintin-ansi-face))

    ;; Brackets and operators
    (,(rx (group (or "[" "]" "(" ")" "{" "}" ";" "+" "*" "-" "/")))
     (1 'tintin-symbol-face))

    ;; Hash commands (catch-all, keep this last)
    (,(rx "#" (group (one-or-more alnum))
          (or space eol "{" ";"))
     (1 'tintin-hash-face)))
  "Font lock keywords for TinTin++ mode.")

(defvar tintin-mode-syntax-table
  (let ((st (make-syntax-table)))
    (modify-syntax-entry ?_ "w" st)
    (modify-syntax-entry ?\{ "(}" st)
    (modify-syntax-entry ?\} "){" st)
    st)
  "Syntax table for TinTin++ mode.")

(defun tintin-in-string-or-comment-p (&optional pos)
  "Return non-nil if POS (or point) is inside a string or comment.
Uses font-lock face properties since TinTin++ comments (#nop) cannot
be expressed through the syntax table."
  (let ((pos (or pos (point))))
    (let ((face (get-text-property pos 'face)))
      (memq face '(font-lock-string-face
                   font-lock-comment-face
                   tintin-comment-face)))))

(defun tintin-calculate-indentation ()
  "Calculate the indentation for the current line."
  (save-excursion
    (beginning-of-line)
    (let ((cur-line (buffer-substring-no-properties
                     (line-beginning-position)
                     (line-end-position))))
      (cond
       ;; First line
       ((bobp) 0)

       ;; Closing brace - find its matching opening brace
       ((string-match "^[ \t]*}" cur-line)
        (save-excursion
          ;; Find the } on the current line
          (back-to-indentation)
          (condition-case nil
              (progn
                ;; Go to the matching {
                (forward-char)  ; Move past the }
                (backward-sexp) ; Jump to matching {
                (current-indentation))
            (error 0))))

       ;; Normal line - find previous non-empty line
       (t
        ;; Skip back over empty lines
        (forward-line -1)
        (while (and (not (bobp))
                    (looking-at "^[ \t]*$"))
          (forward-line -1))

        (let ((prev-indent (current-indentation))
              (line-start (line-beginning-position))
              (line-end (line-end-position))
              (net-braces 0))
          ;; Count braces on the previous non-empty line
          (goto-char line-start)
          (while (and (< (point) line-end)
                      (re-search-forward "[{}]" line-end t))
            (let ((brace-pos (1- (point)))
                  (brace-char (char-before)))
              (save-excursion
                (unless (tintin-in-string-or-comment-p brace-pos)
                  (if (eq brace-char ?{)
                      (setq net-braces (1+ net-braces))
                    (setq net-braces (1- net-braces)))))))

          (max 0 (+ prev-indent (* net-braces tintin-indent-offset)))))))))

(defun tintin-newline-and-indent ()
  "Insert a newline and indent, handling brace pairs."
  (interactive)
  (let ((at-brace-pair (and (eq (char-before) ?{)
                            (eq (char-after) ?}))))
    (if at-brace-pair
        (progn
          ;; Insert newline and indent current position
          (insert "\n")
          (indent-according-to-mode)
          ;; Move to next line (where } is) and indent it
          (save-excursion
            (forward-line 1)
            (indent-according-to-mode)))
      ;; Normal case
      (insert "\n")
      (indent-according-to-mode))))

(defun tintin-indent-line ()
  "Indent current line as TinTin++ code."
  (interactive)
  (let* ((indent-col (tintin-calculate-indentation))
         (cur-indent (current-indentation))
         (pos (- (point-max) (point))))

    (unless (= cur-indent indent-col)
      (beginning-of-line)
      (delete-horizontal-space)
      (indent-to indent-col)

      ;; Restore point position relative to indentation
      (when (> (- (point-max) pos) (point))
        (goto-char (- (point-max) pos))))))

(defun tintin-indent-region (start end)
  "Indent region from START to END."
  (interactive "r")
  (save-excursion
    (goto-char start)
    (while (< (point) end)
      (unless (looking-at "^[ \t]*$")
        (tintin-indent-line))
      (forward-line 1))))

(defun tintin-imenu-create-index ()
  "Create an imenu index for TinTin++ functions and variables."
  (let ((functions '())
        (variables '()))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward
              (rx "#" (or "function" "FUNCTION") (one-or-more space)
                  (or (seq "{" (group (*? (not "}"))) "}")
                      (group (one-or-more (not space)))))
              nil t)
        (let ((name (or (match-string 1) (match-string 2))))
          (when name
            (push (cons name (match-beginning 0)) functions))))

      (goto-char (point-min))
      (while (re-search-forward
              (rx "#" (or "var" "VAR" "variable" "VARIABLE") (one-or-more space)
                  (or (seq "{" (group (*? (not "}"))) "}")
                      (group (one-or-more (not space)))))
              nil t)
        (let ((name (or (match-string 1) (match-string 2))))
          (when name
            (push (cons name (match-beginning 0)) variables)))))

    (append (when functions (list (cons "Functions" (nreverse functions))))
            (when variables (list (cons "Variables" (nreverse variables)))))))

(defun tintin-completion-at-point ()
  "Provide completion for TinTin++ commands with metadata.
Works with both Company and Corfu through `completion-at-point'."
  (cond
   ((looking-back "#\\([a-zA-Z]*\\)" (line-beginning-position))
    (let ((start (match-beginning 1))
          (end (match-end 1)))
      (list start end tintin-commands
            :annotation-function #'tintin-annotate-command
            :exclusive 'no)))

   ((looking-back "[$%*&]\\([a-zA-Z_][a-zA-Z0-9_]*\\)?" (line-beginning-position))
    (let ((start (match-beginning 1))
          (end (match-end 1))
          (vars (tintin-collect-variables)))
      (when vars
        (list start end vars
              :annotation-function (lambda (_) " var")
              :exclusive 'no))))

   ((looking-back "@\\([a-zA-Z_][a-zA-Z0-9_]*\\)?" (line-beginning-position))
    (let ((start (match-beginning 1))
          (end (match-end 1))
          (funcs (tintin-collect-functions)))
      (when funcs
        (list start end funcs
              :annotation-function (lambda (_) " fn")
              :exclusive 'no))))))

(defun tintin-annotate-command (candidate)
  "Return annotation for CANDIDATE command."
  (or (cdr (assoc candidate tintin-command-annotations)) ""))

(defun tintin-collect-variables ()
  "Collect all variable names defined in current buffer."
  (let ((vars '()))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward
              (rx "#" (or "var" "VAR" "variable" "VARIABLE" "local" "LOCAL")
                  (one-or-more space)
                  (or (seq "{" (group (+ (not (any "}")))) "}")
                      (group (+ (not space)))))
              nil t)
        (let ((name (or (match-string 1) (match-string 2))))
          (when (and name (not (member name vars)))
            (push name vars)))))
    (nreverse vars)))

(defun tintin-collect-functions ()
  "Collect all function names defined in current buffer."
  (let ((funcs '()))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward
              (rx "#" (or "function" "FUNCTION")
                  (one-or-more space)
                  (or (seq "{" (group (+ (not (any "}")))) "}")
                      (group (+ (not space)))))
              nil t)
        (let ((name (or (match-string 1) (match-string 2))))
          (when (and name (not (member name funcs)))
            (push name funcs)))))
    (nreverse funcs)))

(defun tintin-outline-level ()
  "Return the outline level for the current line."
  (save-excursion
    (beginning-of-line)
    (cond
     ((looking-at "#\\(function\\|session\\)") 1)
     ((looking-at "#\\(if\\|loop\\|while\\|foreach\\)") 2)
     (t 3))))

(defvar tintin-command-docs
  '(("action" . "#action {pattern} {commands} {priority} - Trigger commands on matching output")
    ("act" . "#act {pattern} {commands} {priority} - Trigger commands on matching output")
    ("alias" . "#alias {name} {commands} - Create a command shortcut")
    ("all" . "#all {commands} - Send commands to all sessions")
    ("bell" . "#bell - Ring the terminal bell")
    ("break" . "#break - Break out of a loop")
    ("buffer" . "#buffer {command} - Manipulate the scrollback buffer")
    ("button" . "#button {square} {commands} {priority} - Create a clickable button")
    ("case" . "#case {value} {commands} - Match a value in a #switch block")
    ("cat" . "#cat {variable} {value} - Concatenate a value to a variable")
    ("chat" . "#chat {command} {args} - Peer-to-peer chat commands")
    ("class" . "#class {name} {command} - Group triggers/aliases into a class")
    ("config" . "#config {option} {value} - Set configuration options")
    ("cr" . "#cr - Send a carriage return")
    ("cursor" . "#cursor {command} - Input line cursor manipulation")
    ("daemon" . "#daemon {name} {command} - Run a background session")
    ("debug" . "#debug {command} {arg} - Toggle debug information")
    ("default" . "#default {commands} - Default case in a #switch block")
    ("delay" . "#delay {seconds|name} {commands} - Execute commands after a delay")
    ("draw" . "#draw {option} {square} {text} - Draw on the screen")
    ("echo" . "#echo {format} {args} - Display formatted text locally")
    ("edit" . "#edit {command} - Edit triggers/aliases")
    ("else" . "#else {commands} - Execute if previous #if was false")
    ("elseif" . "#elseif {condition} {commands} - Conditional else branch")
    ("end" . "#end - Terminate TinTin++")
    ("event" . "#event {type} {commands} - Trigger on system events")
    ("foreach" . "#foreach {list} {variable} {commands} - Iterate over list items")
    ("format" . "#format {variable} {format} {args} - Printf-style formatting")
    ("func" . "#func {name} {commands} - Define a function (returns #result)")
    ("function" . "#function {name} {commands} - Define a function (returns #result)")
    ("gag" . "#gag {pattern} - Suppress matching output lines")
    ("grep" . "#grep {pattern} - Search scrollback buffer")
    ("help" . "#help {command} - Show help for a command")
    ("highlight" . "#highlight {pattern} {color} - Colorize matching output")
    ("history" . "#history {command} - Manipulate command history")
    ("if" . "#if {condition} {true-commands} - Conditional execution")
    ("ignore" . "#ignore {command} - Toggle trigger processing")
    ("info" . "#info {command} - Show system information")
    ("kill" . "#kill {type} {pattern} - Remove triggers/aliases/etc")
    ("line" . "#line {option} {commands} - Modify command line behavior")
    ("list" . "#list {variable} {command} {args} - List manipulation commands")
    ("local" . "#local {name} {value} - Define a local variable")
    ("log" . "#log {option} {filename} - Log session output to file")
    ("loop" . "#loop {start} {end} {variable} {commands} - Numeric for loop")
    ("macro" . "#macro {key} {commands} - Bind commands to a key sequence")
    ("map" . "#map {command} {args} - Automapper commands")
    ("math" . "#math {variable} {expression} - Evaluate math expression")
    ("message" . "#message {type} {on|off} - Toggle system messages")
    ("mouse" . "#mouse {event} {commands} - Handle mouse events")
    ("nop" . "#nop {text} - Comment, ignored by parser")
    ("parse" . "#parse {string} {variable} {commands} - Iterate over characters")
    ("path" . "#path {command} - Speedwalk path commands")
    ("pathdir" . "#pathdir {dir} {reverse} {coord} - Define path directions")
    ("port" . "#port {command} {args} - Socket/port commands")
    ("prompt" . "#prompt {pattern} {substitution} - Modify the prompt line")
    ("read" . "#read {filename} - Read and execute a TinTin++ file")
    ("regexp" . "#regexp {string} {pattern} {true} {false} - Regular expression match")
    ("repeat" . "#repeat {count} {commands} - Repeat commands N times")
    ("replace" . "#replace {variable} {old} {new} - Replace text in a variable")
    ("return" . "#return {value} - Return a value from a #function")
    ("save" . "#save {filename} - Save current session to file")
    ("scan" . "#scan {option} {filename} - Read file as mud output")
    ("screen" . "#screen {command} {args} - Terminal screen manipulation")
    ("script" . "#script {variable} {shell command} - Run shell command, capture output")
    ("send" . "#send {text} - Send text to the mud")
    ("session" . "#session {name} {host} {port} - Open a mud connection")
    ("show" . "#show {text} - Display text locally (deprecated, use #showme)")
    ("showme" . "#showme {text} - Display text locally")
    ("snoop" . "#snoop {session} - Toggle viewing another session's output")
    ("split" . "#split {top} {bottom} - Split the screen")
    ("ssl" . "#ssl {name} {host} {port} - Open an SSL mud connection")
    ("sub" . "#sub {pattern} {replacement} - Substitute matching output text")
    ("substitute" . "#substitute {pattern} {replacement} - Substitute matching output text")
    ("switch" . "#switch {value} {cases} - Multi-way branch")
    ("system" . "#system {command} - Execute a shell command")
    ("tab" . "#tab {word} - Add a tab completion word")
    ("textin" . "#textin {filename} - Send file contents to mud line by line")
    ("ticker" . "#ticker {name} {commands} {interval} - Repeat commands on interval")
    ("trigger" . "#trigger {pattern} {commands} {priority} - Synonym for #action")
    ("ungag" . "#ungag {pattern} - Remove a gag")
    ("unhighlight" . "#unhighlight {pattern} - Remove a highlight")
    ("unvar" . "#unvar {name} - Remove a variable")
    ("unvariable" . "#unvariable {name} - Remove a variable")
    ("var" . "#var {name} {value} - Define a variable")
    ("variable" . "#variable {name} {value} - Define a variable")
    ("while" . "#while {condition} {commands} - Loop while condition is true")
    ("write" . "#write {filename} - Save session to file")
    ("zap" . "#zap - Close the current session"))
  "Documentation strings for TinTin++ commands, shown by eldoc.")

(defun tintin-eldoc-function ()
  "Return eldoc documentation for TinTin++ command at point."
  (save-excursion
    (let ((case-fold-search nil))
      ;; If we're inside or right after a #command, find it.
      ;; First, skip back over any alpha chars to find a potential #
      (skip-chars-backward "a-zA-Z")
      (when (eq (char-before) ?#)
        (backward-char))
      ;; Now check if we're at a #command
      (when (or (looking-at "#\\([a-zA-Z]+\\)")
                (re-search-backward "#\\([a-zA-Z]+\\)" (line-beginning-position) t))
        (let* ((cmd (downcase (match-string-no-properties 1)))
               (doc (cdr (assoc cmd tintin-command-docs))))
          doc)))))

(defun tintin-which-function ()
  "Return the name of the function at point."
  (save-excursion
    (when (re-search-backward
           (rx "#" (or "function" "FUNCTION") (one-or-more space)
               (or (seq "{" (group (*? (not "}"))) "}")
                   (group (one-or-more (not space)))))
           nil t)
      (or (match-string 1) (match-string 2)))))

;;;###autoload
(define-derived-mode tintin-mode prog-mode "TinTin++"
  "Major mode for editing TinTin++ configuration files.

\\{tintin-mode-map}"
  :syntax-table tintin-mode-syntax-table
  :group 'tintin

  ;; Comments
  (setq-local comment-start "#nop ")
  (setq-local comment-end "")
  (setq-local comment-start-skip "#nop[ \t]*")

  ;; Font lock
  (setq-local font-lock-defaults '(tintin-font-lock-keywords nil nil))

  ;; Indentation
  (setq-local indent-line-function #'tintin-indent-line)
  (setq-local indent-tabs-mode nil)
  (setq-local tab-width tintin-indent-offset)

  ;; Imenu
  (setq-local imenu-create-index-function #'tintin-imenu-create-index)

  ;; Completion
  (add-hook 'completion-at-point-functions #'tintin-completion-at-point nil t)

  ;; Outline mode support
  (setq-local outline-regexp "#\\(function\\|session\\|if\\|loop\\|while\\|foreach\\)")
  (setq-local outline-level #'tintin-outline-level)

  ;; Which-function-mode support
  (setq-local which-func-functions '(tintin-which-function))

  ;; Eldoc support
  (setq-local eldoc-documentation-function #'tintin-eldoc-function)

  ;; Electric pairs for common TinTin++ constructs
  (when (fboundp 'electric-pair-local-mode)
    (setq-local electric-pair-pairs '((?{ . ?}) (?\( . ?\))))
    (electric-pair-local-mode 1)))

(provide 'tintin-mode)

;;; tintin-mode.el ends here
