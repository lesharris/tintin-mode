;;; tintin-mode.el --- Major mode for editing TinTin++ config files -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Les Harris

;; Author: Les Harris <les@lesharris.com>
;; Version: 0.2
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
  (let ((map (make-keymap)))
    (define-key map "\C-j" 'newline-and-indent)
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
    "foreach" "format" "function"
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

(defconst tintin-font-lock-keywords
  `(
    ;; Comments - #nop, #no, etc. (keep this first)
    (,(rx line-start (zero-or-more space)
          "#" (or "nop" "NOP" "no" "No" "NO")
          (zero-or-more nonl))
     . 'tintin-comment-face)

    ;; Strings (single line only to prevent runaway matches)
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
    (modify-syntax-entry ?_ "w" st)      ; underscore is part of words
    (modify-syntax-entry ?\{ "(}" st)    ; brace pairs
    (modify-syntax-entry ?\} "){" st)
    st)
  "Syntax table for TinTin++ mode.")

(defun tintin-indent-line ()
  "Indent current line as TinTin++ code."
  (interactive)
  (let ((indent-col 0)
        (cur-indent (current-indentation)))
    (save-excursion
      (beginning-of-line)
      (when (not (bobp))
        (forward-line -1)
        (setq indent-col (current-indentation))
        ;; Increase indent after opening braces or conditionals
        (when (looking-at ".*{[^}]*$\\|.*#\\(if\\|else\\|loop\\|while\\|foreach\\)")
          (setq indent-col (+ indent-col tintin-indent-offset)))
        ;; Handle closing braces
        (forward-line 1)
        (when (looking-at "^[ \t]*}")
          (setq indent-col (max 0 (- indent-col tintin-indent-offset))))))

    (unless (= cur-indent indent-col)
      (beginning-of-line)
      (delete-horizontal-space)
      (indent-to indent-col))))

(defun tintin-imenu-create-index ()
  "Create an imenu index for TinTin++ functions and variables."
  (let ((functions '())
        (variables '()))
    (save-excursion
      (goto-char (point-min))
      ;; Find function definitions
      (while (re-search-forward
              (rx "#" (or "function" "FUNCTION") (one-or-more space)
                  (or (seq "{" (group (*? (not "}"))) "}")
                      (group (one-or-more (not space)))))
              nil t)
        (let ((name (or (match-string 1) (match-string 2))))
          (when name
            (push (cons name (match-beginning 0)) functions))))

      (goto-char (point-min))
      ;; Find variable definitions
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
  "Provide completion for TinTin++ commands."
  (when (looking-back "#\\([a-zA-Z]*\\)" (line-beginning-position))
    (let ((start (match-beginning 1))
          (end (match-end 1)))
      (list start end tintin-commands))))

(defun tintin-outline-level ()
  "Return the outline level for the current line."
  (save-excursion
    (beginning-of-line)
    (cond
     ((looking-at "#\\(function\\|session\\)") 1)
     ((looking-at "#\\(if\\|loop\\|while\\|foreach\\)") 2)
     (t 3))))

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

  ;; Electric pairs for common TinTin++ constructs
  (when (fboundp 'electric-pair-local-mode)
    (setq-local electric-pair-pairs '((?{ . ?}) (?\( . ?\))))
    (electric-pair-local-mode 1)))

(provide 'tintin-mode)

;;; tintin-mode.el ends here
