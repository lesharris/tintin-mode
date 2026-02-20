;;; tintin-mode-tests.el --- Tests for tintin-mode -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Les Harris

;;; Commentary:

;; ERT test suite for tintin-mode.  Run with:
;;   emacs -batch -l tintin-mode.el -l tintin-mode-tests.el -f ert-run-tests-batch-and-exit

;;; Code:

(require 'ert)
(require 'tintin-mode)

;;; ---------------------------------------------------------------------------
;;; Helpers
;;; ---------------------------------------------------------------------------

(defmacro tintin-test-with-buffer (content &rest body)
  "Insert CONTENT into a `tintin-mode' buffer and evaluate BODY."
  (declare (indent 1))
  `(with-temp-buffer
     (tintin-mode)
     (insert ,content)
     (font-lock-ensure)
     ,@body))

(defun tintin-test-indent (input expected)
  "Assert that indenting INPUT produces EXPECTED."
  (with-temp-buffer
    (tintin-mode)
    (insert input)
    (indent-region (point-min) (point-max))
    (should (string= (buffer-string) expected))))

(defun tintin-test-face-at (content pos expected-face)
  "Assert that face at POS in CONTENT matches EXPECTED-FACE."
  (tintin-test-with-buffer content
                           (font-lock-ensure)
                           (let ((face (get-text-property pos 'face)))
                             (should (eq face expected-face)))))

;;; ---------------------------------------------------------------------------
;;; Indentation
;;; ---------------------------------------------------------------------------

(ert-deftest tintin-test-indent-flat ()
  "Top-level lines stay at column 0."
  (tintin-test-indent
   "#var {hp} {100}\n#var {mp} {50}"
   "#var {hp} {100}\n#var {mp} {50}"))

(ert-deftest tintin-test-indent-open-brace ()
  "Line after an opening brace is indented."
  (tintin-test-indent
   "#action {pattern} {\nresponse\n}"
   "#action {pattern} {\n  response\n}"))

(ert-deftest tintin-test-indent-nested ()
  "Nested braces increase indentation."
  (tintin-test-indent
   "#action {pattern} {\n#if {1} {\ninner\n}\n}"
   "#action {pattern} {\n  #if {1} {\n    inner\n  }\n}"))

(ert-deftest tintin-test-indent-closing-brace-aligns ()
  "Closing brace aligns with its opening line."
  (tintin-test-indent
   "#function {test} {\n#if {1} {\nfoo\n}\n}"
   "#function {test} {\n  #if {1} {\n    foo\n  }\n}"))

(ert-deftest tintin-test-indent-bare-text ()
  "Bare MUD commands at top level stay at column 0."
  (tintin-test-indent
   "kill orc\nlook\nnorth"
   "kill orc\nlook\nnorth"))

(ert-deftest tintin-test-indent-inline-braces ()
  "Balanced braces on one line do not change indentation of next line."
  (tintin-test-indent
   "#var {hp} {100}\n#var {mp} {50}"
   "#var {hp} {100}\n#var {mp} {50}"))

(ert-deftest tintin-test-indent-multiple-blocks ()
  "Sequential blocks each indent and dedent correctly."
  (tintin-test-indent
   "#action {foo} {\nbar\n}\n#action {baz} {\nqux\n}"
   "#action {foo} {\n  bar\n}\n#action {baz} {\n  qux\n}"))

(ert-deftest tintin-test-indent-empty-lines-skipped ()
  "Empty lines between code don't break indentation."
  (tintin-test-indent
   "#action {foo} {\n\nbar\n}"
   "#action {foo} {\n\n  bar\n}"))

;;; ---------------------------------------------------------------------------
;;; Font-lock / syntax highlighting
;;; ---------------------------------------------------------------------------

(ert-deftest tintin-test-face-comment ()
  "Comments get the comment face."
  (tintin-test-face-at "#nop this is a comment" 1 'tintin-comment-face))

(ert-deftest tintin-test-face-NOP-comment ()
  "Case variant #NOP is recognized as comment."
  (tintin-test-face-at "#NOP also a comment" 1 'tintin-comment-face))

(ert-deftest tintin-test-face-var-def ()
  "Variable definitions are highlighted."
  (tintin-test-with-buffer "#var {hp} {100}"
                           (let ((face (get-text-property 6 'face)))
                             (should (eq face 'tintin-var-def-face)))))

(ert-deftest tintin-test-face-function-def ()
  "Function definitions are highlighted."
  (tintin-test-with-buffer "#function {heal} {#return 1}"
                           (let ((face (get-text-property 11 'face)))
                             (should (eq face 'tintin-function-def-face)))))

(ert-deftest tintin-test-face-conditional ()
  "Conditionals get the conditional face."
  (tintin-test-with-buffer "#if {1} {foo}"
                           ;; The "if" part (after #)
                           (let ((face (get-text-property 2 'face)))
                             (should (eq face 'tintin-conditional-face)))))

(ert-deftest tintin-test-face-variable-ref ()
  "Variable references ($var) are highlighted."
  (tintin-test-with-buffer "#showme {$hp}"
                           (let ((face (get-text-property 11 'face)))
                             (should (eq face 'tintin-var-face)))))

(ert-deftest tintin-test-face-function-call ()
  "Function calls (@func) are highlighted."
  (tintin-test-with-buffer "#showme {@heal{}}"
                           (let ((face (get-text-property 11 'face)))
                             (should (eq face 'tintin-function-face)))))

(ert-deftest tintin-test-face-hash-command ()
  "General hash commands get the hash face."
  (tintin-test-with-buffer "#session {name} {host} {port}"
                           (let ((face (get-text-property 2 'face)))
                             (should (eq face 'tintin-hash-face)))))

;;; ---------------------------------------------------------------------------
;;; Imenu
;;; ---------------------------------------------------------------------------

(ert-deftest tintin-test-imenu-functions ()
  "Imenu finds function definitions."
  (tintin-test-with-buffer "#function {heal} {\n#return 1\n}\n#function {nuke} {\n#return 2\n}"
                           (let* ((index (tintin-imenu-create-index))
                                  (funcs (cdr (assoc "Functions" index)))
                                  (names (mapcar #'car funcs)))
                             (should (member "heal" names))
                             (should (member "nuke" names)))))

(ert-deftest tintin-test-imenu-variables ()
  "Imenu finds variable definitions."
  (tintin-test-with-buffer "#var {hp} {100}\n#variable {mp} {50}"
                           (let* ((index (tintin-imenu-create-index))
                                  (vars (cdr (assoc "Variables" index)))
                                  (names (mapcar #'car vars)))
                             (should (member "hp" names))
                             (should (member "mp" names)))))

(ert-deftest tintin-test-imenu-empty-buffer ()
  "Imenu returns nil for an empty buffer."
  (tintin-test-with-buffer ""
                           (let ((index (tintin-imenu-create-index)))
                             (should (null index)))))

;;; ---------------------------------------------------------------------------
;;; Completion
;;; ---------------------------------------------------------------------------

(ert-deftest tintin-test-collect-variables ()
  "Variable collection finds all #var and #variable definitions."
  (tintin-test-with-buffer "#var {hp} {100}\n#variable {mp} {50}\n#local {tmp} {0}"
                           (let ((vars (tintin-collect-variables)))
                             (should (member "hp" vars))
                             (should (member "mp" vars))
                             (should (member "tmp" vars)))))

(ert-deftest tintin-test-collect-functions ()
  "Function collection finds all #function definitions."
  (tintin-test-with-buffer "#function {heal} {#return}\n#function {nuke} {#return}"
                           (let ((funcs (tintin-collect-functions)))
                             (should (member "heal" funcs))
                             (should (member "nuke" funcs)))))

(ert-deftest tintin-test-annotate-command ()
  "Command annotations return expected values."
  (should (string= (tintin-annotate-command "action") " trigger"))
  (should (string= (tintin-annotate-command "function") " fn"))
  (should (string= (tintin-annotate-command "func") " fn"))
  (should (string= (tintin-annotate-command "zap") "")))

;;; ---------------------------------------------------------------------------
;;; Eldoc
;;; ---------------------------------------------------------------------------

(ert-deftest tintin-test-eldoc-known-command ()
  "Eldoc returns documentation for known commands."
  (tintin-test-with-buffer "#action"
                           (goto-char 2)
                           (let ((doc (tintin-eldoc-function)))
                             (should doc)
                             (should (string-match-p "action" doc)))))

(ert-deftest tintin-test-eldoc-unknown ()
  "Eldoc returns nil when not on a command."
  (tintin-test-with-buffer "kill orc"
                           (goto-char 1)
                           (let ((doc (tintin-eldoc-function)))
                             (should (null doc)))))

;;; ---------------------------------------------------------------------------
;;; Which-function
;;; ---------------------------------------------------------------------------

(ert-deftest tintin-test-which-function ()
  "which-function returns the enclosing function name."
  (tintin-test-with-buffer "#function {heal} {\n  cast heal\n}"
                           (goto-char 25) ;; inside the function body
                           (should (string= (tintin-which-function) "heal"))))

(ert-deftest tintin-test-which-function-outside ()
  "which-function returns nil outside any function."
  (tintin-test-with-buffer "#var {hp} {100}"
                           (goto-char 1)
                           (should (null (tintin-which-function)))))

;;; ---------------------------------------------------------------------------
;;; Mode setup
;;; ---------------------------------------------------------------------------

(ert-deftest tintin-test-mode-activates ()
  "tintin-mode activates without error."
  (with-temp-buffer
    (tintin-mode)
    (should (eq major-mode 'tintin-mode))))

(ert-deftest tintin-test-comment-vars ()
  "Comment variables are set correctly."
  (with-temp-buffer
    (tintin-mode)
    (should (string= comment-start "#nop "))
    (should (string= comment-end ""))))

(provide 'tintin-mode-tests)

;;; tintin-mode-tests.el ends here
