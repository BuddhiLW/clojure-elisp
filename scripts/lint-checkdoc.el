;;; lint-checkdoc.el --- Run checkdoc over files named on the command line -*- lexical-binding: t; -*-

;;; Commentary:

;; emacs -Q --batch -l scripts/lint-checkdoc.el FILE...
;; Prints each diagnostic as FILE:LINE: TEXT and exits 1 when there were
;; any.  `sentence-end-double-space' is nil, as in melpazoid.

;;; Code:

(require 'checkdoc)

(defvar lint-checkdoc--count 0
  "Number of diagnostics reported so far.")

(defun lint-checkdoc--report (text start _end &optional _unfixable)
  "Print TEXT for the position START in the current buffer and count it."
  (setq lint-checkdoc--count (1+ lint-checkdoc--count))
  (message "%s:%d: %s" (file-name-nondirectory (or (buffer-file-name) "?"))
           (line-number-at-pos start) text)
  nil)

(setq sentence-end-double-space nil)
(setq checkdoc-create-error-function #'lint-checkdoc--report)
(setq checkdoc-autofix-flag 'never)

;; Load every file first, as melpazoid does by byte-compiling before it runs
;; checkdoc: a symbol that is both a function and a variable is only reported
;; ambiguous once its library is loaded.
(dolist (file command-line-args-left)
  (load (expand-file-name file) nil t))

(dolist (file command-line-args-left)
  (with-current-buffer (find-file-noselect file)
    (checkdoc-current-buffer t)))

(setq command-line-args-left nil)
(kill-emacs (if (> lint-checkdoc--count 0) 1 0))

;;; lint-checkdoc.el ends here
