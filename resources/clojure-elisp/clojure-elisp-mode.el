;;; clojure-elisp-mode.el --- Major mode for editing ClojureElisp files -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Pedro G. Branquinho
;; Author: Pedro G. Branquinho <pedrogbranquinho@gmail.com>
;; Maintainer: Pedro G. Branquinho <pedrogbranquinho@gmail.com>
;; URL: https://github.com/BuddhiLW/clojure-elisp
;; Version: 0.3.1
;; Package-Requires: ((emacs "28.1") (clojure-mode "5.0"))
;; Keywords: languages, lisp, clojure
;; SPDX-License-Identifier: MIT

;;; Commentary:
;;
;; Major mode for editing ClojureElisp (.cljel) files.
;; Derives from clojure-mode to inherit syntax highlighting,
;; indentation, and paredit/smartparens support.

;;; Code:

(require 'clojure-mode)

(defgroup clojure-elisp nil
  "ClojureElisp editing support."
  :group 'clojure
  :prefix "clojure-elisp-")

(defcustom clojure-elisp-compile-command "clojure -M -m clojure-elisp.core"
  "Command used to invoke the ClojureElisp compiler."
  :type 'string
  :group 'clojure-elisp)

(defvar clojure-elisp-font-lock-keywords
  `((,(regexp-opt '("elisp/" "clel-") 'symbols) . font-lock-builtin-face))
  "Additional font-lock keywords for ClojureElisp mode.")

;;;###autoload
(define-derived-mode clojure-elisp-mode clojure-mode "ClojureElisp"
  "Major mode for editing ClojureElisp (.cljel) files.
Derives from `clojure-mode' for syntax highlighting, indentation,
and structural editing support."
  (setq-local compile-command
              (concat clojure-elisp-compile-command " "
                      (when buffer-file-name
                        (shell-quote-argument buffer-file-name))))
  (font-lock-add-keywords nil clojure-elisp-font-lock-keywords))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.cljel\\'" . clojure-elisp-mode))

(provide 'clojure-elisp-mode)

;;; clojure-elisp-mode.el ends here
