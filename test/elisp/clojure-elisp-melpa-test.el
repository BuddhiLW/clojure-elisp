;;; clojure-elisp-melpa-test.el --- A compiled package passes MELPA's checks -*- lexical-binding: t; -*-

;;; Commentary:

;; The fixture is a three-file package, test/elisp/sources/melpa, which the
;; Makefile compiles with `compile-project-from-config' from its clel.edn.
;; These tests hold it to what MELPA runs: byte-compile with warnings as
;; errors, checkdoc as melpazoid configures it, and the headers package.el
;; and lisp-mnt parse.  They also check what the Clojure test suite cannot
;; see from strings: that help and eldoc read the real signatures, that
;; loaddefs finds the autoload cookies, and that the code runs.

;;; Code:

(require 'ert)
(require 'checkdoc)
(require 'lisp-mnt)
(require 'package)
(require 'help)

(defconst clel-melpa-test--dir
  (file-name-directory (or load-file-name buffer-file-name)))

(defconst clel-melpa-test--fixtures
  (expand-file-name "fixtures/melpa" clel-melpa-test--dir))

(defconst clel-melpa-test--runtime
  (expand-file-name "../../resources/clojure-elisp" clel-melpa-test--dir))

(defconst clel-melpa-test--files '("clelfix.el" "clelfix-core.el" "clelfix-util.el"))

(defun clel-melpa-test--file (name)
  "Absolute path of fixture file NAME."
  (expand-file-name name clel-melpa-test--fixtures))

(defmacro clel-melpa-test--visiting (name &rest body)
  "Run BODY in a buffer holding fixture file NAME."
  (declare (indent 1))
  `(with-temp-buffer
     (insert-file-contents (clel-melpa-test--file ,name))
     (setq buffer-file-name (clel-melpa-test--file ,name))
     (emacs-lisp-mode)
     (unwind-protect (progn ,@body)
       (set-buffer-modified-p nil)
       (setq buffer-file-name nil))))

(defun clel-melpa-test--emacs (&rest args)
  "Run a batch Emacs with the runtime and fixtures on the load path.
ARGS follow.  Return (EXIT . OUTPUT)."
  (with-temp-buffer
    (let ((exit (apply #'call-process
                       (expand-file-name invocation-name invocation-directory)
                       nil t nil "-Q" "--batch"
                       "-L" clel-melpa-test--runtime
                       "-L" clel-melpa-test--fixtures
                       args)))
      (cons exit (buffer-string)))))

(ert-deftest clel-melpa-byte-compiles-with-warnings-as-errors ()
  "MELPA's checklist asks for a clean byte-compile."
  (let ((out-dir (make-temp-file "clel-melpa-elc" t)))
    (unwind-protect
        (dolist (f clel-melpa-test--files)
          (pcase-let ((`(,exit . ,out)
                       (clel-melpa-test--emacs
                        "--eval" (format "(setq byte-compile-error-on-warn t byte-compile-dest-file-function (lambda (f) (expand-file-name (concat (file-name-nondirectory f) \"c\") %S)))" out-dir)
                        "-f" "batch-byte-compile" (clel-melpa-test--file f))))
            (should (equal (list f 0 "") (list f exit (string-trim out))))))
      (delete-directory out-dir t))))

(ert-deftest clel-melpa-checkdoc-is-clean ()
  "Checkdoc as melpazoid runs it: `sentence-end-double-space' nil."
  (dolist (f clel-melpa-test--files)
    (let ((checkdoc-diagnostic-buffer "*clel-melpa-checkdoc*")
          (sentence-end-double-space nil))
      (with-current-buffer (get-buffer-create checkdoc-diagnostic-buffer)
        (let ((inhibit-read-only t)) (erase-buffer)))
      (clel-melpa-test--visiting f
        (checkdoc-current-buffer t))
      (let ((diagnostics
             (with-current-buffer checkdoc-diagnostic-buffer
               (seq-filter (lambda (l) (string-match-p ":[0-9]+: " l))
                           (split-string (buffer-string) "\n" t)))))
        (should (equal (list f nil) (list f diagnostics)))))))

(ert-deftest clel-melpa-main-file-has-the-package-header ()
  (clel-melpa-test--visiting "clelfix.el"
    (let ((desc (package-buffer-info)))
      (should (eq (package-desc-name desc) 'clelfix))
      (should (equal (package-desc-version desc) '(0 1 0)))
      (should (equal (package-desc-summary desc) "Exercise the MELPA surface of compiled output"))
      (should (equal (alist-get 'emacs (package-desc-reqs desc)) '((28 1))))
      (should (assq 'clel (package-desc-reqs desc))))))

(ert-deftest clel-melpa-secondary-files-have-a-secondary-header ()
  "Summary, license and Commentary, and no Package-Requires, which
package-lint reports as an error outside the main file."
  (dolist (f '("clelfix-core.el" "clelfix-util.el"))
    (clel-melpa-test--visiting f
      (should (> (length (lm-summary)) 0))
      (should (equal (lm-header "SPDX-License-Identifier") "GPL-3.0-or-later"))
      (should (equal (car (lm-authors)) '("Jane Doe" . "jane@example.org")))
      (should-not (lm-header "Package-Requires"))
      (should-not (lm-header "Version"))
      (should (> (length (string-trim (lm-commentary))) (length ";;; Commentary:")))
      (goto-char (point-min))
      (should (< (lm-code-start) (search-forward "(eval-and-compile"))))))

(ert-deftest clel-melpa-autoload-cookies-reach-loaddefs ()
  "Package.el builds autoloads from the cookies: the global mode and the
command must be autoloaded before their file is loaded."
  ;; A fresh file: `loaddefs-generate' leaves an output newer than its
  ;; sources alone.
  (let* ((dir (make-temp-file "clelfix-autoloads" t))
         (loaddefs (expand-file-name "clelfix-autoloads.el" dir)))
    (unwind-protect
        (progn
          (loaddefs-generate clel-melpa-test--fixtures loaddefs)
          (pcase-let ((`(,exit . ,out)
                       (clel-melpa-test--emacs
                        "-l" loaddefs
                        "--eval" "(princ (list (autoloadp (symbol-function 'clelfix-mode)) (autoloadp (symbol-function 'clelfix-hello)) (commandp 'clelfix-hello) (featurep 'clelfix)))")))
            (should (equal 0 exit))
            (should (string-suffix-p "(t t t nil)" (string-trim out)))))
      (delete-directory dir t))))

(ert-deftest clel-melpa-help-sees-the-real-signatures ()
  (add-to-list 'load-path clel-melpa-test--runtime)
  (add-to-list 'load-path clel-melpa-test--fixtures)
  (require 'clelfix)
  (ert-info ("a variadic defun has its real arglist and a docstring")
    (should (equal (help-function-arglist 'clelfix-core-first-even t) '(factor &rest nums)))
    (should (string-prefix-p "Return the first even" (documentation 'clelfix-core-first-even t))))
  (ert-info ("a multi-arity defun documents its signature for help and eldoc")
    (should (equal (car (help-split-fundoc (documentation 'clelfix-core-span t) 'clelfix-core-span))
                   "(clelfix-core-span START &optional END)"))))

(ert-deftest clel-melpa-compiled-code-runs ()
  (add-to-list 'load-path clel-melpa-test--runtime)
  (add-to-list 'load-path clel-melpa-test--fixtures)
  (require 'clelfix)
  (should (equal (clelfix-core-span 20) 4))
  (should (equal (clelfix-core-span 3 10) 7))
  (should (equal (clelfix-core-first-even 10 1 3 4 6) 40))
  (should-not (clelfix-core-first-even 10 1 3))
  (should (equal (clelfix-core-or-default nil 5) 5))
  (should (equal (clelfix-core-or-default 2 5) 2))
  (should (equal (clelfix-util-tags '("b" "a") '("c" "a")) "a,b,c"))
  (should (equal (clelfix-util-describe-state '((:phase . :day) (:hour . 12))) ":day at 12"))
  (should (equal (clelfix-summary 20 '("x") '("y")) '(4 "x,y")))
  (let ((before clelfix--toggles))
    (clelfix-mode 1)
    (clelfix-mode -1)
    (should (equal clelfix--toggles (+ before 2)))))

(provide 'clojure-elisp-melpa-test)
;;; clojure-elisp-melpa-test.el ends here
