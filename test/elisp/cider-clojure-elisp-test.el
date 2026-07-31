;;; cider-clojure-elisp-test.el --- ERT tests for the CLJEL client eval path -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Regression cover for the client half of a CLJEL load-file: the middleware
;; now compiles a whole buffer, so the payload is a multi-form Elisp program.
;; `cider-cljel--eval-elisp-string' must install EVERY top-level form, while
;; a single-form payload keeps its previous expression semantics.
;;
;; Run:
;;   emacs -Q -batch -l ert \
;;         -l test/elisp/cider-clojure-elisp-test.el \
;;         -f ert-run-tests-batch-and-exit
;;
;; `cider' and `nrepl-client' are stubbed: the eval path under test is pure
;; Emacs and must not drag a package install into the suite.

;;; Code:

(require 'ert)

(provide 'cider)
(provide 'nrepl-client)

(load (expand-file-name
       "../../resources/clojure-elisp/cider-clojure-elisp.el"
       (file-name-directory (or load-file-name buffer-file-name)))
      nil t)

(defvar cider-cljel-test--probe nil
  "Scratch variable written by evaluated test payloads.")

;;; --- Reading every top-level form ---

(ert-deftest cider-cljel-test-read-forms-single ()
  "A single form reads as a one-element list."
  (should (equal '((+ 1 2)) (cider-cljel--read-forms "(+ 1 2)"))))

(ert-deftest cider-cljel-test-read-forms-multiple ()
  "Every top-level form is read, in source order."
  (should (equal '((a) (b) (c))
                 (cider-cljel--read-forms "(a)\n(b)\n  (c)\n"))))

(ert-deftest cider-cljel-test-read-forms-trailing-whitespace ()
  "Trailing whitespace terminates the scan without signalling."
  (should (equal '((a)) (cider-cljel--read-forms "(a)   \n\n  "))))

(ert-deftest cider-cljel-test-read-forms-empty ()
  "An empty payload yields no forms."
  (should (equal nil (cider-cljel--read-forms "")))
  (should (equal nil (cider-cljel--read-forms "   \n "))))

;;; --- Single-form eval is unchanged ---

(ert-deftest cider-cljel-test-eval-single-form-value ()
  "A single-expression payload still returns that expression's value."
  (should (equal "3" (cider-cljel--eval-elisp-string "(+ 1 2)"))))

(ert-deftest cider-cljel-test-eval-single-form-side-effect ()
  "A single-form payload still performs its side effect."
  (setq cider-cljel-test--probe nil)
  (should (equal "42" (cider-cljel--eval-elisp-string
                       "(setq cider-cljel-test--probe 42)")))
  (should (equal 42 cider-cljel-test--probe)))

(ert-deftest cider-cljel-test-eval-error-is-reported-as-string ()
  "An erroring payload returns a description instead of signalling."
  (should (string-match-p "Elisp eval error"
                          (cider-cljel--eval-elisp-string
                           "(cider-cljel-test--no-such-function)"))))

;;; --- The regression: multi-form payloads install every form ---

(ert-deftest cider-cljel-test-eval-multi-form-runs-all ()
  "Every top-level form of a multi-form payload is evaluated."
  (setq cider-cljel-test--probe nil)
  (should (equal "3" (cider-cljel--eval-elisp-string
                      "(setq cider-cljel-test--probe 1)
                       (setq cider-cljel-test--probe
                             (1+ cider-cljel-test--probe))
                       (setq cider-cljel-test--probe 3)")))
  (should (equal 3 cider-cljel-test--probe)))

(ert-deftest cider-cljel-test-eval-namespaced-defuns-are-installed ()
  "Namespace-prefixed defuns from a whole-buffer compile become callable."
  (fmakunbound 'cider-cljel-test-mod-bump)
  (fmakunbound 'cider-cljel-test-mod--helper)
  (cider-cljel--eval-elisp-string
   "(defun cider-cljel-test-mod--helper (x) (* x 2))
    (defun cider-cljel-test-mod-bump (n)
      (cider-cljel-test-mod--helper n))")
  (should (fboundp 'cider-cljel-test-mod--helper))
  (should (fboundp 'cider-cljel-test-mod-bump))
  (should (equal 14 (cider-cljel-test-mod-bump 7))))

(ert-deftest cider-cljel-test-eval-provide-form-is-installed ()
  "A trailing (provide ...) — the last form of a compiled file — takes effect."
  (setq features (delq 'cider-cljel-test-provided features))
  (should-not (featurep 'cider-cljel-test-provided))
  (cider-cljel--eval-elisp-string
   "(defvar cider-cljel-test-provided-var 1)
    (provide 'cider-cljel-test-provided)")
  (should (featurep 'cider-cljel-test-provided)))

(ert-deftest cider-cljel-test-eval-keymap-definition-is-installed ()
  "A keymap defvar plus its bindings survive a multi-form payload."
  (makunbound 'cider-cljel-test-mode-map)
  (cider-cljel--eval-elisp-string
   "(defvar cider-cljel-test-mode-map
      (let* ((m (make-sparse-keymap)))
        (define-key m (kbd \"C-c C-b\") 'cider-cljel-test-mod-bump)
        m))
    (provide 'cider-cljel-test-keymap)")
  (should (keymapp cider-cljel-test-mode-map))
  (should (eq 'cider-cljel-test-mod-bump
              (lookup-key cider-cljel-test-mode-map (kbd "C-c C-b"))))
  (should (featurep 'cider-cljel-test-keymap)))

(ert-deftest cider-cljel-test-eval-full-buffer-payload ()
  "A full compiled-file payload — header comments, ns forms, defuns, provide —
loads end to end and reports the last form's value."
  (fmakunbound 'cider-cljel-test-buf-greet)
  (makunbound 'cider-cljel-test-buf-counter)
  (setq features (delq 'cider-cljel-test-buf features))
  (let ((payload "\
;;; cider-cljel-test-buf.el --- -*- lexical-binding: t; -*-
;; Generated by ClojureElisp

;;; Code:

(defvar cider-cljel-test-buf-counter 0
  \"A counter.\")

(defun cider-cljel-test-buf--double (x)
  \"Private helper.\"
  (* x 2))

(defun cider-cljel-test-buf-greet (n)
  \"Public entry point.\"
  (setq cider-cljel-test-buf-counter
        (cider-cljel-test-buf--double n)))

(defvar cider-cljel-test-buf-map
  (let* ((m (make-sparse-keymap)))
    (define-key m (kbd \"C-c C-g\") 'cider-cljel-test-buf-greet)
    m))

(provide 'cider-cljel-test-buf)
;;; cider-cljel-test-buf.el ends here
"))
    (should (equal "cider-cljel-test-buf"
                   (cider-cljel--eval-elisp-string payload))))
  (should (featurep 'cider-cljel-test-buf))
  (should (fboundp 'cider-cljel-test-buf--double))
  (should (equal 0 cider-cljel-test-buf-counter))
  (should (equal 10 (cider-cljel-test-buf-greet 5)))
  (should (equal 10 cider-cljel-test-buf-counter))
  (should (keymapp cider-cljel-test-buf-map)))

(provide 'cider-clojure-elisp-test)
;;; cider-clojure-elisp-test.el ends here
