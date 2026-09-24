;;; clojure-elisp-semantics-test.el --- Compiled cljel behaves like Clojure -*- lexical-binding: t; -*-

;;; Commentary:

;; The Clojure suite asserts on emitted STRINGS, which cannot tell whether the
;; string does what the Clojure meant once Emacs runs it.  The fixture is
;; test/elisp/sources/semantics.cljel, compiled by the Makefile; these tests
;; load it next to the runtime and call it.
;;
;; Run:
;;   make test-elisp

;;; Code:

(require 'ert)

(defconst clel-semantics-test--dir
  (file-name-directory (or load-file-name buffer-file-name)))

(add-to-list 'load-path
             (expand-file-name "../../resources/clojure-elisp" clel-semantics-test--dir))
(load (expand-file-name "fixtures/semantics.el" clel-semantics-test--dir) nil t)

(defun clel-semantics-test--realize (x)
  "Return X with every lazy sequence inside it forced."
  (cond ((clel-lazy-seq-p x) (clel-semantics-test--realize (clel-doall x)))
        ((consp x) (cons (clel-semantics-test--realize (car x))
                         (clel-semantics-test--realize (cdr x))))
        (t x)))

;;; Function values

(ert-deftest clel-semantics-local-fn-is-funcalled ()
  "A function held in a parameter or a let is called through `funcall'."
  (should (equal 2 (semantics-call-param #'1+ 1)))
  (should (equal 2 (semantics-call-let-bound))))

(ert-deftest clel-semantics-def-value-is-funcalled ()
  "A def holding a function is a variable, so its call site funcalls it."
  (should (equal 42 (semantics-call-def-value))))

(ert-deftest clel-semantics-computed-callee ()
  "A computed or #'-quoted callee is funcalled, never spliced into
function position."
  (should (equal 3 (semantics-call-computed)))
  (should (equal 2 (semantics-call-var-quote))))

(ert-deftest clel-semantics-core-fn-as-value ()
  "A core function passed as a value arrives as a function, not a void
variable."
  (should (equal 42 (semantics-pass-core-fn))))

(ert-deftest clel-semantics-letfn-bindings ()
  "letfn names live in the function namespace; a parameter of the same name
shadows them."
  (should (equal '(2 4) (clel-semantics-test--realize (semantics-letfn-value))))
  (should (equal 6 (semantics-letfn-param-shadows))))

;;; Keywords and maps in function position

(ert-deftest clel-semantics-keyword-call ()
  "(:k m) and (:k m default) read the map."
  (should (equal 1 (semantics-kw-get (list (cons :k 1)))))
  (should (equal nil (semantics-kw-get nil)))
  (should (equal :none (semantics-kw-default (list (cons :j 1)))))
  (should (equal 1 (semantics-map-as-fn))))

(provide 'clojure-elisp-semantics-test)
;;; clojure-elisp-semantics-test.el ends here
