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

;;; Keywords, maps and sets as function arguments

(ert-deftest clel-semantics-keyword-as-hof-fn ()
  "Higher-order fns accept a keyword where Clojure accepts one."
  (should (equal '(:night :golden :civil)
                 (clel-semantics-test--realize (semantics-kw-map))))
  (should (equal '(:night :civil :golden)
                 (clel-semantics-test--realize (semantics-kw-sort-by))))
  (should (equal '(:golden :civil :night)
                 (clel-semantics-test--realize (semantics-kw-sort-by-desc))))
  (should (equal '(1 3) (mapcar (lambda (m) (clel-get m :n))
                                (clel-semantics-test--realize (semantics-kw-filter)))))
  (let ((groups (semantics-kw-group-by)))
    (should (equal '(1 3) (mapcar (lambda (m) (clel-get m :n)) (clel-get groups :a))))
    (should (equal '(2) (mapcar (lambda (m) (clel-get m :n)) (clel-get groups :b))))))

(ert-deftest clel-semantics-collections-as-hof-fn ()
  "A set literal is a membership predicate and a map is a lookup."
  (should (equal '(:a :c) (clel-semantics-test--realize (semantics-set-as-pred))))
  (should (equal '(1 2 nil) (clel-semantics-test--realize (semantics-map-as-mapper)))))

;;; Comparators and sorting

(ert-deftest clel-semantics-sort-arities ()
  "(sort coll) uses compare; (sort cmp coll) takes a predicate or a
three-way comparator."
  (should (equal '(1 2 3) (semantics-sort-default)))
  (should (equal '(1 2 3) (semantics-sort-lt)))
  (should (equal '(3 2 1) (semantics-sort-gt)))
  (should (equal '("a" "b" "c") (semantics-sort-compare)))
  (should (equal '(3 2 1) (semantics-sort-reverse-compare)))
  (should (equal '(:a :b :c) (semantics-sort-keywords))))

(ert-deftest clel-semantics-compare ()
  (should (equal '(-1 1 0 -1) (semantics-compare-values))))

(ert-deftest clel-semantics-sort-does-not-mutate ()
  "Sorting returns a new list and leaves its argument alone."
  (let ((xs (list 3 1 2)))
    (should (equal '(1 2 3) (clel-sort xs)))
    (should (equal '(3 1 2) xs))))

;;; Extra function arguments

(ert-deftest clel-semantics-swap-update-extra-args ()
  "swap! threads update and its function argument through the atom."
  (should (equal 12 (clel-get (semantics-swap-update) :count)))
  (should (equal 11 (clel-get (semantics-update-extra-args) :a))))

;;; Destructuring in iteration bindings

(ert-deftest clel-semantics-doseq-destructures ()
  "doseq destructures each element instead of binding the coll to nil."
  (should (equal '(3 7) (semantics-doseq-pairs)))
  (should (equal '((:a 1) (:b 2)) (semantics-doseq-map-entries))))

(ert-deftest clel-semantics-for-destructures ()
  (should (equal '(3 13) (clel-semantics-test--realize (semantics-for-pairs))))
  (should (equal '(:y) (clel-semantics-test--realize (semantics-for-map-entries)))))

(ert-deftest clel-semantics-loop-destructures ()
  "recur rebinds the whole pattern."
  (should (equal 10 (semantics-loop-rest '(1 2 3 4))))
  (should (equal 0 (semantics-loop-rest nil)))
  (should (equal 5000050000
                 (semantics-loop-rest (number-sequence 1 100000)))))

(ert-deftest clel-semantics-fn-destructures-map-entries ()
  (should (equal '((1 :a) (2 :b))
                 (clel-semantics-test--realize (semantics-map-fn-entries)))))

;;; Destructuring shorter collections

(ert-deftest clel-semantics-short-collections-bind-nil ()
  "Names past the end of the collection bind nil, as in Clojure."
  (should (equal '(1 2 nil) (semantics-short-vector)))
  (should (equal '(1 nil) (semantics-short-rest))))

(ert-deftest clel-semantics-rest-of-lazy-seq ()
  "& rest walks a lazy sequence instead of the struct holding it."
  (should (equal '(2 (3 4)) (clel-semantics-test--realize (semantics-rest-of-lazy)))))

(ert-deftest clel-semantics-nested-map-destructuring ()
  (should (equal '(1 2 3)
                 (semantics-nested-map-destructure
                  (list (cons :pt (list 1 2))
                        (cons :inner (list (cons :z 3))))))))

(provide 'clojure-elisp-semantics-test)
;;; clojure-elisp-semantics-test.el ends here
