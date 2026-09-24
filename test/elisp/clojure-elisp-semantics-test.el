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

;;; Definitions: defonce, private names, assignment

(ert-deftest clel-semantics-defonce ()
  "defonce is a documented defvar: reloading does not reset it."
  (should (equal "Registered things."
                 (documentation-property 'semantics-registry 'variable-documentation)))
  (should (equal "Loaded once."
                 (documentation-property 'semantics-loads 'variable-documentation)))
  (let ((before semantics-registry))
    (setq semantics-loads 5)
    (load (expand-file-name "fixtures/semantics.el" clel-semantics-test--dir) nil t)
    (should (eq before semantics-registry))
    (should (equal 5 semantics-loads))
    (setq semantics-loads 0)))

(ert-deftest clel-semantics-private-names-agree ()
  "^:private and defn- name the defun and every call site ns--name."
  (should (fboundp 'semantics--meta-private))
  (should (fboundp 'semantics--dash-private))
  (should-not (fboundp 'semantics-meta-private))
  (should (boundp 'semantics--secret))
  (should (equal '(43 4 45 (10)) (clel-semantics-test--realize (semantics-call-privates)))))

(ert-deftest clel-semantics-setq-reaches-the-def ()
  "setq and set! of a def assign the namespaced variable."
  (setq semantics-counter 0)
  (should (equal 11 (semantics-bump-counter)))
  (should (equal 11 semantics-counter))
  (should-not (boundp 'counter)))

;;; Maps

(ert-deftest clel-semantics-map-equality ()
  "= compares maps by entries, in any order; sequences stay ordered."
  (should (equal '(t t t nil nil nil t nil t) (semantics-map-equality))))

(ert-deftest clel-semantics-contains ()
  "contains? on a map checks keys whatever the value; on a set, members."
  (should (equal '(t t t nil t t) (semantics-map-contains))))

(ert-deftest clel-semantics-integer-keys ()
  "An integer is a key in a map and an index in a vector."
  (should (equal '(:jan :dec :none 20 (3 4)) (semantics-integer-keys))))

(ert-deftest clel-semantics-into-and-conj-maps ()
  (let ((r (semantics-into-map)))
    (should (clel-equal (nth 0 r) (clel-array-map :a 1 :b 2)))
    (should (clel-equal (nth 1 r) (clel-array-map :z 0 :a 1)))
    (should (clel-equal (nth 2 r) (clel-array-map :a 2 :b 3)))
    (should (clel-equal (nth 3 r) (clel-array-map :a 1 :b 2)))
    (should (equal 1 (clel-get (nth 0 r) :a)))))

(ert-deftest clel-semantics-assoc-dissoc-many ()
  (should (equal '((:a . 10) (:b . 2) (:c . 3)) (semantics-assoc-many)))
  (should (equal '((1 :x 3) (1 2 3)) (semantics-assoc-vector)))
  (should (equal '((:b . 2)) (semantics-dissoc-many))))

(ert-deftest clel-semantics-list-valued-entries ()
  "An entry whose value is a map or a vector destructures to that value,
not to its first element."
  (should (equal '((:night ((:theme . :dark)))
                   (:day ((:theme . :light) (:wall . "d.png")))
                   (:tags (:x)))
                 (clel-semantics-test--realize (semantics-list-valued-entries)))))

(ert-deftest clel-semantics-entry-accessors ()
  (should (equal '(:a (1 2) 2 (1 2) (1 2) ((1 2))) (semantics-entry-accessors)))
  (should (equal 6 (semantics-reduce-entries))))

(ert-deftest clel-semantics-map-and-vector-predicates ()
  (should (equal '(t t nil nil nil t nil t) (semantics-map-predicates))))

(ert-deftest clel-semantics-hash-map-is-a-map ()
  (should (equal '(t (2) t) (semantics-hash-map-builds-a-map))))

(ert-deftest clel-semantics-map-builders ()
  (let ((r (semantics-zipmap-and-friends)))
    (should (equal '((:a . 3) (:b . 2)) (nth 0 r)))
    (should (equal '((:x . 2) (:y . 1)) (nth 1 r)))
    (should (equal '((:a . 1) (:c . 3)) (nth 2 r)))
    (should (clel-equal (nth 3 r) (clel-array-map :a 1 :b 3 :c 4)))))

(ert-deftest clel-semantics-quoted-map-is-an-alist ()
  (should (equal '((:k . v)) (semantics-quoted-map))))

(ert-deftest clel-semantics-foreign-alists ()
  "An alist the runtime did not build (JSON, a defcustom) is still a map
when its entries are dotted pairs, and `get' reads it either way."
  (let ((json (json-parse-string "{\"a\": 1, \"b\": {\"c\": [1, 2]}}"
                                 :object-type 'alist :array-type 'list)))
    (should (clel-map-p json))
    (should (equal 1 (clel-get json 'a)))
    (should (equal '(1 2) (clel-get-in json '(b c))))
    (should (clel-equal '((a . 1) (b . 2)) '((b . 2) (a . 1))))
    (should (clel-contains-p '((a . 0)) 'a))))

(ert-deftest clel-semantics-empty-string ()
  (should (equal '(t nil nil (1)) (semantics-empty-string-empty))))

;;; Core functions

(ert-deftest clel-semantics-realized-seqs ()
  (should (equal '((2 3) (1 2) (:a (1 2)) (1 3) ((0 :a) (1 :b)))
                 (clel-semantics-test--realize (semantics-realized-seqs)))))

(ert-deftest clel-semantics-min-max-key ()
  (should (equal '(:night "abc" -1) (semantics-extreme-keys))))

(ert-deftest clel-semantics-names-and-keywords ()
  "name drops the colon and the namespace, keyword builds one."
  (should (equal '("night" "sym" "s" "n" "ns" :night :ns/n :k x ":night")
                 (semantics-names-and-keywords))))

(ert-deftest clel-semantics-coercions ()
  (should (equal '(3 -3 nil t t t t) (semantics-coercions))))

(ert-deftest clel-semantics-not-equal-is-generic ()
  "not= compares any values, not only numbers."
  (should (equal '(t nil t) (semantics-not-equal-generic))))

(ert-deftest clel-semantics-ex-message-and-data ()
  (should (equal '(("neg" ((:x . -1))) ("boom 1" nil)) (semantics-exceptions))))

(ert-deftest clel-semantics-parsing ()
  (should (equal '(42 -7 nil nil 2.5) (semantics-parsing))))

(ert-deftest clel-semantics-java-math ()
  (should (equal '(2.0 3.0 3 -2 1024.0 3 4.0 5 t -1.0 3.0) (semantics-math))))

;;; condp

(ert-deftest clel-semantics-condp ()
  (should (equal '(:one :two :other) (mapcar #'semantics-classify '(1 2 3))))
  (should (equal :one (semantics-classify-strict 1)))
  (should-error (semantics-classify-strict 9))
  (should (equal 7 (semantics-condp-arrow '(1 6))))
  (should (equal 3 (semantics-condp-arrow '(4))))
  (should (equal :none (semantics-condp-arrow '(9))))
  (should (equal '(:big :mid :small) (mapcar #'semantics-condp-fn-pred '(11 7 1)))))

;;; Emacs definition forms

(ert-deftest clel-semantics-defcustom-safe-predicate ()
  "A :safe #'pred option is a function, not a call to `var'."
  (should (eq 'integerp (get 'semantics-limit 'safe-local-variable)))
  (should (equal 3 semantics-limit)))

(ert-deftest clel-semantics-cl-defstruct-docstring ()
  "The docstring documents the struct instead of becoming slots."
  (should (equal '(:sunrise 0) (semantics-make-event)))
  (should (equal '(cl-tag-slot kind elevation)
                 (mapcar #'car (cl-struct-slot-info 'semantics-event)))))

(provide 'clojure-elisp-semantics-test)
;;; clojure-elisp-semantics-test.el ends here
