;;; clojure-elisp-runtime-test.el --- ERT tests for the cljel runtime -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Behavioural cover for the runtime library. The Clojure suite asserts on
;; EMITTED STRINGS, which cannot see a runtime defect: emitted code that reads
;; correctly still returned 4 for (count (map inc '(1 2 3))). These tests load
;; clojure-elisp-runtime.el and call it.
;;
;; The central law: an eager consumer must answer the same for a lazy sequence
;; as for the realized list it stands for.
;;
;; Run:
;;   emacs -Q -batch -l ert \
;;         -l test/elisp/clojure-elisp-runtime-test.el \
;;         -f ert-run-tests-batch-and-exit

;;; Code:

(require 'ert)
(require 'subr-x)

(load (expand-file-name
       "../../resources/clojure-elisp/clojure-elisp-runtime.el"
       (file-name-directory (or load-file-name buffer-file-name)))
      nil t)

(defun clel-test--lazy (&rest items)
  "Return ITEMS as an unrealized lazy sequence."
  (clel-map #'identity items))

;;; The law: f(lazy) equals f(realized)

(ert-deftest clel-lazy-parity-counting ()
  "`count' measures the sequence, never the lazy-seq struct."
  (should (equal 3 (clel-count (clel-test--lazy 1 2 3))))
  (should (equal 0 (clel-count (clel-test--lazy))))
  (should (equal 3 (clel-count (list 1 2 3))))
  (should (equal 3 (clel-count (vector 1 2 3)))))

(ert-deftest clel-lazy-parity-reduction ()
  "Reducing consumers force before they fold."
  (should (equal 9 (clel-reduce #'+ (clel-map #'1+ (list 1 2 3)))))
  (should (equal 19 (clel-reduce #'+ 10 (clel-map #'1+ (list 1 2 3)))))
  (should (equal 6 (clel-reduce #'+ (list 1 2 3)))))

(ert-deftest clel-lazy-parity-apply ()
  "`apply' spreads a lazy trailing argument, with and without leading args."
  (should (equal 9 (clel-apply #'+ (clel-map #'1+ (list 1 2 3)))))
  (should (equal 19 (clel-apply #'+ 10 (clel-map #'1+ (list 1 2 3)))))
  (should (equal 6 (clel-apply #'+ (list 1 2 3))))
  (should (equal 0 (clel-apply #'+))))

(ert-deftest clel-lazy-parity-positional ()
  "Positional accessors force rather than index into the struct."
  (let ((lazy (clel-map #'1+ (list 1 2 3))))
    (should (equal 2 (clel-first lazy)))
    (should (equal 3 (clel-second (clel-map #'1+ (list 1 2 3)))))
    (should (equal 4 (clel-last (clel-map #'1+ (list 1 2 3)))))
    (should (equal 2 (clel-nth (clel-map #'1+ (list 1 2 3)) 0)))
    (should (equal 4 (clel-nth (clel-map #'1+ (list 1 2 3)) 2)))
    (should (equal 2 (clel-peek (clel-map #'1+ (list 1 2 3)))))))

(ert-deftest clel-lazy-parity-conversion ()
  "Conversions produce the realized collection, not the struct."
  (should (equal (list 2 3 4) (clel-into (list) (clel-map #'1+ (list 1 2 3)))))
  (should (equal (vector 2 3 4) (clel-into (vector) (clel-map #'1+ (list 1 2 3)))))
  (should (equal (list 2 3 4) (clel-seq (clel-map #'1+ (list 1 2 3)))))
  (should (equal (list 2 3 4 9) (clel-conj (clel-map #'1+ (list 1 2 3)) 9)))
  (should (equal (list 4 3 2) (clel-reverse (clel-map #'1+ (list 1 2 3)))))
  (should (equal (list 2 3) (clel-butlast (clel-map #'1+ (list 1 2 3)))))
  (should (equal "2-3-4" (clel-str-join "-" (clel-map #'1+ (list 1 2 3))))))

(ert-deftest clel-lazy-parity-ordering-and-grouping ()
  "Sorting and grouping see every element exactly once."
  (should (equal (list 2 3 4) (clel-sort #'< (clel-map #'1+ (list 3 1 2)))))
  (should (equal (list 2 3 4) (clel-sort-by #'identity (clel-map #'1+ (list 3 1 2)))))
  (should (equal (list (cons 2 1) (cons 3 1) (cons 4 1))
                 (clel-frequencies (clel-map #'1+ (list 1 2 3)))))
  (should (equal (list (cons t (list 2 4)) (cons nil (list 3)))
                 (clel-group-by #'cl-evenp (clel-map #'1+ (list 1 2 3))))))

(ert-deftest clel-lazy-parity-predicates ()
  "Predicates stop at the real end of the sequence, not one past it."
  (should (equal t (clel-every-p #'integerp (clel-map #'1+ (list 1 2 3)))))
  (should (equal nil (clel-every-p #'cl-evenp (clel-map #'1+ (list 1 2 3)))))
  (should (equal t (clel-some #'cl-evenp (clel-map #'1+ (list 1 2 3)))))
  (should (equal nil (clel-empty-p (clel-map #'1+ (list 1 2 3)))))
  (should (equal t (clel-empty-p (clel-map #'1+ (list)))))
  (should (equal t (clel-contains-p (clel-map #'1+ (list 1 2 3)) 3))))

(ert-deftest clel-lazy-parity-set-and-map-building ()
  "Hash-table builders key on elements, never on the struct."
  (should (equal 3 (hash-table-count
                    (clel-set-from-coll (clel-map #'1+ (list 1 2 3))))))
  (should (equal (list (cons 2 :a))
                 (clel-zipmap (clel-map #'1+ (list 1)) (list :a)))))

;;; A lazy tail reached through a plain cons is still lazy

(ert-deftest clel-lazy-tail-through-cons ()
  "A realized head with a lazy tail is realized before a raw primitive sees it."
  (should (equal 3 (clel-count (cons 1 (clel-map #'identity (list 2 3))))))
  (should (equal 3 (clel-last (cons 1 (clel-map #'identity (list 2 3)))))))

;;; Laziness is preserved: infinite sequences must not be realized

(ert-deftest clel-laziness-preserved-on-infinite-seqs ()
  "Taking from an infinite sequence terminates."
  (should (equal (list 1 2 1 2 1) (clel-realize (clel-take 5 (clel-cycle (list 1 2))))))
  (should (equal (list 0 1 2) (clel-realize (clel-take 3 (clel-iterate #'1+ 0)))))
  (should (equal (list 7 7) (clel-realize (clel-take 2 (clel-repeat 100 7))))))

;;; Composition across lazy operators

(ert-deftest clel-lazy-composition ()
  "Lazy operators compose and realize once at the end."
  (should (equal (list 1 3) (clel-realize (clel-remove #'cl-evenp (list 1 2 3)))))
  (should (equal (list 2) (clel-realize (clel-filter #'cl-evenp (list 1 2 3)))))
  (should (equal (list 1 1 2 2)
                 (clel-realize (clel-mapcat (lambda (x) (list x x)) (list 1 2)))))
  (should (equal (list 1 2 3)
                 (clel-realize (clel-concat (list 1) (clel-map #'identity (list 2 3))))))
  (should (equal (list 5 7) (clel-realize (clel-map #'+ (list 1 2) (list 4 5)))))
  (should (equal 9 (clel-transduce (clel-map-xf #'1+) #'+ 0 (list 1 2 3)))))

;;; Arity dispatch: an empty collection is not an absent collection

(ert-deftest clel-transducer-arity-is-not-emptiness ()
  "Recursing onto an empty tail must yield nil, never a transducer."
  (should (equal (list 1 2) (clel-realize (clel-distinct (list 1 2 1 2)))))
  (should (equal (list 1 2) (clel-realize (clel-distinct (clel-map #'identity (list 1 2 1))))))
  (should (equal nil (clel-realize (clel-distinct (list)))))
  (should (equal (list 2 4)
                 (clel-realize (clel-keep (lambda (x) (and (cl-evenp x) x))
                                          (list 1 2 3 4)))))
  (should (equal nil (clel-realize (clel-keep #'identity (list)))))
  (should (equal (list 1 2 1) (clel-realize (clel-dedupe (list 1 1 2 2 1)))))
  (should (equal (list 1 0 2) (clel-realize (clel-interpose 0 (list 1 2)))))
  (should (equal nil (clel-realize (clel-interpose 0 (list))))))

(ert-deftest clel-transducer-arity-still-returns-a-transducer ()
  "The no-collection arities keep handing back a transducer."
  (should (functionp (clel-distinct)))
  (should (functionp (clel-dedupe)))
  (should (functionp (clel-keep #'identity)))
  (should (functionp (clel-interpose 0))))

;;; A default applies only when the key is ABSENT

(ert-deftest clel-get-default-only-on-absence ()
  "A present nil or false is returned as itself, never replaced by the default.
`or' against the default is what made destructuring :or lose a deliberately
falsy value."
  (let ((ht (clel-hash-map :x nil :y 7))
        (al (list (cons :x nil) (cons :y 7))))
    (should (equal nil (clel-get ht :x 5)))
    (should (equal 7 (clel-get ht :y 5)))
    (should (equal 5 (clel-get ht :z 5)))
    (should (equal nil (clel-get al :x 5)))
    (should (equal 7 (clel-get al :y 5)))
    (should (equal 5 (clel-get al :z 5)))))

(ert-deftest clel-get-default-on-indexed-collections ()
  "Index lookups distinguish a nil element from an out-of-range index."
  (should (equal 2 (clel-get (list 1 2 3) 1 99)))
  (should (equal nil (clel-get (list 1 nil 3) 1 99)))
  (should (equal 99 (clel-get (list 1 2 3) 9 99)))
  (should (equal 2 (clel-get (vector 1 2 3) 1 99)))
  (should (equal 99 (clel-get (vector 1 2 3) 9 99)))
  (should (equal 5 (clel-get nil :x 5))))

(provide 'clojure-elisp-runtime-test)
;;; clojure-elisp-runtime-test.el ends here
