(ns clojure-elisp.regression-emitter-fixes-test
  "Regression coverage for the 2026-07-10 emitter/runtime correctness fixes
   (branch fix/cljel-emitter-correctness). One fix per section:

   - map literals    evaluate keys/values -> (list (cons k v) ...)   (was quoted alist)
   - nth / & rest    (nth c i) -> (clel-nth c i); rest -> (nthcdr i c) (was reversed nthrest)
   - variadic fn     (fn [a & b] ...) -> (... &rest b)
   - #() gensym      reader auto-gensym trailing # stripped (no invalid-read-syntax)
   - case/defmethod  string|nil|bool dispatch -> pcase / (eql <literal>) (cl-case eql fails)

   Exercises hive-test v0.3.0: deftrifecta (golden + mutation facets) with
   hive-test.mutation.combinators. Gensym-bearing emissions are asserted with
   includes?/absence checks (non-deterministic symbol names), while the golden
   facet snapshots only the deterministic, gensym-free forms."
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [hive-test.trifecta :refer [deftrifecta]]
            [hive-test.mutation.combinators :as mutc]
            [clojure-elisp.core :as clel]
            [clojure-elisp.analyzer :as ana]
            [clojure-elisp.emitter :as emit]
            [clojure.string :as str]))

;; ============================================================================
;; Subject
;; ============================================================================

(defn emit-form
  "Analyze a Clojure form and emit its Elisp string."
  [form]
  (-> form ana/analyze emit/emit))

;; ============================================================================
;; Trifecta — golden snapshot (deterministic forms) + mutation facet
;; ============================================================================

(deftrifecta emit-form-fixes
  #'emit-form
  {:golden-path "test/golden/emitter-fixes.edn"
   :cases       {:map-basic '{:a 1 :b 2}
                 :map-empty '{}
                 :map-expr  '{:a (+ 1 2)}
                 :nth       '(nth coll 2)
                 :case-kw   '(case x :a 1 :b 2 :d)
                 :case-str  '(case s "foo" 1 "bar" 2 :none)
                 :case-num  '(case n 1 :one 2 :two :other)}
   ;; each mutant produces output diverging from the snapshot -> caught by golden
   :mutations   [(mutc/always "WRONG")
                 (mutc/echo-arg)]})

;; ============================================================================
;; Fix 1: map literals evaluate keys/values
;; ============================================================================

(deftest map-literal-evaluates-pairs
  (testing "keys and values are evaluated, not frozen in a quoted alist"
    (is (= "(list (cons :a 1) (cons :b 2))" (emit-form '{:a 1 :b 2})))
    (is (= "(list (cons :a (+ 1 2)))"       (emit-form '{:a (+ 1 2)})))
    (is (= "(list )"                        (emit-form '{})))))

;; ============================================================================
;; Fix 2: nth arg-order + & rest destructuring
;; ============================================================================

(deftest nth-and-rest-destructuring
  (testing "nth maps to clel-nth (elt-based, bounds-safe)"
    (is (= "(clel-nth coll 2)" (emit-form '(nth coll 2)))))
  (testing "vector destructuring element access uses clel-nth"
    (is (str/includes? (emit-form '(let [[a b] xs] a)) "(clel-nth")))
  (testing "& rest destructuring uses (nthcdr idx coll) — elisp arg order, not (nthrest coll idx)"
    (let [out (emit-form '(let [[a & r] xs] r))]
      (is (str/includes? out "(nthcdr 1 "))
      (is (not (str/includes? out "nthrest"))))))

;; ============================================================================
;; Fix 3: variadic fn & rest
;; ============================================================================

(deftest variadic-fn-rest-param
  (testing "(fn [a & b] ...) emits an elisp &rest parameter"
    (is (str/includes? (emit-form '(fn [a & b] b)) "(a &rest b)"))))

(deftest variadic-arg-collision
  ;; Regression for kanban 20260710115813-58f15258:
  ;; the synthetic full-arglist var must NOT be named `args`. When a user param
  ;; is itself named `args`, the emitted destructuring let self-shadows:
  ;;   (let ((args (nthcdr 1 args))) ...) — under lexical-binding this reads the
  ;; WHOLE list, not the tail (silent corruption; broke clel-reduce et al.).
  ;; Fix: synthetic arglist is `clel--args`; the user `args` binds from it.
  (testing "single-arity variadic with a user param literally named `args`"
    (let [out (emit-form '(defn clel-reduce [f & args] (list f args)))]
      (is (str/includes? out "(&rest clel--args)"))
      (is (str/includes? out "(f (nth 0 clel--args))"))
      (is (str/includes? out "(args (nthcdr 1 clel--args))"))
      (is (not (str/includes? out "(nthcdr 1 args)")))
      (is (not (str/includes? out "(&rest args)")))))
  (testing "multi-arity with a user param literally named `args`"
    (let [out (emit-form '(defn f ([args] args) ([a & args] (list a args))))]
      (is (str/includes? out "(&rest clel--args)"))
      (is (str/includes? out "(length clel--args)"))
      (is (str/includes? out "(args (car clel--args))"))
      (is (str/includes? out "(args (nthcdr 1 clel--args))"))
      (is (not (str/includes? out "(&rest args)"))))))

(deftest cond-list-test-and-result
  ;; Regression for kanban 20260710133453-372031e6:
  ;; bare Clojure `cond` is ALWAYS flat test/expr pairs. The old analyzer
  ;; auto-detected "Elisp-style pre-grouped" via (every? seq? clauses), so a
  ;; flat cond whose tests AND results were all lists mis-compiled into
  ;; spurious (progn) clauses with broken pairing.
  (testing "flat cond with list test AND list result pairs correctly (no spurious progn)"
    (let [out (emit-form '(cond (pred x) (f) (pred2 y) (g)))]
      (is (str/includes? out "((pred x) (f))"))
      (is (str/includes? out "((pred2 y) (g))"))
      (is (not (str/includes? out "(progn")))))
  (testing "runtime-shaped nested cond (clel-merge) nests instead of flattening into progn"
    (let [out (emit-form '(cond (hash-table-p r)
                                (cond (hash-table-p m) (a) (listp m) (b))
                                (listp r) (c)))]
      (is (str/includes? out "((hash-table-p r) (cond"))
      (is (str/includes? out "((hash-table-p m) (a))"))
      (is (str/includes? out "((listp m) (b))"))
      (is (str/includes? out "((listp r) (c))"))))
  (testing "flat cond regressions: atoms, :else, list-test+atom-result still correct"
    (is (str/includes? (emit-form '(cond true 1 false 2)) "(t 1)"))
    (is (str/includes? (emit-form '(cond (> x 0) "pos" :else "zero")) "(t \"zero\")"))
    (is (str/includes? (emit-form '(cond (pred x) 1 (pred2 y) 2)) "((pred x) 1)")))
  (testing "elisp-cond STILL accepts the pre-grouped Elisp clause shape (desugars to flat)"
    (let [single (emit-form '(elisp-cond ((string= x "a") (do-a)) (t (default-action))))
          multi  (emit-form '(elisp-cond ((test-fn x) (setq r 1) (msg)) (t (other))))]
      (is (str/includes? single "((string= x \"a\") (do-a))"))
      (is (str/includes? single "(t (default-action))"))
      (is (str/includes? multi "progn")))))

(deftest cond-legacy-elisp-shape-fails-loud
  ;; Regression for the 0.6.1 silent miscompile found 2026-07-21 while rebuilding
  ;; hive-emacs: pre-grouped Elisp-style clauses passed to BARE `cond` were read as
  ;; flat test/expr pairs, so N clauses collapsed into ceil(N/2) groups whose car is
  ;; a former clause. Elisp then treats that car as a function to CALL -> byte-compile
  ;; "Malformed function", and an odd trailing clause (the `(t ...)` fallback) was
  ;; dropped outright. Both failure modes were SILENT: the compiler reported success.
  ;;
  ;; A test position whose head is itself a list is provably uncallable in Elisp
  ;; (function position admits a symbol or a lambda form, never a computed value),
  ;; so this detector cannot fire on the legitimate flat shapes above -- their heads
  ;; are symbols. That is what makes it strictly safer than the old, correctly
  ;; rejected (every? seq? clauses) auto-detection.
  (testing "pre-grouped Elisp clauses in bare cond throw, naming elisp-cond as the fix"
    (let [e (is (thrown? clojure.lang.ExceptionInfo
                         (emit-form '(cond ((= x 1) "one") ((= x 2) "two") (t "many")))))]
      (is (str/includes? (ex-message e) "elisp-cond")
          "error must point at the supported pre-grouped surface")))
  (testing "the exact hive-emacs shape that silently lost its (t ...) fallback throws"
    (is (thrown? clojure.lang.ExceptionInfo
                 (emit-form '(cond ((not (string-prefix-p p n)) (message "blocked") nil)
                                   (t (message "ok") t))))))
  (testing "odd flat clause count throws instead of silently dropping the last form"
    (is (thrown? clojure.lang.ExceptionInfo
                 (emit-form '(cond (pred x) (f) (pred2 y))))))
  (testing "a lambda in test-head position is genuinely callable and must NOT throw"
    (is (string? (emit-form '(cond ((lambda (v) v) 1) "yes" :else "no")))))
  (testing "legitimate flat conds still compile unchanged (no false positives)"
    (is (str/includes? (emit-form '(cond (pred x) (f) (pred2 y) (g))) "((pred x) (f))"))
    (is (str/includes? (emit-form '(cond (hash-table-p r)
                                         (cond (hash-table-p m) (a) (listp m) (b))
                                         (listp r) (c)))
                       "((listp r) (c))"))
    (is (str/includes? (emit-form '(cond true 1 false 2)) "(t 1)"))))

(deftest multi-arity-fn-and-defmethod-rest
  ;; Regression for kanban 20260710134255-3ce11ce0:
  ;;  (1) multi-arity anonymous fn silently dropped all but the first arity.
  ;;  (2) defmethod variadic emitted a bare `&` instead of Elisp `&rest`.
  (testing "multi-arity fn literal dispatches on arg count (no dropped arities)"
    (let [out (emit-form '(fn ([x] x) ([x y] (+ x y))))]
      (is (str/includes? out "(lambda (&rest clel--args)"))
      (is (str/includes? out "(cl-case (length clel--args)"))
      (is (str/includes? out "(1 (let ((x (car clel--args))) x))"))
      (is (str/includes? out "(2 (let ((x (car clel--args)) (y (cadr clel--args))) (+ x y)))"))))
  (testing "multi-arity fn literal with a variadic arity uses t catch-all"
    (let [out (emit-form '(fn ([x] x) ([x & more] more)))]
      (is (str/includes? out "(1 (let"))
      (is (str/includes? out "(t (let"))
      (is (str/includes? out "(more (nthcdr 1 clel--args))"))))
  (testing "single-arity fn is unchanged (plain lambda, no dispatch)"
    (is (= "(lambda (x)\n    x)" (emit-form '(fn [x] x))))
    (is (str/includes? (emit-form '(fn [a & xs] xs)) "(lambda (a &rest xs)")))
  (testing "one wrapped arity stays a plain lambda (count-1 guard)"
    (is (= "(lambda (x)\n    x)" (emit-form '(fn ([x] x))))))
  (testing "defmethod variadic emits &rest, not bare &"
    (let [out (emit-form '(defmethod area :k [a & args] (list a args)))]
      (is (str/includes? out "&rest args"))
      (is (not (str/includes? out "(eql :k)) & args"))))))

;; ============================================================================
;; Fix 4: #() reader-gensym trailing # stripped
;; ============================================================================

(deftest anon-fn-gensym-has-no-hash
  (testing "#() rest-arg gensym strips trailing # (else emits invalid-read-syntax \"#)\")"
    (let [out (emit-form (read-string "#(apply + %&)"))]
      (is (str/includes? out "&rest"))
      (is (not (re-find #"[0-9A-Za-z_]#" out)))))
  (testing "#() positional gensyms carry no #"
    (is (not (str/includes? (emit-form (read-string "#(+ %1 %2)")) "#")))))

;; ============================================================================
;; Fix 5: case / defmethod dispatch on string|nil|bool -> pcase / (eql literal)
;; ============================================================================

(deftest case-emits-pcase
  (testing "keyword dispatch"
    (let [out (emit-form '(case x :a 1 :b 2 :d))]
      (is (str/includes? out "pcase"))
      (is (str/includes? out "(_ :d)"))
      (is (not (str/includes? out "cl-case")))))
  (testing "string dispatch — cl-case eql fails on strings; pcase equal works"
    (let [out (emit-form '(case s "foo" 1 "bar" 2 :none))]
      (is (str/includes? out "pcase"))
      (is (str/includes? out "(\"foo\" 1)"))
      (is (str/includes? out "(_ :none)"))
      (is (not (str/includes? out "cl-case")))))
  (testing "numeric dispatch default arm uses (_ ...)"
    (is (str/includes? (emit-form '(case n 1 :one 2 :two :other)) "(_ :other)"))))

(deftest defmethod-string-dispatch
  (testing "defmethod with string / keyword dispatch-val emits (eql <literal>)"
    (let [out (clel/compile-file-string
               "(ns shp)\n(defmulti area :kind)\n(defmethod area \"circle\" [s] (:r s))\n(defmethod area :square [s] (:side s))")]
      (is (str/includes? out "(eql \"circle\")"))
      (is (str/includes? out "(eql :square)")))))

;; ============================================================================
;; Properties
;; ============================================================================

(defspec map-emit-total-and-structured 200
  (prop/for-all [m (gen/map gen/keyword gen/small-integer {:max-elements 6})]
    (let [out (emit-form m)]
      (and (string? out)
           (str/starts-with? out "(list")
           ;; one (cons k v) per entry — keys/values genuinely emitted
           (= (count m) (count (re-seq #"\(cons " out)))))))

(defspec case-emits-pcase-never-cl-case 100
  (prop/for-all [ks (gen/fmap distinct (gen/not-empty (gen/vector gen/small-integer)))]
    (let [clauses (mapcat (fn [k] [k (keyword (str "v" k))]) ks)
          form    (concat (list 'case 'x) clauses (list :default))
          out     (emit-form form)]
      (and (str/includes? out "pcase")
           (not (str/includes? out "cl-case"))))))

;; ============================================================================
;; Fix 5: loop-less recur binds via cl-labels (was a bare call to undefined recur)
;; ============================================================================

(deftest defn-tail-recur-binds-recur
  (testing "loop-less recur in a defn emits a cl-labels recur binder"
    (let [out (emit-form '(defn f [x] (if (> x 0) (recur (dec x)) x)))]
      (is (str/includes? out "cl-labels ((recur"))
      (is (str/includes? out "(recur x)"))))
  (testing "loop-less recur in a fn (lambda) binds recur too"
    (let [out (emit-form '(fn [x] (if (> x 0) (recur (dec x)) x)))]
      (is (str/includes? out "lambda"))
      (is (str/includes? out "cl-labels ((recur"))))
  (testing "recur belonging to a nested loop is not double-wrapped"
    (let [out (emit-form '(defn g [x] (loop [i 0] (when (< i x) (recur (inc i))))))]
      (is (= 1 (count (re-seq #"cl-labels \(\(recur" out))))))
  (testing "a defn without recur emits no cl-labels"
    (is (not (str/includes? (emit-form '(defn h [x] (+ x 1))) "cl-labels"))))
  (testing "variadic / multi-arity self-recur fails loudly, not silently broken"
    (is (thrown-with-msg? clojure.lang.ExceptionInfo #"not supported"
                          (emit-form '(defn bad [x & xs] (recur xs)))))
    (is (thrown-with-msg? clojure.lang.ExceptionInfo #"not supported"
                          (emit-form '(fn [x & xs] (recur xs)))))
    (is (thrown-with-msg? clojure.lang.ExceptionInfo #"not supported"
                          (emit-form '(defn m ([x] (recur x)) ([x y] x)))))))

(def gen-recur-defn
  "A loop-less, fixed-arity defn/fn whose tail recur targets itself."
  (gen/elements
   ['(defn a [x] (if (> x 0) (recur (dec x)) x))
    '(defn b [n acc] (if (> n 0) (recur (dec n) (+ acc n)) acc))
    '(defn c [x] (when (> x 0) (recur (dec x))))
    '(fn [x] (if (> x 0) (recur (dec x)) x))]))

(defspec loop-less-recur-always-bound 100
  (prop/for-all [form gen-recur-defn]
    (let [out (emit-form form)]
      ;; a (recur is emitted, and a cl-labels binder for it is always present
      (and (str/includes? out "(recur")
           (str/includes? out "cl-labels ((recur")))))