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

;; ============================================================================
;; Fix 4: #() reader-gensym trailing # stripped
;; ============================================================================

(deftest anon-fn-gensym-has-no-hash
  (testing "#() rest-arg gensym strips trailing # (else emits invalid-read-syntax \"#)\")"
    (let [out (emit-form (read-string "#(apply + %&)"))]
      (is (str/includes? out "&rest"))
      (is (not (str/includes? out "#")))))
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
