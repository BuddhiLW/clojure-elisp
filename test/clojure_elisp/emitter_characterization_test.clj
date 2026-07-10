(ns clojure-elisp.emitter-characterization-test
  "Trifecta characterization of the ClojureElisp emitter.

  Pins `emit` behaviour BEFORE the planned emitter.clj -> sibling-namespace
  decomposition, so that refactor is provably behaviour-preserving: after the
  split, re-running this namespace must reproduce every golden snapshot
  byte-for-byte.

  Three arms (hive-test `deftrifecta`, one macro -> three tests):
    * GOLDEN   — snapshot `emit` over a corpus spanning every feature category.
    * PROPERTY — `emit` is total (returns a non-empty String) over generated forms.
    * MUTATION — a broken `emit`/`mangle-name` must fail the golden (guards the net).

  Non-determinism: destructuring emits gensym names (vec__NNNN) and `reify`
  advances a global counter (clel--reify-NNNN); `canon` normalizes both so the
  goldens are stable across runs. Regenerate goldens intentionally with
  `UPDATE_GOLDEN=true clojure -M:test`."
  (:require [clojure.test :refer [deftest is use-fixtures]]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [hive-test.trifecta :refer [deftrifecta]]
            [hive-test.mutation :as mut]
            [hive-test.mutation.combinators :as mutc]
            [clojure-elisp.core :as clel]
            [clojure-elisp.analyzer :as ana]
            [clojure-elisp.emitter :as emit]
            [clojure.string :as str]))

;; The analyzer keeps a process-wide macro registry — reset around every test
;; so this namespace cannot perturb (or be perturbed by) the rest of the suite.
(use-fixtures :each (fn [t] (ana/clear-macros!) (try (t) (finally (ana/clear-macros!)))))

(defn canon
  "Normalize non-deterministic names in emitted Elisp so snapshots are stable:
   gensyms (`vec__20515`) and the global `reify` counter (`clel--reify-2`).
   Coerces via `str` first so mutant outputs (nil, non-strings) compare cleanly
   instead of throwing."
  [s]
  (-> (str s)
      (str/replace #"__\d+" "__N")
      (str/replace #"reify-\d+" "reify-N")))

;; --- generators -----------------------------------------------------------

(def ^:private gen-leaf
  (gen/one-of [gen/small-integer
               (gen/return 'x)
               (gen/return 'y)
               gen/string-alphanumeric
               (gen/return true)
               (gen/return nil)]))

(def ^:private gen-emittable-form
  "Forms the emitter handles without throwing — leaves plus a few nestable
   special forms. Deliberately excludes destructuring/reify so property/
   determinism checks need no gensym normalization."
  (gen/one-of
    [gen-leaf
     (gen/fmap (fn [[a b]]   (list '+ a b))                  (gen/tuple gen-leaf gen-leaf))
     (gen/fmap (fn [[p a b]] (list 'if p a b))               (gen/tuple gen-leaf gen-leaf gen-leaf))
     (gen/fmap (fn [[a b]]   (list 'do a b))                 (gen/tuple gen-leaf gen-leaf))
     (gen/fmap (fn [[a b]]   (list 'let ['v a] (list '+ 'v b))) (gen/tuple gen-leaf gen-leaf))]))

(def ^:private gen-mangle-symbol
  "Symbols mixing plain and mangle-triggering characters (?, !, *, ->)."
  (gen/fmap (comp symbol str/join)
            (gen/vector (gen/elements [\a \b \c \x \y \z \- \? \! \* \+ \>]) 1 10)))

;; --- the emit corpus (golden + mutation subjects) -------------------------

(def emitter-corpus
  "Representative Clojure forms spanning every emitter feature category.
   Values are quoted forms; keys label the category. Applied 1-arg to `emit`
   by the golden and mutation arms of the `core-emit` trifecta."
  '{:const-num 42, :const-str "hi", :const-kw :kw, :const-bool true, :const-nil nil
    :call (+ 1 2 3), :inc (inc x)
    :defn (defn foo [x] (+ x 1))
    :defn-multi (defn bar ([x] x) ([x y] (+ x y)))
    :defn-vararg (defn baz [& xs] xs)
    :defn-pred (defn valid? [x] x)
    :defn-bang (defn go! [] 1)
    :fn (fn [x] (* x x))
    :if (if p a b), :when (when p a b)
    :cond (cond p1 a :else b), :case (case x 1 :one 2 :two :other)
    :let (let [a 1 b 2] (+ a b))
    :let-vec (let [[a b] xs] (+ a b))
    :let-map (let [{:keys [x y]} m] (+ x y))
    :loop (loop [i 0] (if (< i 10) (recur (inc i)) i))
    :do (do a b), :and (and a b), :or (or a b)
    :thread (-> x f g)
    :defprotocol (defprotocol P (m [this]))
    :defrecord (defrecord R [a b])
    :reify (reify P (m [this] 1))
    :deftype (deftype T [x])
    :defmulti (defmulti area :shape)
    :atom (atom 0), :swap (swap! a inc), :deref (deref a), :reset (reset! a 1)
    :try (try a (catch Error e e) (finally c))
    :throw (throw (ex-info "m" {}))
    :save-excursion (save-excursion a b)
    :with-current-buffer (with-current-buffer buf a)
    :with-temp-buffer (with-temp-buffer a)
    :save-restriction (save-restriction a)
    :str (str "a" b), :first (first xs), :map (map f xs), :assoc (assoc m :k v)})

;; --- TRIFECTA 1: core/emit ------------------------------------------------
;; Expands to: core-emit-golden (deftest), core-emit-property (defspec),
;;             core-emit-mutations (deftest).

(deftrifecta core-emit
  clel/emit
  {:golden-path "test/golden/emitter-corpus.edn"
   :cases       emitter-corpus
   :xf          canon
   :gen         gen-emittable-form
   :pred        (fn [out] (and (string? out) (pos? (count out))))
   :num-tests   200
   :mutations   [(mutc/always nil)
                 (mutc/echo-arg 0)]})

;; --- TRIFECTA 2: emitter/mangle-name --------------------------------------
;; The name mangler is load-bearing for every emitted identifier.

(deftrifecta mangle-name
  emit/mangle-name
  {:golden-path "test/golden/mangle-name.edn"
   :cases       {:pred 'valid?
                 :bang 'go!
                 :star 'foo*
                 :arrow 'next->
                 :plain 'plain-name}
   :gen         gen-mangle-symbol
   :num-tests   200
   :mutations   [(mutc/echo-arg 0)
                 (mutc/always "MUT")]})

;; --- Cross-layer mutation witness -----------------------------------------
;; Proves a broken mangle-name is observable at the emit surface (not just in
;; its own golden). Phase 1 asserts the real mappings; phase 2 kills mutants.

(mut/deftest-mutations mangle-name-drives-emit
  emit/mangle-name
  [(mutc/echo-arg 0)
   (mutc/always "zzz")]
  (fn []
    (is (= "(defun valid-p (x)\n  x)" (clel/emit '(defn valid? [x] x))))
    (is (= "(defun go-bang ()\n  1)" (clel/emit '(defn go! [] 1))))))

;; --- Purity / referential-transparency ------------------------------------
;; emit must remain a pure function of its input across the decomposition.

(defspec emit-is-referentially-transparent 200
  (prop/for-all [f gen-emittable-form]
    (= (canon (clel/emit f)) (canon (clel/emit f)))))
