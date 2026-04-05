(ns clojure-elisp.regression-core-shadow-test
  "Regression tests for core function name shadowing.

   Bug: When a namespace defines a function with the same name as a
   Clojure core function (e.g., `list`, `map`, `filter`), the analyzer's
   pre-scan-defs registers it as a local def. Subsequent calls to the
   core function resolve as the local ns-qualified version, causing
   arity mismatches at runtime.

   Example: (ns my-pkg) (defn list [] ...) (list :a 1)
   Bug output:  (my-pkg-list :a 1)  — calls the 0-arg interactive command
   Fixed output: (list :a 1)         — calls the core list function

   This test file uses hive-test battle-tested macros:
   - hive-test.golden/deftest-golden   — snapshot/characterization testing
   - hive-test.mutation/deftest-mutations — mutation testing
   - hive-test.properties              — property-based testing"
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [hive-test.properties :as props]
            [hive-test.mutation :as mut]
            [hive-test.golden :as golden]
            [hive-test.generators.core :as gen-core]
            [clojure-elisp.core :as clel]
            [clojure-elisp.analyzer :as ana]
            [clojure-elisp.emitter :as emit]
            [clojure-elisp.mappings :as mappings]
            [clojure.string :as str]))

;; ============================================================================
;; Fixtures
;; ============================================================================

(use-fixtures :each
  (fn [test-fn]
    (ana/clear-macros!)
    (try (test-fn)
         (finally (ana/clear-macros!)))))

;; ============================================================================
;; Helpers
;; ============================================================================

(defn ae
  "Analyze form and emit to Elisp string."
  [form]
  (-> form ana/analyze emit/emit))

;; ============================================================================
;; Unit Tests: core `list` resolution without shadowing
;; ============================================================================

(deftest core-list-resolves-without-shadow
  (testing "bare `list` call emits as Elisp `list` (core mapping)"
    (is (= "(list :a 1)" (ae '(list :a 1))))
    (is (= "(list :text \"hello\" :pos 42)" (ae '(list :text "hello" :pos 42)))))

  (testing "list in compile-file-string without shadow uses core mapping"
    (let [source "(ns my-pkg)\n(defn make-plist [a b] (list :key a :val b))"
          result (clel/compile-file-string source)]
      (is (str/includes? result "(list :key a :val b)")
          "core list should emit as bare list, not ns-qualified"))))

;; ============================================================================
;; Unit Tests: core `list` shadowed by local defn (documents current behavior)
;; ============================================================================

(deftest core-list-shadowed-by-local-defn
  (testing "local defn named `list` shadows core — calls resolve to ns-qualified"
    (let [source "(ns my-pkg)\n(defn list [] \"lists things\")\n(defn make-data [] (list :a 1))"
          result (clel/compile-file-string source)]
      ;; After the fix, the compiler warns but still shadows (Clojure semantics)
      (is (str/includes? result "my-pkg-list")
          "local defn `list` should shadow core and produce ns-qualified calls"))))

;; ============================================================================
;; Unit Tests: the prompts.cljel pattern — plist construction with core list
;; ============================================================================

(deftest plist-construction-with-core-list
  (testing "namespace that uses (list :key val ...) for plist construction
            must emit bare `list`, not ns-qualified (after fixing source to not shadow)"
    (let [source "(ns my-prompts)
(defn- -extract [buffer]
  (list :text (buffer-substring buffer) :pos 42))
(defn- -queue [id text buf]
  (list :slave-id id :prompt text :buffer buf))
(defn show [] (message \"showing\"))"
          result (clel/compile-file-string source)]
      ;; The key assertion: list calls should be bare `list`
      (is (str/includes? result "(list :text")
          "plist construction should use bare list")
      (is (str/includes? result "(list :slave-id")
          "plist construction should use bare list")
      ;; The show function should be ns-qualified
      (is (str/includes? result "my-prompts-show")
          "defn show should be ns-qualified"))))

;; ============================================================================
;; Unit Tests: other core functions that could be shadowed
;; ============================================================================

(deftest core-map-not-shadowed
  (testing "core `map` resolves correctly when not shadowed"
    (is (= "(clel-map 1+ coll)" (ae '(map inc coll))))))

(deftest core-filter-not-shadowed
  (testing "core `filter` resolves correctly when not shadowed"
    (is (= "(clel-filter cl-evenp coll)" (ae '(filter even? coll))))))

(deftest core-cons-not-shadowed
  (testing "core `cons` resolves correctly when not shadowed"
    (is (= "(cons 1 coll)" (ae '(cons 1 coll))))))

;; ============================================================================
;; Golden Test: correct plist-constructing namespace output
;; ============================================================================

(golden/deftest-golden core-list-plist-golden
  "test/golden/core-list-plist.edn"
  (clel/compile-file-string
   "(ns plist-builder)
(defn- -make-entry [k v] (list :key k :val v))
(defn- -make-record [id name] (list :id id :name name :timestamp 0))
(defn show-entries [] (message \"done\"))"))

;; ============================================================================
;; Property Test: all core-mapped functions resolve when bare (no ns)
;; ============================================================================

(defspec core-mapping-bare-var-resolves 200
  (prop/for-all [fn-sym (gen/elements (keys mappings/core-fn-mapping))]
    (let [expected (get mappings/core-fn-mapping fn-sym)
          node     {:op :var :name fn-sym :ns nil}
          result   (emit/emit-node node)]
      (= expected result))))

;; ============================================================================
;; Property Test: core-mapped functions with clojure.core ns resolve
;; ============================================================================

(defspec core-mapping-clojure-core-resolves 200
  (prop/for-all [fn-sym (gen/elements (keys mappings/core-fn-mapping))]
    (let [expected (get mappings/core-fn-mapping fn-sym)
          node     {:op :var :name fn-sym :ns 'clojure.core}
          result   (emit/emit-node node)]
      (= expected result))))

;; ============================================================================
;; Property Test: compile-file-string totality for shadow-prone names
;; ============================================================================

(def gen-shadow-prone-source
  "Generator for source strings that define functions with core-mapping names."
  (gen/let [core-name (gen/elements ['list 'map 'filter 'cons 'first 'rest
                                     'count 'get 'str 'not 'apply 'identity])
            ns-suffix gen-core/gen-non-blank-string]
    (str "(ns pkg-" ns-suffix ")\n"
         "(defn " core-name " [] \"shadow\")\n"
         "(defn use-it [x] (" core-name " x))")))

(props/defprop-total compile-shadow-prone-total
  clel/compile-file-string
  gen-shadow-prone-source
  {:num-tests 100 :pred string?})

;; ============================================================================
;; Property Test: unshadowed core functions produce correct mapping
;; ============================================================================

(def gen-core-fn-call-source
  "Generator for source with core function calls (no shadowing)."
  (gen/let [ns-suffix gen-core/gen-non-blank-string]
    ;; Use list in a namespace that does NOT redefine it
    (str "(ns pkg-" ns-suffix ")\n"
         "(defn make-pair [a b] (list a b))")))

(defspec unshadowed-core-list-emits-bare 100
  (prop/for-all [source gen-core-fn-call-source]
    (ana/clear-macros!)
    (let [result (clel/compile-file-string source)]
      (str/includes? result "(list a b)"))))

;; ============================================================================
;; Mutation Test: emitter :var core-fn-mapping lookup is load-bearing
;; ============================================================================

(mut/deftest-mutations emitter-var-core-lookup-mutations-caught
  clojure-elisp.emitter/core-fn-mapping
  [["empty-mapping"     {}]
   ["nil-mapping"       nil]
   ["swapped-map"       (assoc mappings/core-fn-mapping 'map "WRONG")]
   ;; Use 'first (maps to "clel-first") not 'list (maps to "list" = same as mangle-name)
   ["missing-first"     (dissoc mappings/core-fn-mapping 'first)]
   ["swapped-first"     (assoc mappings/core-fn-mapping 'first "WRONG")]]
  (fn []
    ;; Use functions where mapping differs from mangle-name output
    (let [node-first  {:op :var :name 'first :ns nil}
          node-map    {:op :var :name 'map :ns nil}]
      (is (= "clel-first" (emit/emit-node node-first))
          "bare first var should emit as 'clel-first'")
      (is (= "clel-map" (emit/emit-node node-map))
          "bare map var should emit as 'clel-map'"))))

;; ============================================================================
;; Mutation Test: analyzer pre-scan-defs captures local definitions
;; ============================================================================

(mut/deftest-mutation-witness pre-scan-defs-captures-defn
  clojure-elisp.analyzer/pre-scan-defs
  ;; Mutant: always returns empty map (no local defs found)
  (fn [_forms] {})
  (fn []
    ;; compile a namespace with a function, verify CALL SITE is ns-qualified
    (ana/clear-macros!)
    (let [source "(ns my-pkg)\n(defn helper [x] x)\n(defn caller [] (helper 42))"
          result (clel/compile-file-string source)]
      ;; Check the call site specifically — (my-pkg-helper 42) not just "my-pkg-helper"
      ;; The defn definition is ALWAYS ns-qualified, but the call site depends on :defs
      (is (str/includes? result "(my-pkg-helper 42)")
          "call site must be ns-qualified: (my-pkg-helper 42)"))))

;; ============================================================================
;; Compiler warning test: shadowing core produces stderr warning
;; ============================================================================

(deftest compiler-warns-on-core-shadow
  (testing "Compiling a ns that shadows a core-mapped function emits a warning"
    (let [source "(ns my-pkg)\n(defn list [] \"shadow\")\n(defn use-it [] (list))"
          warnings (atom [])
          original-err *err*]
      (ana/clear-macros!)
      (binding [*err* (java.io.PrintWriter.
                       (proxy [java.io.Writer] []
                         (write
                           ([s] (swap! warnings conj (str s)))
                           ([s off len] (swap! warnings conj (String. (if (string? s) (.toCharArray ^String s) s) (int off) (int len)))))
                         (flush [])
                         (close [])))]
        (clel/compile-file-string source))
      (let [warning-text (str/join @warnings)]
        (is (str/includes? warning-text "list")
            "Warning should mention the shadowed function name")
        (is (or (str/includes? warning-text "shadow")
                (str/includes? warning-text "WARNING")
                (str/includes? warning-text "mapped"))
            "Warning should indicate shadowing")))))
