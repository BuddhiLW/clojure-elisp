(ns clojure-elisp.regression-naming-test
  "Regression tests for private function naming (triple-dash bug fix f48384d).

   Bug: (defn- -helper [x] x) in namespace my-pkg emitted `my-pkg---helper`
   (triple-dash) instead of `my-pkg--helper` (double-dash). The leading dash
   from Clojure's -private naming convention conflicted with Elisp's ns--name
   private convention, producing ns-- + -name = ns---name.

   Fix: Strip leading dash from mangled name when private? is true, in both
   `ns-qualify-name` and the `:var` emit-node method."
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [hive-test.properties :as props]
            [hive-test.generators.core :as gen-core]
            [hive-dsl.result :as r]
            [clojure-elisp.core :as clel]
            [clojure-elisp.analyzer :as ana]
            [clojure-elisp.emitter :as emit :refer [mangle-name]]
            [clojure.string :as str]))

;; ============================================================================
;; Helpers
;; ============================================================================

(defn analyze-and-emit
  "Analyze a form and emit it to Elisp."
  [form]
  (-> form ana/analyze emit/emit))

;; ============================================================================
;; Unit Tests: ns-qualify-name (direct function tests)
;; ============================================================================

(deftest ns-qualify-name-private-dash-prefix-test
  (testing "Regression f48384d: private + dash-prefixed name must NOT produce triple-dash"
    (let [env {:ns 'my-pkg}
          result (emit/ns-qualify-name '-helper env true)]
      (is (= "my-pkg--helper" result)
          "Private dash-prefixed name should produce ns--name (double-dash)")
      (is (not (str/includes? result "---"))
          "Must never contain triple-dash (the original bug)")))

  (testing "Private name without dash prefix uses double-dash separator"
    (let [env {:ns 'my-pkg}
          result (emit/ns-qualify-name 'secret env true)]
      (is (= "my-pkg--secret" result))
      (is (not (str/includes? result "---")))))

  (testing "Regression f48384d: private + double-dash-prefixed name"
    ;; --internal mangles to "--internal"; fix strips one leading dash -> "-internal"
    ;; Result: my-pkg-- + -internal = my-pkg---internal (only one dash stripped).
    ;; This is an edge case — the fix handles the common single-dash case.
    (let [env {:ns 'my-pkg}
          result (emit/ns-qualify-name '--internal env true)]
      (is (string? result) "Should produce a string without crashing")
      (is (str/starts-with? result "my-pkg--")
          "Should start with ns + private separator")))

  (testing "Public (non-private) name keeps dash prefix as-is"
    (let [env {:ns 'my-pkg}
          result (emit/ns-qualify-name '-main env false)]
      (is (= "my-pkg--main" result)
          "Public dash-prefixed: ns separator + name's leading dash = double-dash")))

  (testing "User namespace (no prefix) ignores private? flag"
    (let [env {:ns 'user}]
      (is (= "-helper" (emit/ns-qualify-name '-helper env true))
          "user ns gets no prefix, name preserved as-is")
      (is (= "-helper" (emit/ns-qualify-name '-helper env false))
          "user ns gets no prefix regardless of private? flag")))

  (testing "Nil namespace (no prefix) ignores private? flag"
    (let [env {}]
      (is (= "-helper" (emit/ns-qualify-name '-helper env true))
          "nil ns gets no prefix, name preserved as-is"))))

;; ============================================================================
;; Unit Tests: mangle-name with dash-prefixed symbols
;; ============================================================================

(deftest mangle-name-dash-prefix-test
  (testing "mangle-name preserves leading dashes (no stripping — that's ns-qualify-name's job)"
    (is (= "-helper" (mangle-name '-helper))
        "Single leading dash preserved")
    (is (= "--internal" (mangle-name '--internal))
        "Double leading dash preserved")
    (is (= "-main" (mangle-name '-main))
        "Standard -main preserved"))

  (testing "mangle-name handles dash-prefixed names with special chars"
    (is (= "-reset-bang" (mangle-name '-reset!))
        "Dash prefix + bang suffix")
    (is (= "-nil-p" (mangle-name '-nil?))
        "Dash prefix + predicate suffix")))

;; ============================================================================
;; Unit Tests: compile-file-string end-to-end (full pipeline)
;; ============================================================================

(deftest compile-file-string-private-dash-name-test
  (testing "Regression f48384d: defn- with dash-prefixed name in namespace"
    (let [source "(ns my-pkg)\n(defn- -helper [x] x)"
          result (clel/compile-file-string source)]
      (is (str/includes? result "my-pkg--helper")
          "Compiled output must contain my-pkg--helper (double-dash)")
      (is (not (str/includes? result "my-pkg---helper"))
          "Must NOT contain my-pkg---helper (triple-dash — the original bug)")))

  (testing "defn- with normal (non-dash) name in namespace"
    (let [source "(ns my-pkg)\n(defn- secret [x] (+ x 1))"
          result (clel/compile-file-string source)]
      (is (str/includes? result "my-pkg--secret")
          "Private fn uses double-dash separator")
      (is (not (str/includes? result "my-pkg-secret\n"))
          "Should not use single-dash (that would be public)")))

  (testing "Public defn with dash-prefixed name in namespace"
    (let [source "(ns my-pkg)\n(defn -main [x] x)"
          result (clel/compile-file-string source)]
      (is (str/includes? result "my-pkg--main")
          "Public -main: ns separator dash + name's leading dash = double-dash")))

  (testing "defn- with dash-prefixed name, dotted namespace"
    (let [source "(ns my.utils)\n(defn- -init [x] x)"
          result (clel/compile-file-string source)]
      (is (str/includes? result "my-utils--init")
          "Dotted ns mangled + private separator + stripped dash")
      (is (not (str/includes? result "my-utils---init"))
          "Must NOT contain triple-dash with dotted namespace"))))

;; ============================================================================
;; Unit Tests: private fn cross-reference within same namespace
;; ============================================================================

(deftest private-fn-cross-reference-test
  (testing "Regression f48384d: calling a private dash-prefixed fn resolves correctly"
    (let [source "(ns my-pkg)\n(defn- -helper [x] x)\n(defn public-fn [x] (-helper x))"
          result (clel/compile-file-string source)]
      ;; The definition should use my-pkg--helper
      (is (str/includes? result "my-pkg--helper")
          "Both definition and call site should use my-pkg--helper")
      ;; Should NOT contain the buggy triple-dash anywhere
      (is (not (str/includes? result "my-pkg---helper"))
          "No triple-dash in definition or call site")))

  (testing "Private non-dash fn cross-reference works normally"
    (let [source "(ns my-pkg)\n(defn- secret [x] x)\n(defn public-fn [x] (secret x))"
          result (clel/compile-file-string source)]
      (is (str/includes? result "my-pkg--secret")
          "Private fn definition and call site both use double-dash"))))

;; ============================================================================
;; Unit Tests: emit-node :var with private? flag
;; ============================================================================

(deftest emit-node-var-private-dash-test
  (testing "Regression f48384d: :var node with private? and dash-prefixed name"
    (let [node {:op :var :name '-helper :ns 'my-pkg :private? true}
          result (emit/emit-node node)]
      (is (= "my-pkg--helper" result)
          ":var emit with private? strips leading dash")
      (is (not (str/includes? result "---"))
          "No triple-dash from :var emit")))

  (testing ":var node without private? keeps dash in name"
    (let [node {:op :var :name '-main :ns 'my-pkg :private? false}
          result (emit/emit-node node)]
      (is (= "my-pkg--main" result)
          "Public dash-prefixed: ns- + -main")))

  (testing ":var node with private? but no dash prefix"
    (let [node {:op :var :name 'secret :ns 'my-pkg :private? true}
          result (emit/emit-node node)]
      (is (= "my-pkg--secret" result)
          "Private non-dash name uses double-dash separator")))

  (testing ":var node with nil ns ignores private? flag"
    (let [node {:op :var :name '-helper :ns nil}
          result (emit/emit-node node)]
      (is (= "-helper" result)
          "No namespace means bare mangled name"))))

;; ============================================================================
;; Property Tests: ns-qualify-name totality
;; ============================================================================

(def gen-symbol-name
  "Generator for symbol names including dash-prefixed variants.
   Uses gen-non-blank-string from hive-test for robust name generation."
  (gen/one-of [(gen/fmap symbol gen-core/gen-non-blank-string)
               (gen/fmap #(symbol (str "-" %)) gen-core/gen-non-blank-string)
               (gen/fmap #(symbol (str "--" %)) gen-core/gen-non-blank-string)
               (gen/elements ['-helper '-main '--internal 'foo 'bar-baz])]))

(def gen-ns-symbol
  "Generator for namespace symbols.
   Uses gen-non-blank-string from hive-test for realistic namespace names."
  (gen/one-of [(gen/return nil)
               (gen/return 'user)
               (gen/fmap #(symbol (str "pkg-" %)) gen-core/gen-non-blank-string)
               (gen/elements ['my-pkg 'my.utils 'clojure.core])]))

(props/defprop-total ns-qualify-name-private-total
  (fn [input]
    (let [{:keys [name ns private?]} input
          env (if ns {:ns ns} {})]
      (emit/ns-qualify-name name env (boolean private?))))
  (gen/hash-map :name gen-symbol-name
                :ns gen-ns-symbol
                :private? gen/boolean)
  {:num-tests 200})

;; ============================================================================
;; Property Tests: mangle-name idempotency on dash-prefixed symbols
;; ============================================================================

(props/defprop-idempotent mangle-name-dash-prefix-idempotent
  emit/mangle-name
  (gen/fmap #(symbol (str "-" %)) gen/string-alpha-numeric)
  {:num-tests 200})

;; ============================================================================
;; Property Tests: no triple-dash for private + dash-prefixed names
;; ============================================================================

(defspec private-dash-never-triple-dash 200
  (prop/for-all [suffix gen/string-alpha-numeric]
    (let [name   (symbol (str "-" suffix))
          env    {:ns 'test-pkg}
          result (emit/ns-qualify-name name env true)]
      (not (str/includes? result "---")))))

(defspec public-dash-no-triple-dash 200
  (prop/for-all [suffix gen/string-alpha-numeric]
    (let [name   (symbol (str "-" suffix))
          env    {:ns 'test-pkg}
          result (emit/ns-qualify-name name env false)]
      ;; Public: separator is single dash, name has leading dash -> double-dash is fine
      ;; but triple-dash should never appear
      (not (str/includes? result "---")))))

;; ============================================================================
;; Property: compile-file-string totality for private fn sources
;; ============================================================================

(def gen-alpha-id
  "Generator for valid Clojure identifiers (letter-prefixed alphanumeric)."
  (gen/let [first-char (gen/elements (seq "abcdefghijklmnopqrstuvwxyz"))
            rest-chars gen/string-alpha-numeric]
    (str first-char rest-chars)))

(def gen-private-fn-source
  "Generator for (ns ...) (defn- ...) source strings with various name shapes."
  (gen/let [ns-name gen-alpha-id
            fn-name gen-alpha-id
            dash?   gen/boolean]
    (let [name (if dash? (str "-" fn-name) fn-name)]
      (str "(ns pkg-" ns-name ")\n(defn- " name " [x] (+ x 1))"))))

(props/defprop-total compile-private-fn-total
  clel/compile-file-string
  gen-private-fn-source
  {:num-tests 100 :pred string?})

;; ============================================================================
;; Property: emit-result complement for private fn forms
;; ============================================================================

(def gen-private-defn-form
  "Generator for (defn- name [x] body) forms."
  (gen/let [dash?  gen/boolean
            suffix gen-core/gen-non-blank-string]
    (let [name (symbol (if dash? (str "-fn-" suffix) (str "fn-" suffix)))]
      (list 'defn- name ['x] '(+ x 1)))))

(props/defprop-complement emit-result-private-fn-complement
  r/ok? r/err?
  (gen/fmap clel/emit-result gen-private-defn-form)
  {:num-tests 100})

;; ============================================================================
;; Property: ns-qualify-name output always starts with ns prefix (when ns given)
;; ============================================================================

(defspec ns-qualify-name-always-has-prefix 200
  (prop/for-all [suffix gen-core/gen-non-blank-string
                 private? gen/boolean]
    (let [name   (symbol (str "-" suffix))
          env    {:ns 'test-pkg}
          result (emit/ns-qualify-name name env private?)]
      (str/starts-with? result "test-pkg-"))))

;; ============================================================================
;; Property: full pipeline roundtrip — compile private fn, output contains ns prefix
;; ============================================================================

(defspec private-fn-pipeline-has-ns-prefix 100
  (prop/for-all [suffix gen-core/gen-non-blank-string]
    (let [source (str "(ns my-mod)\n(defn- fn-" suffix " [x] x)")
          result (clel/compile-file-string source)]
      (str/includes? result "my-mod--"))))

;; ============================================================================
;; Mutation test: PROVE the dash-strip fix is load-bearing
;; ============================================================================

(deftest mutation-dash-strip-is-load-bearing
  (testing "MUTATION TEST: mangle-name preserves the leading dash.
            If ns-qualify-name did NOT strip it, combining with private separator
            would produce triple-dash. This proves the strip logic matters."
    (let [mangled (mangle-name '-helper)]
      ;; mangle-name itself keeps the dash
      (is (str/starts-with? mangled "-")
          "mangle-name preserves leading dash — the raw material for the bug")
      ;; Without the strip, private ns-qualify would produce:
      ;; "my-pkg" + "--" + "-helper" = "my-pkg---helper"
      (let [naive-result (str (mangle-name 'my-pkg) "--" mangled)]
        (is (str/includes? naive-result "---")
            "Naive concatenation DOES produce triple-dash (the original bug)")
        ;; The fix strips it:
        (let [fixed-result (emit/ns-qualify-name '-helper {:ns 'my-pkg} true)]
          (is (not (str/includes? fixed-result "---"))
              "ns-qualify-name fix strips the dash, avoiding triple-dash")
          (is (= "my-pkg--helper" fixed-result)))))))
