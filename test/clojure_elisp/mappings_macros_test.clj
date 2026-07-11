(ns clojure-elisp.mappings-macros-test
  "Schema + invariant tests for the Clojure→Elisp mapping tables and the
   compile-time macro registry.

   Verifies that the Malli schemas ACCEPT the real production data (no false
   rejection), that the collision guard both stays green on the real tables and
   actually fires on a synthetic conflict, and that the macro-registry schema
   and contracts describe the live registry."
  (:require [clojure.test :refer [deftest is testing]]
            [malli.core :as m]
            [malli.instrument :as mi]
            [clojure-elisp.mappings :as mappings]
            [clojure-elisp.macros :as macros]))

;; ============================================================================
;; MappingTable — accepts every real table
;; ============================================================================

(deftest mapping-table-accepts-real-tables
  (testing "every source table conforms to MappingTable"
    (doseq [[table-name table] mappings/mapping-tables]
      (is (m/validate mappings/MappingTable table)
          (str table-name " should conform to MappingTable: "
               (m/explain mappings/MappingTable table)))))
  (testing "the merged core-fn-mapping conforms to MappingTable"
    (is (m/validate mappings/MappingTable mappings/core-fn-mapping))))

(deftest mapping-table-rejects-bad-data
  (testing "non-symbol keys are rejected"
    (is (not (m/validate mappings/MappingTable {"str-key" "value"}))))
  (testing "blank / whitespace-containing values are rejected"
    (is (not (m/validate mappings/MappingTable {'x ""})))
    (is (not (m/validate mappings/MappingTable {'x "two tokens"})))
    (is (not (m/validate mappings/MappingTable {'x "trailing "}))))
  (testing "a real single-token value is accepted"
    (is (m/validate mappings/MappingTable {'x "clel-thing"}))))

;; ============================================================================
;; Collision guard
;; ============================================================================

(deftest no-real-collisions
  (testing "the real tables have no conflicting keys"
    (is (= {} (mappings/table-collisions))))
  (testing "validate-tables! returns true on the real registry"
    (is (true? (mappings/validate-tables!)))))

(deftest collision-guard-fires-on-conflict
  (testing "table-collisions reports a key two tables map to different tokens"
    (with-redefs [mappings/mapping-tables {"a" {'dup "one"}
                                           "b" {'dup "two"}
                                           "c" {'ok "fine"}}]
      (let [collisions (mappings/table-collisions)]
        (is (contains? collisions 'dup))
        (is (not (contains? collisions 'ok))))))
  (testing "same key mapped to the SAME token is not a collision"
    (with-redefs [mappings/mapping-tables {"a" {'same "v"}
                                           "b" {'same "v"}}]
      (is (= {} (mappings/table-collisions)))))
  (testing "validate-tables! throws on a conflicting registry"
    (with-redefs [mappings/mapping-tables {"a" {'dup "one"}
                                           "b" {'dup "two"}}]
      (is (thrown? clojure.lang.ExceptionInfo (mappings/validate-tables!))))))

;; ============================================================================
;; ArgSlots — higher-order-fn-arg-slots
;; ============================================================================

(deftest arg-slots-accepts-real-table
  (testing "higher-order-fn-arg-slots conforms to ArgSlots"
    (is (m/validate mappings/ArgSlots mappings/higher-order-fn-arg-slots)
        (m/explain mappings/ArgSlots mappings/higher-order-fn-arg-slots))))

(deftest arg-slots-rejects-bad-data
  (testing "a non-set / non-:all value is rejected"
    (is (not (m/validate mappings/ArgSlots {'f 0})))
    (is (not (m/validate mappings/ArgSlots {'f #{-1}})))
    (is (not (m/validate mappings/ArgSlots {'f :some}))))
  (testing ":all and sets of nat-ints are accepted"
    (is (m/validate mappings/ArgSlots {'f :all}))
    (is (m/validate mappings/ArgSlots {'f #{0 2}}))))

;; ============================================================================
;; MacroRegistry + contracts
;; ============================================================================

(deftest macro-registry-schema
  (testing "an empty registry conforms"
    (is (m/validate macros/MacroRegistry {})))
  (testing "the live registry conforms"
    (is (m/validate macros/MacroRegistry @macros/macro-registry))
    (is (m/validate macros/MacroRegistry @macros/builtin-macros)))
  (testing "a populated registry conforms"
    (is (m/validate macros/MacroRegistry {'when-not (fn [& _] nil)})))
  (testing "non-ifn values are rejected"
    (is (not (m/validate macros/MacroRegistry {'when-not "not-a-fn"})))))

(deftest macro-contracts-registered
  (testing "m/=> contracts are registered for the macro registry fns"
    (let [schemas (get (m/function-schemas) 'clojure-elisp.macros)]
      (is (contains? schemas 'get-macro))
      (is (contains? schemas 'register-macro!))
      (is (contains? schemas 'register-builtin-macro!)))))

(deftest macro-contracts-enforced-under-instrumentation
  (testing "instrumented macro fns accept valid calls and reject bad input"
    (try
      (mi/instrument! {:filters [(mi/-filter-ns 'clojure-elisp.macros)]})
      ;; valid: symbol + ifn
      (is (nil? (macros/get-macro 'definitely-absent-macro)))
      (let [f (fn [& body] (cons 'do body))]
        (is (m/validate macros/MacroRegistry (macros/register-macro! 'tmp-macro f)))
        (is (ifn? (macros/get-macro 'tmp-macro))))
      ;; invalid input: non-symbol key violates the [:cat :symbol ...] contract
      (is (thrown? Exception (macros/get-macro "not-a-symbol")))
      (is (thrown? Exception (macros/register-macro! "not-a-symbol" (fn [& _] nil))))
      (finally
        (mi/unstrument! {:filters [(mi/-filter-ns 'clojure-elisp.macros)]})
        ;; leave the registry as tests found it
        (swap! macros/macro-registry dissoc 'tmp-macro)))))
