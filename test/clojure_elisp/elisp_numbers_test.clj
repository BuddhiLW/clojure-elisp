(ns clojure-elisp.elisp-numbers-test
  "Property-based and regression tests for Elisp number-symbol handling.

   The Clojure reader rejects `1+` and `1-` as invalid number literals.
   The compiler preprocesses these to safe aliases before reading and
   postprocesses the emitted Elisp to restore the original names.
   These tests verify correctness of the round-trip and guard against
   the silent-truncation regression where forms after `1+` were lost."
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [clojure-elisp.core :as clel]
            [clojure.string :as str]))

;; ============================================================================
;; Helpers — access private functions via var deref
;; ============================================================================

(def ^:private preprocess  #'clojure-elisp.core/preprocess-elisp-numbers)
(def ^:private postprocess #'clojure-elisp.core/postprocess-elisp-numbers)

;; ============================================================================
;; Generators
;; ============================================================================

(def gen-safe-symbol
  "Generator for safe Elisp symbol names (no leading digits)."
  (gen/fmap (fn [s] (str "x" s))
            (gen/not-empty gen/string-alpha-numeric)))

(def gen-one-plus-call
  "Generator for (1+ <expr>) calls."
  (gen/fmap (fn [sym] (str "(1+ " sym ")"))
            gen-safe-symbol))

(def gen-one-minus-call
  "Generator for (1- <expr>) calls."
  (gen/fmap (fn [sym] (str "(1- " sym ")"))
            gen-safe-symbol))

(def gen-one-op-call
  "Generator for either (1+ x) or (1- x)."
  (gen/one-of [gen-one-plus-call gen-one-minus-call]))

(def gen-defn-with-one-op
  "Generator for (defn <name> [<var>] (1+/1- <var>))."
  (gen/let [fn-name gen-safe-symbol
            var-name gen-safe-symbol
            op (gen/elements ["1+" "1-"])]
    (str "(defn " fn-name " [" var-name "] (" op " " var-name "))")))

(def gen-defvar-then-defn-with-one-op
  "Generator for a file with (def ...) THEN (defn ... (1+/1- ...)).
   The critical pattern — forms after the first 1+ must not be lost."
  (gen/let [var-name gen-safe-symbol
            fn-name  gen-safe-symbol
            arg      gen-safe-symbol
            op       (gen/elements ["1+" "1-"])
            n        gen/small-integer]
    (str "(def " var-name " " n ")\n"
         "(defn " fn-name " [" arg "] (" op " " arg "))")))

(def gen-nested-one-op
  "Generator for nested expressions containing 1+/1-."
  (gen/let [a gen-safe-symbol
            b gen-safe-symbol]
    (str "(if (> " a " 0) (1+ " a ") (1- " b "))")))

(def gen-multi-one-op-file
  "Generator for files with multiple 1+/1- calls."
  (gen/let [calls (gen/vector gen-one-op-call 2 5)]
    (str/join "\n" calls)))

;; ============================================================================
;; Property: preprocess -> postprocess round-trip is identity
;; ============================================================================

(defspec preprocess-postprocess-roundtrip 200
  (prop/for-all [call gen-one-op-call]
    (= call (postprocess (preprocess call)))))

(defspec preprocess-postprocess-roundtrip-multi 200
  (prop/for-all [source gen-multi-one-op-file]
    (= source (postprocess (preprocess source)))))

;; ============================================================================
;; Property: compilation preserves form count (no truncation)
;; ============================================================================

(defspec defvar-then-one-op-not-truncated 100
  (prop/for-all [source gen-defvar-then-defn-with-one-op]
    (let [result (clel/compile-file-string source)]
      ;; The output must contain BOTH a defvar and a defun.
      ;; The old bug would silently drop everything after 1+.
      (and (str/includes? result "defvar")
           (str/includes? result "defun")))))

(defspec multi-one-op-all-compile 100
  (prop/for-all [source gen-multi-one-op-file]
    (let [result (clel/compile-string source)]
      ;; Each 1+ or 1- call produces output containing that operator.
      ;; Count the number of (1+ or (1- in the output and verify
      ;; it matches the number in the input.
      (let [input-count  (count (re-seq #"\(1[+-]" source))
            output-count (count (re-seq #"\(1[+-]" result))]
        (= input-count output-count)))))

;; ============================================================================
;; Property: output contains original symbols, not aliases
;; ============================================================================

(defspec no-alias-leakage-in-output 200
  (prop/for-all [source gen-defn-with-one-op]
    (let [result (clel/compile-file-string source)]
      (and (not (str/includes? result "cljel--1plus"))
           (not (str/includes? result "cljel--1minus"))))))

;; ============================================================================
;; Property: preprocess never introduces unbalanced parens
;; ============================================================================

(defspec preprocess-preserves-paren-balance 200
  (prop/for-all [call gen-one-op-call]
    (let [preprocessed (preprocess call)
          opens  (count (filter #(= % \() preprocessed))
          closes (count (filter #(= % \)) preprocessed))]
      (= opens closes))))

;; ============================================================================
;; Property: compile-file-string on 1+/1- expressions is total (no crash)
;; ============================================================================

(defspec compile-one-op-total 200
  (prop/for-all [source gen-defn-with-one-op]
    (string? (clel/compile-file-string source))))

;; ============================================================================
;; Regression Tests — Specific Patterns
;; ============================================================================

(deftest basic-one-plus-call-test
  (testing "(1+ x) compiles to (1+ x)"
    (is (= "(1+ x)" (clel/compile-string "(1+ x)")))))

(deftest basic-one-minus-call-test
  (testing "(1- x) compiles to (1- x)"
    (is (= "(1- x)" (clel/compile-string "(1- x)")))))

(deftest setq-with-one-plus-test
  (testing "(setq var (1+ var)) compiles correctly"
    (let [result (clel/compile-string "(setq counter (1+ counter))")]
      (is (str/includes? result "setq"))
      (is (str/includes? result "(1+ counter)")))))

(deftest one-minus-in-function-call-test
  (testing "(1- (line-number-at-pos)) compiles correctly"
    (let [result (clel/compile-string "(1- (line-number-at-pos))")]
      (is (str/includes? result "(1-"))
      (is (str/includes? result "line-number-at-pos")))))

(deftest defn-with-one-plus-test
  (testing "(defn foo [] (1+ bar)) produces a defun with 1+"
    (let [result (clel/compile-file-string "(defn foo [] (1+ bar))")]
      (is (str/includes? result "defun"))
      (is (str/includes? result "(1+ bar)")))))

(deftest defvar-then-defn-with-one-plus-test
  (testing "File with defvar THEN defn using 1+ compiles ALL forms"
    (let [source "(def my-counter 0)\n(defn inc-counter [] (1+ my-counter))"
          result (clel/compile-file-string source)]
      ;; BOTH forms must appear in output — this is the core regression
      (is (str/includes? result "defvar") "defvar must be present")
      (is (str/includes? result "defun") "defun must be present")
      (is (str/includes? result "(1+ my-counter)")))))

(deftest nested-if-with-one-plus-and-one-minus-test
  (testing "Nested: (if test (1+ a) (1- b)) compiles correctly"
    (let [result (clel/compile-string "(if test (1+ a) (1- b))")]
      (is (str/includes? result "(1+ a)"))
      (is (str/includes? result "(1- b)")))))

(deftest condition-case-with-one-plus-test
  (testing "1+ inside condition-case compiles correctly"
    (let [source "(condition-case err (1+ x) (error (message \"err\")))"
          result (clel/compile-string source)]
      (is (str/includes? result "(1+"))
      (is (not (str/includes? result "cljel--1plus"))))))

(deftest multiple-one-plus-in-same-file-test
  (testing "Multiple 1+ in same file all compile"
    (let [source (str "(defn inc-a [a] (1+ a))\n"
                      "(defn inc-b [b] (1+ b))\n"
                      "(defn inc-c [c] (1+ c))")
          result (clel/compile-file-string source)]
      ;; All three defuns must be present
      (is (= 3 (count (re-seq #"defun" result)))
          "All three defuns must be emitted")
      ;; All three 1+ calls must be present
      (is (= 3 (count (re-seq #"\(1\+" result)))
          "All three (1+ calls must be in output"))))

(deftest multiple-one-minus-in-same-file-test
  (testing "Multiple 1- in same file all compile"
    (let [source (str "(defn dec-a [a] (1- a))\n"
                      "(defn dec-b [b] (1- b))")
          result (clel/compile-file-string source)]
      (is (= 2 (count (re-seq #"defun" result))))
      (is (= 2 (count (re-seq #"\(1-" result)))))))

(deftest mixed-one-plus-and-one-minus-test
  (testing "File mixing 1+ and 1- compiles all forms"
    (let [source (str "(defn go-up [n] (1+ n))\n"
                      "(defn go-down [n] (1- n))")
          result (clel/compile-file-string source)]
      (is (= 2 (count (re-seq #"defun" result))))
      (is (str/includes? result "(1+"))
      (is (str/includes? result "(1-")))))

;; ============================================================================
;; Safety Net — unhandled number-like symbols throw, not truncate
;; ============================================================================

(deftest safety-net-unknown-number-symbol-test
  (testing "Unknown number-like symbol (2+) throws an informative error"
    ;; 2+ looks like a number to the reader but is not in our alias table.
    ;; The safety net in read-all-forms should throw, not silently truncate.
    (is (thrown-with-msg?
         clojure.lang.ExceptionInfo
         #"Unhandled Elisp number symbol"
         (clel/compile-file-string "(def x 1)\n(2+ y)")))))

(deftest safety-net-three-minus-test
  (testing "Unknown number-like symbol (3-) throws an informative error"
    (is (thrown-with-msg?
         clojure.lang.ExceptionInfo
         #"Unhandled Elisp number symbol"
         (clel/compile-file-string "(3- z)")))))

(deftest safety-net-preserves-preceding-forms-context-test
  (testing "Safety net throws even when valid forms precede the bad symbol"
    ;; Ensures the error is raised instead of silently returning partial output
    (is (thrown-with-msg?
         clojure.lang.ExceptionInfo
         #"Unhandled Elisp number symbol"
         (clel/compile-file-string "(def a 1)\n(def b 2)\n(2+ c)")))))

;; ============================================================================
;; No-Alias-Leakage — deterministic checks
;; ============================================================================

(deftest no-alias-leakage-compile-string-test
  (testing "compile-string output never contains internal aliases"
    (let [result (clel/compile-string "(1+ x)")]
      (is (not (str/includes? result "cljel--1plus")))
      (is (not (str/includes? result "cljel--1minus"))))))

(deftest no-alias-leakage-compile-file-string-test
  (testing "compile-file-string output never contains internal aliases"
    (let [source "(defn foo [x] (1+ x))\n(defn bar [y] (1- y))"
          result (clel/compile-file-string source)]
      (is (not (str/includes? result "cljel--1plus")))
      (is (not (str/includes? result "cljel--1minus"))))))

;; ============================================================================
;; Edge Cases
;; ============================================================================

(deftest bare-number-symbol-in-non-call-position-test
  (testing "Bare 1+ not in call position triggers safety net (not silently lost)"
    ;; When 1+ appears as a bare symbol (not preceded by open-paren),
    ;; preprocessing does NOT alias it, so the reader hits NumberFormatException.
    ;; The safety net should catch this and throw an informative error.
    (is (thrown-with-msg?
         clojure.lang.ExceptionInfo
         #"Unhandled Elisp number symbol"
         (clel/compile-file-string "(map 1+ items)")))))

(deftest preprocess-idempotent-on-clean-source-test
  (testing "Preprocessing source without 1+/1- is identity"
    (let [source "(defn foo [x] (+ x 1))"]
      (is (= source (preprocess source))))))

(deftest postprocess-idempotent-on-clean-output-test
  (testing "Postprocessing output without aliases is identity"
    (let [output "(defun foo (x) (+ x 1))"]
      (is (= output (postprocess output))))))

(deftest empty-string-roundtrip-test
  (testing "Empty string passes through pre/post unchanged"
    (is (= "" (preprocess "")))
    (is (= "" (postprocess "")))))
