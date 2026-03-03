(ns clojure-elisp.elisp-char-literals-test
  "Tests for Elisp character literal and string escape preprocessing.

   The Clojure reader rejects Elisp char literals (`?\\s`, `?\\033`, `?a`)
   and string escapes (`\\e`, `\\a`, `\\0NNN`). The compiler preprocesses
   these before reading. These tests verify correctness of the translation."
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [clojure-elisp.core :as clel]
            [clojure.string :as str]))

;; ============================================================================
;; Helpers — access private functions via var deref
;; ============================================================================

(def ^:private preprocess-chars  #'clojure-elisp.core/preprocess-elisp-char-literals)
(def ^:private preprocess-strings #'clojure-elisp.core/preprocess-elisp-string-escapes)

;; ============================================================================
;; Unit Tests — Named Character Literals
;; ============================================================================

(deftest named-char-literal-test
  (testing "?\\s → 32 (space)"
    (is (= "32" (preprocess-chars "?\\s"))))
  (testing "?\\e → 27 (escape)"
    (is (= "27" (preprocess-chars "?\\e"))))
  (testing "?\\n → 10 (newline)"
    (is (= "10" (preprocess-chars "?\\n"))))
  (testing "?\\t → 9 (tab)"
    (is (= "9" (preprocess-chars "?\\t"))))
  (testing "?\\r → 13 (carriage return)"
    (is (= "13" (preprocess-chars "?\\r"))))
  (testing "?\\a → 7 (bell)"
    (is (= "7" (preprocess-chars "?\\a"))))
  (testing "?\\b → 8 (backspace)"
    (is (= "8" (preprocess-chars "?\\b"))))
  (testing "?\\f → 12 (form feed)"
    (is (= "12" (preprocess-chars "?\\f"))))
  (testing "?\\d → 127 (delete)"
    (is (= "127" (preprocess-chars "?\\d"))))
  (testing "?\\\\ → 92 (backslash)"
    (is (= "92" (preprocess-chars "?\\\\")))))

;; ============================================================================
;; Unit Tests — Octal Character Literals
;; ============================================================================

(deftest octal-char-literal-test
  (testing "?\\033 → 27 (ESC via octal)"
    (is (= "27" (preprocess-chars "?\\033"))))
  (testing "?\\040 → 32 (space via octal)"
    (is (= "32" (preprocess-chars "?\\040"))))
  (testing "?\\012 → 10 (newline via octal)"
    (is (= "10" (preprocess-chars "?\\012"))))
  (testing "?\\177 → 127 (DEL via octal)"
    (is (= "127" (preprocess-chars "?\\177")))))

;; ============================================================================
;; Unit Tests — Hex Character Literals
;; ============================================================================

(deftest hex-char-literal-test
  (testing "?\\x1b → 27 (ESC via hex)"
    (is (= "27" (preprocess-chars "?\\x1b"))))
  (testing "?\\x20 → 32 (space via hex)"
    (is (= "32" (preprocess-chars "?\\x20"))))
  (testing "?\\x0a → 10 (newline via hex)"
    (is (= "10" (preprocess-chars "?\\x0a"))))
  (testing "?\\xFF → 255 (max byte via hex)"
    (is (= "255" (preprocess-chars "?\\xFF")))))

;; ============================================================================
;; Unit Tests — Plain Character Literals
;; ============================================================================

(deftest plain-char-literal-test
  (testing "?a → 97"
    (is (= "97" (preprocess-chars "?a"))))
  (testing "?Z → 90"
    (is (= "90" (preprocess-chars "?Z"))))
  (testing "?0 → 48"
    (is (= "48" (preprocess-chars "?0"))))
  (testing "?! → 33"
    (is (= "33" (preprocess-chars "?!"))))
  (testing "?? → 63 (question mark)"
    (is (= "63" (preprocess-chars "??"))))
  (testing "?( → 40 (open paren)"
    (is (= "40" (preprocess-chars "?("))))
  (testing "?) → 41 (close paren)"
    (is (= "41" (preprocess-chars "?)")))))

;; ============================================================================
;; Unit Tests — Character Literals in Context
;; ============================================================================

(deftest char-literal-in-context-test
  (testing "char literal in function call"
    (is (= "(make-string 5 32)"
           (preprocess-chars "(make-string 5 ?\\s)"))))
  (testing "char literal as argument"
    (is (= "(cl-count 27 input)"
           (preprocess-chars "(cl-count ?\\033 input)"))))
  (testing "multiple char literals"
    (is (= "(list 32 9 10)"
           (preprocess-chars "(list ?\\s ?\\t ?\\n)"))))
  (testing "char literal in let binding"
    (is (= "(let ((ch 32)) ch)"
           (preprocess-chars "(let ((ch ?\\s)) ch)"))))
  (testing "plain char in comparison"
    (is (= "(= 97 c)"
           (preprocess-chars "(= ?a c)")))))

;; ============================================================================
;; Unit Tests — String Escape Preprocessing
;; ============================================================================

(deftest string-escape-test
  (testing "\\e in string → \\u001b"
    (is (= "\"\\u001b\"" (preprocess-strings "\"\\e\""))))
  (testing "\\a in string → \\u0007"
    (is (= "\"\\u0007\"" (preprocess-strings "\"\\a\""))))
  (testing "\\033 in string → \\u001b"
    (is (= "\"\\u001b\"" (preprocess-strings "\"\\033\""))))
  (testing "\\040 in string → \\u0020"
    (is (= "\"\\u0020\"" (preprocess-strings "\"\\040\""))))
  (testing "mixed escapes in one string"
    (is (= "\"hello\\u001bworld\\u0007\""
           (preprocess-strings "\"hello\\eworld\\a\""))))
  (testing "regular escapes pass through"
    (is (= "\"hello\\nworld\"" (preprocess-strings "\"hello\\nworld\""))))
  (testing "\\e outside string is not transformed"
    (is (= "\\e \"\\u001b\"" (preprocess-strings "\\e \"\\e\"")))))

;; ============================================================================
;; Integration Tests — Full Compilation Pipeline
;; ============================================================================

(deftest char-literal-compilation-test
  (testing "(make-string 5 ?\\s) compiles correctly"
    (let [result (clel/compile-string "(make-string 5 ?\\s)")]
      (is (str/includes? result "make-string"))
      (is (str/includes? result "32"))))
  (testing "char literal in defn compiles"
    (let [result (clel/compile-file-string
                  "(defn space-char [] ?\\s)")]
      (is (str/includes? result "defun"))
      (is (str/includes? result "32"))))
  (testing "octal char literal compiles"
    (let [result (clel/compile-string "(cl-count ?\\033 input)")]
      (is (str/includes? result "27")))))

(deftest string-escape-compilation-test
  (testing "string with \\e compiles"
    (let [result (clel/compile-string "(message \"ESC: \\e\")")]
      (is (string? result))
      (is (str/includes? result "message"))))
  (testing "string with \\a compiles"
    (let [result (clel/compile-string "(message \"BEL: \\a\")")]
      (is (string? result)))))

;; ============================================================================
;; Edge Cases
;; ============================================================================

(deftest char-literal-edge-cases-test
  (testing "char literals inside strings are NOT transformed"
    ;; The string "?\\s" should remain a literal string, not become "32"
    (let [result (preprocess-chars "\"?\\\\s\"")]
      (is (= "\"?\\\\s\"" result))))
  (testing "preprocessing source without char literals is identity"
    (let [source "(defn foo [x] (+ x 1))"]
      (is (= source (preprocess-chars source)))))
  (testing "empty string passes through"
    (is (= "" (preprocess-chars ""))))
  (testing "char literal at end of string"
    (is (= "(list 32)" (preprocess-chars "(list ?\\s)")))))

(deftest string-escape-edge-cases-test
  (testing "empty string literal passes through"
    (is (= "\"\"" (preprocess-strings "\"\""))))
  (testing "escaped backslash before e is not \\e"
    ;; "\\\\e" in source is a backslash followed by e, not \\e escape
    (is (= "\"\\\\e\"" (preprocess-strings "\"\\\\e\""))))
  (testing "no strings in input passes through"
    (is (= "(+ 1 2)" (preprocess-strings "(+ 1 2)")))))

;; ============================================================================
;; Property Tests
;; ============================================================================

(def gen-safe-source
  "Generator for source code without char literals (identity through preprocessing)."
  (gen/fmap (fn [syms] (str "(+ " (str/join " " syms) ")"))
            (gen/vector (gen/fmap #(str "x" %) gen/string-alpha-numeric) 1 5)))

(defspec preprocess-char-literals-is-total 200
  (prop/for-all [s gen/string]
    ;; preprocess-chars should never throw, regardless of input
                (string? (preprocess-chars s))))

(defspec preprocess-string-escapes-is-total 200
  (prop/for-all [s gen/string]
    ;; preprocess-strings should never throw, regardless of input
                (string? (preprocess-strings s))))

(defspec roundtrip-identity-on-safe-source 100
  (prop/for-all [source gen-safe-source]
    ;; Source without char literals should pass through unchanged
                (= source (preprocess-chars source))))

(defspec no-placeholder-leakage 100
  (prop/for-all [source gen-safe-source]
    ;; Output should never contain internal placeholders
                (let [result (preprocess-chars source)]
                  (and (not (str/includes? result "CLJEL_CHAR_"))
                       (not (str/includes? result "CLJEL_ESC_"))))))
