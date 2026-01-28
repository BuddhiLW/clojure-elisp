(ns clojure-elisp.core-test
  "Integration tests for ClojureElisp core API."
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-elisp.core :as clel]
            [clojure-elisp.analyzer :as ana]
            [clojure.java.io :as io]
            [clojure.string :as str]))

;; ============================================================================
;; emit - Single Form Compilation
;; ============================================================================

(deftest emit-test
  (testing "emit compiles a single form"
    (is (string? (clel/emit '(+ 1 2)))))
  (testing "emit defn produces valid elisp"
    (let [result (clel/emit '(defn foo [x] (+ x 1)))]
      (is (str/includes? result "defun"))
      (is (str/includes? result "foo"))))
  (testing "emit let produces let*"
    (let [result (clel/emit '(let [a 1] a))]
      (is (str/includes? result "let*")))))

;; ============================================================================
;; emit-forms - Multiple Form Compilation
;; ============================================================================

(deftest emit-forms-test
  (testing "emit-forms compiles multiple forms"
    (let [result (clel/emit-forms ['(def x 1)
                                   '(def y 2)
                                   '(defn add [a b] (+ a b))])]
      (is (string? result))
      (is (str/includes? result "defvar"))
      (is (str/includes? result "defun"))))
  (testing "forms are separated by newlines"
    (let [result (clel/emit-forms ['(def a 1) '(def b 2)])]
      (is (str/includes? result "\n")))))

;; ============================================================================
;; compile-string - String Compilation
;; ============================================================================

(deftest compile-string-test
  (testing "compile-string handles single form"
    (let [result (clel/compile-string "(+ 1 2)")]
      (is (= "(+ 1 2)" result))))
  (testing "compile-string handles multiple forms"
    (let [result (clel/compile-string "(def x 1) (def y 2)")]
      (is (str/includes? result "defvar"))
      (is (str/includes? result "x"))
      (is (str/includes? result "y"))))
  (testing "compile-string handles defn"
    (let [result (clel/compile-string "(defn greet [name] (str \"Hello, \" name))")]
      (is (str/includes? result "defun"))
      (is (str/includes? result "greet"))))
  (testing "compile-string handles complex forms"
    (let [result (clel/compile-string "(defn factorial [n]
                                         (if (<= n 1)
                                           1
                                           (* n (factorial (dec n)))))")]
      (is (str/includes? result "defun"))
      (is (str/includes? result "factorial")))))

;; ============================================================================
;; Elisp Syntax Validity
;; ============================================================================

(deftest elisp-validity-test
  (testing "parentheses are balanced"
    (let [forms ["(defn foo [x] x)"
                 "(let [a 1 b 2] (+ a b))"
                 "(if (> x 0) 1 0)"
                 "(cond true 1 false 2 :else 3)"
                 "(do (println \"hi\") (+ 1 2))"]]
      (doseq [form forms]
        (let [result (clel/compile-string form)
              opens (count (filter #(= % \() result))
              closes (count (filter #(= % \)) result))]
          (is (= opens closes)
              (str "Unbalanced parens for: " form))))))
  (testing "no nil pollution in output"
    ;; Check that "nil nil" doesn't appear (which would indicate emitter bugs)
    (let [result (clel/compile-string "(defn foo [x] (+ x 1))")]
      (is (not (str/includes? result "nil nil"))))))

;; ============================================================================
;; Common Clojure Patterns
;; ============================================================================

(deftest common-patterns-test
  (testing "threading macro ->"
    (let [result (clel/compile-string "(-> x inc (* 2))")]
      ;; After macro expansion, should produce valid elisp
      (is (string? result))))
  (testing "threading macro ->>"
    (let [result (clel/compile-string "(->> xs (map inc) (filter even?))")]
      (is (string? result))))
  (testing "let with multiple bindings"
    (let [result (clel/compile-string "(let [a 1 b (+ a 1) c (+ b 1)] c)")]
      (is (str/includes? result "let*"))
      (is (str/includes? result "(a 1)"))))
  (testing "nested if"
    (let [result (clel/compile-string "(if true (if false 1 2) 3)")]
      (is (string? result))))
  (testing "cond with multiple clauses"
    (let [result (clel/compile-string "(cond (> x 10) :large
                                            (> x 5) :medium
                                            (> x 0) :small
                                            :else :zero)")]
      (is (str/includes? result "cond")))))

;; ============================================================================
;; Edge Cases
;; ============================================================================

(deftest edge-cases-test
  (testing "empty vector"
    (let [result (clel/compile-string "[]")]
      (is (str/includes? result "list"))))
  (testing "empty map"
    (let [result (clel/compile-string "{}")]
      (is (string? result))))
  (testing "nested collections"
    (let [result (clel/compile-string "[[1 2] [3 4]]")]
      (is (string? result))))
  (testing "keywords"
    (let [result (clel/compile-string ":foo")]
      (is (= ":foo" result))))
  (testing "strings with special characters"
    (let [result (clel/compile-string "\"hello\\nworld\"")]
      (is (str/includes? result "hello"))))
  (testing "nil"
    (is (= "nil" (clel/compile-string "nil"))))
  (testing "booleans"
    (is (= "t" (clel/compile-string "true")))
    (is (= "nil" (clel/compile-string "false")))))

;; ============================================================================
;; Core Function Mapping Integration
;; ============================================================================

(deftest core-function-integration-test
  (testing "first -> clel-first"
    (is (= "(clel-first xs)" (clel/compile-string "(first xs)"))))
  (testing "rest -> clel-rest"
    (is (= "(clel-rest xs)" (clel/compile-string "(rest xs)"))))
  (testing "inc -> 1+"
    (is (= "(1+ x)" (clel/compile-string "(inc x)"))))
  (testing "nil? -> null"
    (is (= "(null x)" (clel/compile-string "(nil? x)"))))
  (testing "count -> length"
    (is (= "(length xs)" (clel/compile-string "(count xs)"))))
  (testing "str -> clel-str"
    (is (= "(clel-str a b c)" (clel/compile-string "(str a b c)")))))

;; ============================================================================
;; Complete Program Compilation
;; ============================================================================

(deftest complete-program-test
  (testing "simple program with ns, def, and defn"
    (let [result (clel/compile-string
                  "(ns my.utils)
                    (def pi 3.14159)
                    (defn circle-area [r] (* pi (* r r)))")]
      (is (str/includes? result "my-utils"))
      (is (str/includes? result "pi"))
      (is (str/includes? result "circle-area"))))
  (testing "program with higher-order functions"
    (let [result (clel/compile-string
                  "(defn process [items]
                      (map inc (filter even? items)))")]
      (is (str/includes? result "defun"))
      (is (str/includes? result "clel-map")))))

;; ============================================================================
;; loop/recur Integration
;; ============================================================================

(deftest loop-recur-integration-test
  (testing "simple loop/recur"
    (let [result (clel/compile-string
                  "(loop [i 0 acc 0]
                      (if (>= i 10)
                        acc
                        (recur (inc i) (+ acc i))))")]
      (is (str/includes? result "cl-labels"))
      (is (str/includes? result "recur"))))
  (testing "factorial with recur"
    (let [result (clel/compile-string
                  "(defn factorial [n]
                      (loop [n n acc 1]
                        (if (<= n 1)
                          acc
                          (recur (dec n) (* acc n)))))")]
      (is (str/includes? result "defun"))
      (is (str/includes? result "cl-labels")))))

;; ============================================================================
;; Error Handling (when applicable)
;; ============================================================================

(deftest malformed-input-test
  (testing "empty string produces empty output"
    ;; This tests the compile-string with no forms
    ;; An empty vector read from [] should produce empty join
    (let [result (clel/compile-string "")]
      (is (string? result)))))

;; ============================================================================
;; File Compilation (compile-file)
;; ============================================================================

(deftest compile-file-test
  (testing "compile-file creates output file"
    (let [input-file (java.io.File/createTempFile "test" ".cljel")
          output-file (java.io.File/createTempFile "test" ".el")]
      (try
        ;; Write test input
        (spit input-file "(defn hello [name] (str \"Hello, \" name))")
        ;; Compile
        (let [result (clel/compile-file (.getAbsolutePath input-file)
                                        (.getAbsolutePath output-file))]
          ;; Check result map
          (is (map? result))
          (is (contains? result :input))
          (is (contains? result :output))
          (is (contains? result :size))
          (is (pos? (:size result)))
          ;; Check output file was created
          (is (.exists output-file))
          ;; Check content
          (let [content (slurp output-file)]
            (is (str/includes? content "defun"))
            (is (str/includes? content "hello"))))
        (finally
          ;; Cleanup
          (.delete input-file)
          (.delete output-file))))))

;; ============================================================================
;; Documentation Generation
;; ============================================================================

(deftest docstring-preservation-test
  (testing "defn docstrings are preserved"
    (let [result (clel/compile-string
                  "(defn greet \"Greets a person by name.\" [name]
                      (str \"Hello, \" name))")]
      (is (str/includes? result "Greets a person by name"))))
  (testing "def docstrings are preserved"
    (let [result (clel/compile-string
                  "(def answer \"The answer to everything\" 42)")]
      (is (str/includes? result "The answer to everything")))))

;; ============================================================================
;; Name Mangling in Context
;; ============================================================================

(deftest name-mangling-integration-test
  (testing "predicate function names are mangled"
    (let [result (clel/compile-string "(defn valid? [x] (not (nil? x)))")]
      (is (str/includes? result "valid-p"))))
  (testing "bang function names are mangled"
    (let [result (clel/compile-string "(defn reset! [atom val] val)")]
      (is (str/includes? result "reset-bang"))))
  (testing "namespaced symbols are mangled"
    (let [result (clel/compile-string "(ns my.cool.package)")]
      (is (str/includes? result "my-cool-package")))))

;; ============================================================================
;; Anonymous Functions
;; ============================================================================

(deftest anonymous-function-test
  (testing "fn creates lambda"
    (let [result (clel/compile-string "(fn [x] (* x 2))")]
      (is (str/includes? result "lambda"))))
  (testing "fn passed to higher-order function"
    (let [result (clel/compile-string "(map (fn [x] (* x 2)) items)")]
      (is (str/includes? result "clel-map"))
      (is (str/includes? result "lambda")))))

;; ============================================================================
;; Macro System - End-to-End (clel-027)
;; ============================================================================

(deftest macro-compile-string-test
  (testing "defmacro + usage compiles to expanded Elisp"
    (ana/clear-macros!)
    (let [result (clel/compile-string
                  "(defmacro unless [pred body]
                      (list 'if (list 'not pred) body nil))
                    (unless false 42)")]
      ;; defmacro itself should not emit Elisp code
      ;; The usage expands to (if (not false) 42 nil)
      ;; Emitter optimizes (if test then nil) -> (when test then)
      (is (str/includes? result "when"))
      (is (str/includes? result "not"))
      (is (not (str/includes? result "defmacro")))
      (is (not (str/includes? result "unless")))))

  (testing "variadic macro with & body"
    (ana/clear-macros!)
    (let [result (clel/compile-string
                  "(defmacro my-when [test & body]
                      (list 'if test (cons 'do body) nil))
                    (my-when true 1 2 3)")]
      ;; Emitter optimizes (if test then nil) -> (when test then)
      (is (str/includes? result "when"))
      (is (str/includes? result "progn"))
      (is (not (str/includes? result "my-when")))))

  (testing "macro with syntax-quote produces valid Elisp"
    (ana/clear-macros!)
    (let [result (clel/compile-string
                  "(defmacro with-logging [expr]
                      `(do (println \"executing\") ~expr))
                    (with-logging (+ 1 2))")]
      (is (str/includes? result "progn"))
      (is (str/includes? result "message"))
      (is (not (str/includes? result "with-logging")))))

  (testing "multiple macros defined and used"
    (ana/clear-macros!)
    (let [result (clel/compile-string
                  "(defmacro double-it [x] (list '* 2 x))
                    (defmacro triple-it [x] (list '* 3 x))
                    (+ (double-it 5) (triple-it 10))")]
      (is (str/includes? result "(* 2 5)"))
      (is (str/includes? result "(* 3 10)"))
      (is (not (str/includes? result "double-it")))
      (is (not (str/includes? result "triple-it")))))

  (testing "macro alongside regular defn"
    (ana/clear-macros!)
    (let [result (clel/compile-string
                  "(defmacro unless [pred body]
                      (list 'if (list 'not pred) body nil))
                    (defn safe-div [a b]
                      (unless (zero? b) (/ a b)))")]
      ;; defn should emit normally
      (is (str/includes? result "defun"))
      (is (str/includes? result "safe-div"))
      ;; macro body should be expanded in defn
      (is (not (str/includes? result "unless"))))))

;; ============================================================================
;; Namespace System - Topological Sort (clel-028)
;; ============================================================================

(deftest topological-sort-test
  (testing "linear dependency chain"
    (let [graph {'a #{'b} 'b #{'c} 'c #{}}
          order (clel/topological-sort graph)]
      (is (= ['c 'b 'a] order))))

  (testing "diamond dependency"
    (let [graph {'a #{'b 'c} 'b #{'d} 'c #{'d} 'd #{}}
          order (clel/topological-sort graph)]
      ;; d must come before b and c, both before a
      (is (< (.indexOf order 'd) (.indexOf order 'b)))
      (is (< (.indexOf order 'd) (.indexOf order 'c)))
      (is (< (.indexOf order 'b) (.indexOf order 'a)))
      (is (< (.indexOf order 'c) (.indexOf order 'a)))))

  (testing "no dependencies"
    (let [graph {'a #{} 'b #{} 'c #{}}
          order (clel/topological-sort graph)]
      (is (= 3 (count order)))
      (is (= #{'a 'b 'c} (set order)))))

  (testing "single node"
    (let [graph {'a #{}}
          order (clel/topological-sort graph)]
      (is (= ['a] order)))))

(deftest circular-dependency-detection-test
  (testing "direct circular dependency throws"
    (is (thrown-with-msg? Exception #"[Cc]ircular"
                          (clel/topological-sort {'a #{'b} 'b #{'a}}))))

  (testing "indirect circular dependency throws"
    (is (thrown-with-msg? Exception #"[Cc]ircular"
                          (clel/topological-sort {'a #{'b} 'b #{'c} 'c #{'a}})))))

;; ============================================================================
;; Namespace System - compile-file-string (clel-028)
;; ============================================================================

(deftest compile-file-string-ns-test
  (testing "compile-file-string with ns resolves aliases"
    (let [result (clel/compile-file-string
                  "(ns my.app (:require [clojure.string :as str]))
                    (defn greet [name] (str/join \", \" name))")]
      ;; Should have ns header
      (is (str/includes? result "my-app"))
      ;; defn should be prefixed
      (is (str/includes? result "my-app-greet"))
      ;; str/join should resolve
      (is (str/includes? result "clojure-string-join"))
      ;; provide at end
      (is (str/includes? result "(provide 'my-app)"))))

  (testing "compile-file-string with refers"
    (let [result (clel/compile-file-string
                  "(ns my.app (:require [my.utils :refer [helper]]))
                    (helper 42)")]
      (is (str/includes? result "my-utils-helper"))))

  (testing "compile-file-string without ns works normally"
    (let [result (clel/compile-file-string "(defn foo [x] x)")]
      (is (str/includes? result "defun"))
      (is (str/includes? result "foo")))))
