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
              opens  (count (filter #(= % \() result))
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
    (let [input-file  (java.io.File/createTempFile "test" ".cljel")
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
  (testing "defmacro + usage compiles to Elisp with defmacro definition"
    (ana/clear-macros!)
    (let [result (clel/compile-string
                  "(defmacro unless [pred body]
                      (list 'if (list 'not pred) body nil))
                    (unless false 42)")]
      ;; defmacro now emits the macro definition to output
      (is (str/includes? result "defmacro"))
      (is (str/includes? result "unless"))))

  (testing "variadic macro with & body"
    (ana/clear-macros!)
    (let [result (clel/compile-string
                  "(defmacro my-when [test & body]
                      (list 'if test (cons 'do body) nil))
                    (my-when true 1 2 3)")]
      ;; defmacro emits the definition; usage is still expanded
      (is (str/includes? result "defmacro"))
      (is (str/includes? result "my-when"))
      ;; The expanded usage should contain the progn body
      (is (str/includes? result "progn"))))

  (testing "macro with syntax-quote produces valid Elisp"
    (ana/clear-macros!)
    (let [result (clel/compile-string
                  "(defmacro with-logging [expr]
                      `(do (println \"executing\") ~expr))
                    (with-logging (+ 1 2))")]
      ;; defmacro emits the definition
      (is (str/includes? result "defmacro"))
      (is (str/includes? result "with-logging"))
      ;; The expanded usage should contain progn + message
      (is (str/includes? result "progn"))
      (is (str/includes? result "message"))))

  (testing "multiple macros defined and used"
    (ana/clear-macros!)
    (let [result (clel/compile-string
                  "(defmacro double-it [x] (list '* 2 x))
                    (defmacro triple-it [x] (list '* 3 x))
                    (+ (double-it 5) (triple-it 10))")]
      ;; defmacro definitions emitted
      (is (str/includes? result "defmacro"))
      ;; Expanded usage still works
      (is (str/includes? result "(* 2 5)"))
      (is (str/includes? result "(* 3 10)"))))

  (testing "macro alongside regular defn"
    (ana/clear-macros!)
    (let [result (clel/compile-string
                  "(defmacro unless [pred body]
                      (list 'if (list 'not pred) body nil))
                    (defn safe-div [a b]
                      (unless (zero? b) (/ a b)))")]
      ;; defmacro definition is emitted
      (is (str/includes? result "defmacro"))
      ;; defn should emit normally
      (is (str/includes? result "defun"))
      (is (str/includes? result "safe-div")))))

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
      ;; str/join should resolve to clel-str-join via core-fn-mapping
      (is (str/includes? result "clel-str-join"))
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

;; ============================================================================
;; Source Location Tracking (clel-020)
;; ============================================================================

(deftest source-location-file-reading-test
  (testing "read-all-forms from string preserves line numbers"
    ;; Forms read from a string should have :line metadata
    (let [source "(defn foo [x] x)\n(defn bar [y] y)"
          ;; Use internal function via compile-file-string which uses read-all-forms
          ;; We can test by compiling with source comments enabled
          result (binding [clojure-elisp.emitter/*emit-source-comments* true]
                   (clel/compile-file-string source))]
      ;; First form should be at line 1
      (is (str/includes? result ";;; L1"))
      ;; Second form should be at line 2
      (is (str/includes? result ";;; L2"))))

  (testing "multiline forms have correct starting line"
    (let [source "(defn multiline
                    [x y]
                    (+ x y))
                  (def after 42)"
          result (binding [clojure-elisp.emitter/*emit-source-comments* true]
                   (clel/compile-file-string source))]
      ;; defn starts at line 1
      (is (str/includes? result ";;; L1"))
      ;; def after is on line 4 (after the multiline defn)
      (is (str/includes? result ";;; L4")))))

(deftest compile-file-source-location-test
  (testing "compile-file preserves source locations"
    (let [input-file  (java.io.File/createTempFile "test-src" ".cljel")
          output-file (java.io.File/createTempFile "test-out" ".el")]
      (try
        ;; Write multiline source
        (spit input-file "(ns my.test)\n\n(defn foo [x]\n  (+ x 1))\n\n(def bar 42)")
        ;; Compile with source comments
        (binding [clojure-elisp.emitter/*emit-source-comments* true]
          (clel/compile-file (.getAbsolutePath input-file)
                             (.getAbsolutePath output-file)))
        (let [content (slurp output-file)]
          ;; ns at line 1
          (is (str/includes? content ";;; L1"))
          ;; defn at line 3
          (is (str/includes? content ";;; L3"))
          ;; def at line 6
          (is (str/includes? content ";;; L6")))
        (finally
          (.delete input-file)
          (.delete output-file))))))

(deftest source-location-propagation-test
  (testing "nested forms preserve their own locations in AST"
    (let [source "(let [x (+ 1 2)] x)"
          rdr    (clojure.lang.LineNumberingPushbackReader.
                  (java.io.StringReader. source))
          form   (read rdr)
          ast    (ana/analyze form)]
      ;; Top-level let should have line 1
      (is (= 1 (:line ast)))
      ;; The init expression (+ 1 2) inherits context since symbols don't have metadata
      (is (some? (:line (:init (first (:bindings ast)))))))))

(deftest source-location-error-context-test
  (testing "analysis-error includes source location"
    (let [err (ana/analysis-error "Test error" {:foo 1})]
      ;; Without source context, error should not have location prefix
      (is (= "Test error" (.getMessage err)))))

  (testing "analysis-error with source context includes location"
    (binding [ana/*source-context* {:line 10 :column 5}]
      (let [err (ana/analysis-error "Test error" {:foo 1})]
        (is (str/includes? (.getMessage err) "10:5"))))))

;; ============================================================================
;; extract-ns-name & ns-derived-output-name
;; ============================================================================

(deftest extract-ns-name-test
  (testing "extracts ns name from source"
    (is (= 'hive-mcp-projectile
           (clel/extract-ns-name "(ns hive-mcp-projectile)"))))
  (testing "extracts ns name with requires"
    (is (= 'my.app
           (clel/extract-ns-name "(ns my.app (:require [cl-lib]))"))))
  (testing "returns nil for source without ns"
    (is (nil? (clel/extract-ns-name "(defn foo [x] x)"))))
  (testing "returns nil for empty source"
    (is (nil? (clel/extract-ns-name "")))))

(deftest ns-derived-output-name-test
  (testing "derives output name from ns"
    (is (= "hive-mcp-projectile.el"
           (clel/ns-derived-output-name "(ns hive-mcp-projectile)"))))
  (testing "mangles dotted ns name"
    (is (= "my-app.el"
           (clel/ns-derived-output-name "(ns my.app)"))))
  (testing "returns nil for source without ns"
    (is (nil? (clel/ns-derived-output-name "(defn foo [x] x)")))))

;; ============================================================================
;; Project Descriptor - read-project-config (clel-module-001)
;; ============================================================================

(deftest read-project-config-valid-test
  (testing "reads a fully-specified clel.edn"
    (let [config-file (java.io.File/createTempFile "clel" ".edn")]
      (try
        (spit config-file "{:source-paths [\"src\" \"lib\"] :output-dir \"build\" :runtime :bundled}")
        (let [config (clel/read-project-config (.getAbsolutePath config-file))]
          (is (= ["src" "lib"] (:source-paths config)))
          (is (= "build" (:output-dir config)))
          (is (= :bundled (:runtime config))))
        (finally
          (.delete config-file))))))

(deftest read-project-config-defaults-test
  (testing "applies defaults for missing keys"
    (let [config-file (java.io.File/createTempFile "clel" ".edn")]
      (try
        (spit config-file "{}")
        (let [config (clel/read-project-config (.getAbsolutePath config-file))]
          (is (= ["src"] (:source-paths config)))
          (is (= "out" (:output-dir config)))
          (is (= :require (:runtime config))))
        (finally
          (.delete config-file)))))

  (testing "applies defaults for partial config"
    (let [config-file (java.io.File/createTempFile "clel" ".edn")]
      (try
        (spit config-file "{:output-dir \"dist\"}")
        (let [config (clel/read-project-config (.getAbsolutePath config-file))]
          (is (= ["src"] (:source-paths config)))
          (is (= "dist" (:output-dir config)))
          (is (= :require (:runtime config))))
        (finally
          (.delete config-file))))))

(deftest read-project-config-validation-test
  (testing "rejects non-map config"
    (let [config-file (java.io.File/createTempFile "clel" ".edn")]
      (try
        (spit config-file "[:not :a :map]")
        (is (thrown-with-msg? Exception #"must contain a map"
                              (clel/read-project-config (.getAbsolutePath config-file))))
        (finally
          (.delete config-file)))))

  (testing "rejects invalid :runtime value"
    (let [config-file (java.io.File/createTempFile "clel" ".edn")]
      (try
        (spit config-file "{:runtime :invalid}")
        (is (thrown-with-msg? Exception #":runtime must be"
                              (clel/read-project-config (.getAbsolutePath config-file))))
        (finally
          (.delete config-file)))))

  (testing "rejects invalid :source-paths type"
    (let [config-file (java.io.File/createTempFile "clel" ".edn")]
      (try
        (spit config-file "{:source-paths \"not-a-vector\"}")
        (is (thrown-with-msg? Exception #":source-paths must be"
                              (clel/read-project-config (.getAbsolutePath config-file))))
        (finally
          (.delete config-file))))))

;; ============================================================================
;; Project Descriptor - compile-project-from-config (clel-module-001)
;; ============================================================================

(deftest compile-project-from-config-test
  (testing "compiles a project from clel.edn config"
    (let [project-dir (java.io.File/createTempFile "clel-project" "")
          _           (.delete project-dir)
          _           (.mkdirs project-dir)
          src-dir     (io/file project-dir "src" "my")
          out-dir     (io/file project-dir "out")
          config-file (io/file project-dir "clel.edn")]
      (try
        ;; Create source directory
        (.mkdirs src-dir)
        ;; Write config
        (spit config-file "{:source-paths [\"src\"] :output-dir \"out\"}")
        ;; Write source files
        (spit (io/file src-dir "utils.cljel")
              "(ns my.utils)\n(defn helper [x] (+ x 1))")
        (spit (io/file src-dir "app.cljel")
              "(ns my.app (:require [my.utils :as u]))\n(defn main [] (u/helper 42))")
        ;; Compile
        (let [results (clel/compile-project-from-config (.getAbsolutePath config-file))]
          ;; Should compile both files
          (is (= 2 (count (filter some? results))))
          ;; Output directory should exist
          (is (.exists out-dir))
          ;; Output files should exist
          (is (.exists (io/file out-dir "my-utils.el")))
          (is (.exists (io/file out-dir "my-app.el")))
          ;; Check content of compiled files
          (let [utils-el (slurp (io/file out-dir "my-utils.el"))
                app-el   (slurp (io/file out-dir "my-app.el"))]
            (is (str/includes? utils-el "my-utils-helper"))
            (is (str/includes? app-el "my-app-main"))))
        (finally
          ;; Cleanup recursively
          (doseq [f (reverse (file-seq project-dir))]
            (.delete f))))))

  (testing "compiles with :bundled runtime copies runtime file"
    (let [project-dir (java.io.File/createTempFile "clel-project" "")
          _           (.delete project-dir)
          _           (.mkdirs project-dir)
          src-dir     (io/file project-dir "src")
          out-dir     (io/file project-dir "out")
          config-file (io/file project-dir "clel.edn")]
      (try
        (.mkdirs src-dir)
        (spit config-file "{:source-paths [\"src\"] :output-dir \"out\" :runtime :bundled}")
        (spit (io/file src-dir "hello.cljel")
              "(ns hello)\n(defn greet [] \"hi\")")
        (clel/compile-project-from-config (.getAbsolutePath config-file))
        ;; Runtime should be bundled
        (is (.exists (io/file out-dir "clojure-elisp-runtime.el")))
        (finally
          (doseq [f (reverse (file-seq project-dir))]
            (.delete f)))))))

;; ============================================================================
;; Cross-File Symbol Table (clel-module-002)
;; ============================================================================

(deftest build-project-symbol-table-test
  (testing "scans defs from temp files"
    (let [f1 (java.io.File/createTempFile "test-a" ".cljel")
          f2 (java.io.File/createTempFile "test-b" ".cljel")]
      (try
        (spit f1 "(ns my.utils)\n(defn helper [x] x)\n(def pi 3.14)")
        (spit f2 "(ns my.app)\n(defn main [] nil)\n(defn- private-fn [] nil)")
        (let [table (clel/build-project-symbol-table
                      [(.getAbsolutePath f1) (.getAbsolutePath f2)])]
          ;; my.utils exports helper and pi
          (is (contains? table 'my.utils))
          (is (contains? (get table 'my.utils) 'helper))
          (is (contains? (get table 'my.utils) 'pi))
          ;; my.app exports main and private-fn (both are defs)
          (is (contains? table 'my.app))
          (is (contains? (get table 'my.app) 'main))
          (is (contains? (get table 'my.app) 'private-fn)))
        (finally
          (.delete f1)
          (.delete f2)))))

  (testing "skips files without ns form"
    (let [f1 (java.io.File/createTempFile "test-no-ns" ".cljel")]
      (try
        (spit f1 "(defn orphan [] nil)")
        (let [table (clel/build-project-symbol-table
                      [(.getAbsolutePath f1)])]
          (is (empty? table)))
        (finally
          (.delete f1)))))

  (testing "empty file produces empty table"
    (let [f1 (java.io.File/createTempFile "test-empty" ".cljel")]
      (try
        (spit f1 "")
        (let [table (clel/build-project-symbol-table
                      [(.getAbsolutePath f1)])]
          (is (empty? table)))
        (finally
          (.delete f1))))))

(deftest cross-file-warning-test
  (testing "warning emitted for missing symbol in known namespace"
    (let [f1 (java.io.File/createTempFile "test-utils" ".cljel")
          f2 (java.io.File/createTempFile "test-app" ".cljel")
          out-dir (java.io.File/createTempFile "test-out" "")]
      (try
        ;; out-dir needs to be a directory
        (.delete out-dir)
        (.mkdirs out-dir)
        (spit f1 "(ns my.utils)\n(defn helper [x] x)")
        ;; my.app calls my.utils/nonexistent which doesn't exist
        (spit f2 "(ns my.app (:require [my.utils :as u]))\n(defn main [] (u/nonexistent 42))")
        (let [stderr-output (java.io.StringWriter.)
              result (binding [*err* stderr-output]
                       (clel/compile-project [(.getParent f1)] (.getAbsolutePath out-dir)))
              warnings (str stderr-output)]
          ;; Should produce a warning about nonexistent
          (is (str/includes? warnings "WARNING"))
          (is (str/includes? warnings "nonexistent"))
          (is (str/includes? warnings "my.utils"))
          ;; Compilation should still succeed (warning, not error)
          (is (vector? result)))
        (finally
          (.delete f1)
          (.delete f2)
          ;; Clean up output directory
          (doseq [f (file-seq out-dir)]
            (.delete f))))))

  (testing "no warning for symbols in external namespaces"
    ;; When a namespace is NOT in *project-exports*, no warning should be emitted
    (let [warnings (java.io.StringWriter.)]
      (binding [ana/*project-exports* {'my.utils #{'helper}}
                ana/*env* (assoc ana/*env*
                                 :aliases {'ext 'external.lib})
                *err* warnings]
        ;; ext/something — external.lib is not in project-exports, so no warning
        (ana/analyze 'ext/something))
      (is (= "" (str warnings)))))

  (testing "no warning when project-exports is nil (single-file mode)"
    (let [warnings (java.io.StringWriter.)]
      (binding [ana/*project-exports* nil
                ana/*env* (assoc ana/*env*
                                 :aliases {'u 'my.utils})
                *err* warnings]
        (ana/analyze 'u/anything))
      (is (= "" (str warnings))))))

;; ============================================================================
;; :refer :all - Wildcard Imports (clel-module-003)
;; ============================================================================

(deftest refer-all-project-compilation-test
  (testing "compile-project resolves :refer :all symbols correctly"
    (let [project-dir (java.io.File/createTempFile "clel-refer-all" "")
          _           (.delete project-dir)
          _           (.mkdirs project-dir)
          src-dir     (io/file project-dir "src" "my")
          out-dir     (io/file project-dir "out")]
      (try
        (.mkdirs src-dir)
        ;; utils.cljel exports helper and pi
        (spit (io/file src-dir "utils.cljel")
              "(ns my.utils)\n(defn helper [x] (+ x 1))\n(def pi 3.14)")
        ;; app.cljel uses :refer :all to import everything from my.utils
        (spit (io/file src-dir "app.cljel")
              "(ns my.app (:require [my.utils :refer :all]))\n(defn main [] (helper pi))")
        (let [results (clel/compile-project [(.getAbsolutePath (io/file project-dir "src"))]
                                            (.getAbsolutePath out-dir))]
          ;; Both files should compile
          (is (= 2 (count (filter some? results))))
          ;; Check that referred symbols resolve correctly in the output
          (let [app-el (slurp (io/file out-dir "my-app.el"))]
            ;; helper should resolve to my-utils-helper (namespace-prefixed)
            (is (str/includes? app-el "my-utils-helper"))
            ;; pi should resolve to my-utils-pi (namespace-prefixed)
            (is (str/includes? app-el "my-utils-pi"))))
        (finally
          (doseq [f (reverse (file-seq project-dir))]
            (.delete f))))))

  (testing ":refer :all with :as alias both work together"
    (let [project-dir (java.io.File/createTempFile "clel-refer-all-as" "")
          _           (.delete project-dir)
          _           (.mkdirs project-dir)
          src-dir     (io/file project-dir "src" "my")
          out-dir     (io/file project-dir "out")]
      (try
        (.mkdirs src-dir)
        (spit (io/file src-dir "utils.cljel")
              "(ns my.utils)\n(defn helper [x] (+ x 1))")
        ;; Use both :as and :refer :all
        (spit (io/file src-dir "app.cljel")
              "(ns my.app (:require [my.utils :as u :refer :all]))\n(defn main [] (helper 1))\n(defn other [] (u/helper 2))")
        (let [results (clel/compile-project [(.getAbsolutePath (io/file project-dir "src"))]
                                            (.getAbsolutePath out-dir))]
          (is (= 2 (count (filter some? results))))
          (let [app-el (slurp (io/file out-dir "my-app.el"))]
            ;; Both unqualified (via :refer :all) and qualified (via :as) should resolve
            (is (str/includes? app-el "my-utils-helper"))))
        (finally
          (doseq [f (reverse (file-seq project-dir))]
            (.delete f)))))))

(deftest refer-all-single-file-graceful-test
  (testing ":refer :all in single-file mode compiles without error"
    ;; compile-file-string doesn't set *project-exports*, so :refer :all
    ;; should be a no-op — symbols just won't resolve to a namespace
    (let [result (clel/compile-file-string
                   "(ns my.app (:require [my.utils :refer :all]))\n(defn main [] (helper 42))")]
      (is (string? result))
      ;; helper should appear in output (unresolved, so no namespace prefix)
      (is (str/includes? result "helper")))))
