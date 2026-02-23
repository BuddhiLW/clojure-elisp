(ns clojure-elisp.emitter-test
  "Tests for the ClojureElisp emitter."
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-elisp.analyzer :as ana :refer [analyze]]
            [clojure-elisp.emitter :as emit :refer [emit-node mangle-name]]))

;; ============================================================================
;; Helper Functions
;; ============================================================================

(defn analyze-and-emit
  "Analyze a form and emit it to Elisp."
  [form]
  (-> form ana/analyze emit/emit))

;; ============================================================================
;; Name Mangling
;; ============================================================================

(deftest mangle-name-test
  (testing "simple names"
    (is (= "foo" (mangle-name 'foo)))
    (is (= "bar" (mangle-name 'bar))))
  (testing "names with dots"
    (is (= "my-namespace" (mangle-name 'my.namespace))))
  (testing "names with slashes"
    (is (= "my-ns-foo" (mangle-name 'my.ns/foo))))
  (testing "predicate names (ending in ?)"
    (is (= "nil-p" (mangle-name 'nil?)))
    (is (= "empty-p" (mangle-name 'empty?))))
  (testing "bang names (ending in !)"
    (is (= "reset-bang" (mangle-name 'reset!)))
    (is (= "swap-bang" (mangle-name 'swap!))))
  (testing "comparison operators"
    (is (= "-gt" (mangle-name '>)))
    (is (= "-lt" (mangle-name '<)))
    (is (= "-gt-eq" (mangle-name '>=)))
    (is (= "-lt-eq" (mangle-name '<=))))
  (testing "arithmetic operators"
    (is (= "-plus" (mangle-name '+)))
    (is (= "-star" (mangle-name '*))))
  (testing "complex names"
    (is (= "my-package-do-something-bang" (mangle-name 'my.package/do-something!)))))

;; ============================================================================
;; Constants
;; ============================================================================

(deftest emit-nil-test
  (testing "nil constant"
    (is (= "nil" (analyze-and-emit nil)))))

(deftest emit-boolean-test
  (testing "true"
    (is (= "t" (analyze-and-emit true))))
  (testing "false"
    (is (= "nil" (analyze-and-emit false)))))

(deftest emit-number-test
  (testing "integer"
    (is (= "42" (analyze-and-emit 42))))
  (testing "negative integer"
    (is (= "-17" (analyze-and-emit -17))))
  (testing "float"
    (is (= "3.14" (analyze-and-emit 3.14)))))

(deftest emit-string-test
  (testing "simple string"
    (is (= "\"hello\"" (analyze-and-emit "hello"))))
  (testing "string with quotes"
    (is (= "\"hello \\\"world\\\"\"" (analyze-and-emit "hello \"world\""))))
  (testing "empty string"
    (is (= "\"\"" (analyze-and-emit "")))))

(deftest emit-keyword-test
  (testing "simple keyword"
    (is (= ":foo" (analyze-and-emit :foo))))
  (testing "namespaced keyword"
    (is (= ":bar" (analyze-and-emit :my.ns/bar)))))

;; ============================================================================
;; Symbols
;; ============================================================================

(deftest emit-var-test
  (testing "simple var"
    (is (= "foo" (analyze-and-emit 'foo))))
  (testing "core function maps to elisp"
    (is (= "clel-first" (analyze-and-emit 'first)))
    (is (= "clel-rest" (analyze-and-emit 'rest)))
    (is (= "1+" (analyze-and-emit 'inc)))
    (is (= "1-" (analyze-and-emit 'dec)))
    (is (= "length" (analyze-and-emit 'count)))))

(deftest emit-local-test
  (testing "local in let"
    (let [result (analyze-and-emit '(let [x 1] x))]
      (is (clojure.string/includes? result "x")))))

;; ============================================================================
;; Collections
;; ============================================================================

(deftest emit-vector-test
  (testing "empty vector"
    (is (= "(list )" (analyze-and-emit []))))
  (testing "vector with elements"
    (is (= "(list 1 2 3)" (analyze-and-emit [1 2 3])))))

(deftest emit-map-test
  (testing "empty map"
    (is (= "'()" (analyze-and-emit {}))))
  (testing "map with entries"
    (let [result (analyze-and-emit {:a 1})]
      (is (clojure.string/includes? result ":a"))
      (is (clojure.string/includes? result "1")))))

(deftest emit-set-test
  (testing "set emits as list"
    (let [result (analyze-and-emit #{1 2 3})]
      (is (clojure.string/starts-with? result "(list")))))

;; ============================================================================
;; Quote
;; ============================================================================

(deftest emit-quote-test
  (testing "quoted symbol"
    (is (= "'foo" (analyze-and-emit '(quote foo)))))
  (testing "quoted list"
    (is (= "'(1 2 3)" (analyze-and-emit '(quote (1 2 3)))))))

;; ============================================================================
;; def
;; ============================================================================

(deftest emit-def-test
  (testing "simple def"
    (let [result (analyze-and-emit '(def foo 42))]
      (is (clojure.string/includes? result "defvar"))
      (is (clojure.string/includes? result "foo"))
      (is (clojure.string/includes? result "42"))))
  (testing "def without init"
    (let [result (analyze-and-emit '(def bar))]
      (is (clojure.string/includes? result "defvar"))
      (is (clojure.string/includes? result "bar"))
      (is (clojure.string/includes? result "nil")))))

;; ============================================================================
;; defn
;; ============================================================================

(deftest emit-defn-test
  (testing "simple defn"
    (let [result (analyze-and-emit '(defn foo [x] x))]
      (is (clojure.string/includes? result "defun"))
      (is (clojure.string/includes? result "foo"))
      (is (clojure.string/includes? result "(x)"))))
  (testing "defn with docstring"
    (let [result (analyze-and-emit '(defn greet "Says hello" [name] name))]
      (is (clojure.string/includes? result "defun"))
      (is (clojure.string/includes? result "Says hello"))))
  (testing "defn with multiple params"
    (let [result (analyze-and-emit '(defn add [a b] (+ a b)))]
      (is (clojure.string/includes? result "(a b)"))))
  (testing "defn name mangling"
    (let [result (analyze-and-emit '(defn nil? [x] x))]
      (is (clojure.string/includes? result "nil-p")))))

(deftest emit-defn-multi-arity-test
  (testing "multi-arity defn emits cl-case dispatch"
    (let [result (analyze-and-emit '(defn foo ([x] x) ([x y] (+ x y))))]
      (is (clojure.string/includes? result "defun"))
      (is (clojure.string/includes? result "foo"))
      (is (clojure.string/includes? result "&rest args"))
      (is (clojure.string/includes? result "cl-case"))
      (is (clojure.string/includes? result "(length args)"))
      ;; Check for arity cases
      (is (clojure.string/includes? result "(1 (let"))
      (is (clojure.string/includes? result "(2 (let"))))
  (testing "multi-arity with variadic uses t for catch-all"
    (let [result (analyze-and-emit '(defn bar ([x] x) ([x & more] (cons x more))))]
      (is (clojure.string/includes? result "cl-case"))
      (is (clojure.string/includes? result "(1 (let"))
      (is (clojure.string/includes? result "(t (let"))
      (is (clojure.string/includes? result "nthcdr")))))

(deftest emit-defn-variadic-test
  (testing "single-arity variadic uses &rest"
    (let [result (analyze-and-emit '(defn varargs [x & rest] (cons x rest)))]
      (is (clojure.string/includes? result "defun"))
      (is (clojure.string/includes? result "&rest args"))
      (is (clojure.string/includes? result "let"))
      (is (clojure.string/includes? result "(nth 0 args)"))
      (is (clojure.string/includes? result "nthcdr")))))

;; ============================================================================
;; fn (lambda)
;; ============================================================================

(deftest emit-fn-test
  (testing "simple fn"
    (let [result (analyze-and-emit '(fn [x] x))]
      (is (clojure.string/includes? result "lambda"))
      (is (clojure.string/includes? result "(x)"))))
  (testing "fn with multiple params"
    (let [result (analyze-and-emit '(fn [a b] (+ a b)))]
      (is (clojure.string/includes? result "lambda"))
      (is (clojure.string/includes? result "(a b)")))))

;; ============================================================================
;; let
;; ============================================================================

(deftest emit-let-test
  (testing "simple let"
    (let [result (analyze-and-emit '(let [x 1] x))]
      (is (clojure.string/includes? result "let*"))
      (is (clojure.string/includes? result "(x 1)"))))
  (testing "let with multiple bindings"
    (let [result (analyze-and-emit '(let [a 1 b 2] (+ a b)))]
      (is (clojure.string/includes? result "let*"))
      (is (clojure.string/includes? result "(a 1)"))
      (is (clojure.string/includes? result "(b 2)")))))

;; ============================================================================
;; if
;; ============================================================================

(deftest emit-if-test
  (testing "if with then and else"
    (let [result (analyze-and-emit '(if true 1 2))]
      (is (clojure.string/includes? result "if"))
      (is (clojure.string/includes? result "t"))
      (is (clojure.string/includes? result "1"))
      (is (clojure.string/includes? result "2"))))
  (testing "if without else emits as when"
    (let [result (analyze-and-emit '(if true 1))]
      (is (clojure.string/includes? result "when")))))

;; ============================================================================
;; when
;; ============================================================================

(deftest emit-when-test
  (testing "simple when"
    (let [result (analyze-and-emit '(when true 42))]
      (is (clojure.string/includes? result "when"))
      (is (clojure.string/includes? result "t"))
      (is (clojure.string/includes? result "42")))))

;; ============================================================================
;; cond
;; ============================================================================

(deftest emit-cond-test
  (testing "simple cond"
    (let [result (analyze-and-emit '(cond true 1 false 2))]
      (is (clojure.string/includes? result "cond"))
      (is (clojure.string/includes? result "(t 1)"))
      (is (clojure.string/includes? result "(nil 2)"))))

  (testing "cond :else maps to t (Elisp catch-all)"
    (let [result (analyze-and-emit '(cond
                                      (> x 0) "positive"
                                      (< x 0) "negative"
                                      :else "zero"))]
      (is (clojure.string/includes? result "cond"))
      (is (clojure.string/includes? result "(> x 0)"))
      (is (clojure.string/includes? result "(t \"zero\")"))
      (is (not (clojure.string/includes? result ":else")))
      (is (not (clojure.string/includes? result "else")))))

  (testing "cond with only :else clause"
    (let [result (analyze-and-emit '(cond :else 42))]
      (is (clojure.string/includes? result "(t 42)"))
      (is (not (clojure.string/includes? result ":else"))))))

(deftest emit-case-test
  (testing "case without default"
    (let [result (analyze-and-emit '(case x :a 1 :b 2))]
      (is (clojure.string/includes? result "cl-case"))
      (is (clojure.string/includes? result ":a"))
      (is (clojure.string/includes? result ":b"))))

  (testing "case with default"
    (let [result (analyze-and-emit '(case x :a 1 :b 2 :default-val))]
      (is (clojure.string/includes? result "cl-case"))
      (is (clojure.string/includes? result "(t :default-val)"))))

  (testing "case with numeric tests"
    (let [result (analyze-and-emit '(case n 1 "one" 2 "two" "other"))]
      (is (clojure.string/includes? result "cl-case"))
      (is (clojure.string/includes? result "(1 \"one\")"))
      (is (clojure.string/includes? result "(t \"other\")")))))

;; ============================================================================
;; do
;; ============================================================================

(deftest emit-do-test
  (testing "simple do"
    (let [result (analyze-and-emit '(do 1 2 3))]
      (is (clojure.string/includes? result "progn"))
      (is (clojure.string/includes? result "1"))
      (is (clojure.string/includes? result "2"))
      (is (clojure.string/includes? result "3")))))

(deftest emit-and-test
  (testing "and emits Elisp and form"
    (is (= "(and a b c)" (analyze-and-emit '(and a b c)))))
  (testing "empty and returns t"
    (is (= "t" (analyze-and-emit '(and)))))
  (testing "and with single expression"
    (is (= "(and t)" (analyze-and-emit '(and true))))))

(deftest emit-or-test
  (testing "or emits Elisp or form"
    (is (= "(or a b c)" (analyze-and-emit '(or a b c)))))
  (testing "empty or returns nil"
    (is (= "nil" (analyze-and-emit '(or)))))
  (testing "or with single expression"
    (is (= "(or nil)" (analyze-and-emit '(or false))))))

;; ============================================================================
;; ns
;; ============================================================================

(deftest emit-ns-test
  (testing "simple ns"
    (let [result (analyze-and-emit '(ns my.package))]
      (is (clojure.string/includes? result "my-package"))
      (is (clojure.string/includes? result "lexical-binding: t"))
      (is (clojure.string/includes? result "clojure-elisp-runtime")))))

(deftest emit-ns-require-test
  (testing "emits require for simple dependency"
    (let [code (analyze-and-emit '(ns my.app (:require [foo.bar])))]
      (is (clojure.string/includes? code "(require 'foo-bar)"))))

  (testing "emits require with alias"
    (let [code (analyze-and-emit '(ns my.app (:require [foo.bar :as fb])))]
      (is (clojure.string/includes? code "(require 'foo-bar)"))))

  (testing "emits require for bare symbol"
    (let [code (analyze-and-emit '(ns my.app (:require clojure.set)))]
      (is (clojure.string/includes? code "(require 'clojure-set)"))))

  (testing "emits multiple requires"
    (let [code (analyze-and-emit '(ns my.app
                                    (:require [foo.bar :as fb]
                                              [baz.qux :as bq])))]
      (is (clojure.string/includes? code "(require 'foo-bar)"))
      (is (clojure.string/includes? code "(require 'baz-qux)"))))

  (testing "ns header structure"
    (let [code (analyze-and-emit '(ns my.app (:require [foo.bar])))]
      (is (clojure.string/includes? code ";;; my-app.el"))
      (is (clojure.string/includes? code "lexical-binding: t"))
      (is (clojure.string/includes? code "(require 'clojure-elisp-runtime)")))))

;; ============================================================================
;; loop/recur
;; ============================================================================

(deftest emit-loop-test
  (testing "simple loop"
    (let [result (analyze-and-emit '(loop [x 0] x))]
      (is (clojure.string/includes? result "cl-labels"))
      (is (clojure.string/includes? result "recur")))))

(deftest emit-letfn-test
  (testing "letfn emits cl-labels"
    (let [result (analyze-and-emit '(letfn [(foo [x] (+ x 1))] (foo 5)))]
      (is (clojure.string/includes? result "cl-labels"))
      (is (clojure.string/includes? result "foo"))))

  (testing "letfn with mutual recursion"
    (let [result (analyze-and-emit '(letfn [(a [x] (b x))
                                            (b [y] (+ y 1))]
                                      (a 5)))]
      (is (clojure.string/includes? result "cl-labels"))
      (is (clojure.string/includes? result "(a (x)"))
      (is (clojure.string/includes? result "(b (y)"))))

  (testing "letfn with predicate function names"
    (let [result (analyze-and-emit '(letfn [(even? [n] n)] (even? 2)))]
      ;; Function name should be mangled: even? -> even-p
      (is (clojure.string/includes? result "even-p")))))

(deftest emit-recur-test
  (testing "recur call"
    (let [result (analyze-and-emit '(recur 1 2))]
      (is (clojure.string/includes? result "recur"))
      (is (clojure.string/includes? result "1"))
      (is (clojure.string/includes? result "2")))))

;; ============================================================================
;; Function Invocation
;; ============================================================================

(deftest emit-invoke-test
  (testing "simple function call"
    (let [result (analyze-and-emit '(+ 1 2))]
      (is (= "(+ 1 2)" result))))
  (testing "core function mapping"
    (let [result (analyze-and-emit '(first xs))]
      (is (= "(clel-first xs)" result))))
  (testing "nested function calls"
    (let [result (analyze-and-emit '(+ (* 2 3) 4))]
      (is (clojure.string/includes? result "(* 2 3)"))
      (is (clojure.string/includes? result "+"))))
  (testing "no-arg function call"
    (let [result (analyze-and-emit '(foo))]
      (is (= "(foo)" result)))))

;; ============================================================================
;; Core Function Mappings
;; ============================================================================

(deftest core-fn-mapping-test
  (testing "list operations"
    (is (= "(clel-first xs)" (analyze-and-emit '(first xs))))
    (is (= "(clel-rest xs)" (analyze-and-emit '(rest xs))))
    (is (= "(length xs)" (analyze-and-emit '(count xs)))))
  (testing "arithmetic"
    (is (= "(1+ x)" (analyze-and-emit '(inc x))))
    (is (= "(1- x)" (analyze-and-emit '(dec x)))))
  (testing "predicates"
    (is (= "(null x)" (analyze-and-emit '(nil? x))))
    (is (= "(stringp x)" (analyze-and-emit '(string? x))))
    (is (= "(numberp x)" (analyze-and-emit '(number? x)))))
  (testing "string functions"
    (is (= "(clel-str a b)" (analyze-and-emit '(str a b)))))
  (testing "higher-order functions"
    (is (= "(clel-map f xs)" (analyze-and-emit '(map f xs))))))

;; ============================================================================
;; Sequence Functions (clel-029)
;; ============================================================================

(deftest emit-seq-map-test
  (testing "map with function and collection"
    (is (= "(clel-map f xs)" (analyze-and-emit '(map f xs)))))
  (testing "map with lambda"
    (let [result (analyze-and-emit '(map (fn [x] (+ x 1)) xs))]
      (is (clojure.string/includes? result "clel-map"))
      (is (clojure.string/includes? result "lambda"))))
  (testing "map with core function"
    (is (= "(clel-map 1+ xs)" (analyze-and-emit '(map inc xs))))))

(deftest emit-seq-filter-test
  (testing "filter with predicate and collection"
    (is (= "(clel-filter pred xs)" (analyze-and-emit '(filter pred xs)))))
  (testing "filter with lambda"
    (let [result (analyze-and-emit '(filter (fn [x] (> x 0)) xs))]
      (is (clojure.string/includes? result "clel-filter"))
      (is (clojure.string/includes? result "lambda"))))
  (testing "filter with core predicate"
    (is (= "(clel-filter cl-evenp nums)" (analyze-and-emit '(filter even? nums))))))

(deftest emit-seq-reduce-test
  (testing "reduce with function and collection"
    (is (= "(clel-reduce f xs)" (analyze-and-emit '(reduce f xs)))))
  (testing "reduce with initial value"
    (is (= "(clel-reduce f 0 xs)" (analyze-and-emit '(reduce f 0 xs)))))
  (testing "reduce with + function"
    (is (= "(clel-reduce + xs)" (analyze-and-emit '(reduce + xs)))))
  (testing "reduce with lambda"
    (let [result (analyze-and-emit '(reduce (fn [acc x] (+ acc x)) 0 xs))]
      (is (clojure.string/includes? result "clel-reduce"))
      (is (clojure.string/includes? result "lambda")))))

(deftest emit-seq-take-test
  (testing "take n elements"
    (is (= "(clel-take 5 xs)" (analyze-and-emit '(take 5 xs)))))
  (testing "take with variable"
    (is (= "(clel-take n xs)" (analyze-and-emit '(take n xs))))))

(deftest emit-seq-drop-test
  (testing "drop n elements"
    (is (= "(clel-drop 3 xs)" (analyze-and-emit '(drop 3 xs)))))
  (testing "drop with variable"
    (is (= "(clel-drop n xs)" (analyze-and-emit '(drop n xs))))))

(deftest emit-seq-partition-test
  (testing "partition with size"
    (is (= "(clel-partition 2 xs)" (analyze-and-emit '(partition 2 xs)))))
  (testing "partition with variable size"
    (is (= "(clel-partition n coll)" (analyze-and-emit '(partition n coll))))))

(deftest emit-seq-take-while-test
  (testing "take-while with predicate"
    (is (= "(clel-take-while pred xs)" (analyze-and-emit '(take-while pred xs)))))
  (testing "take-while with lambda"
    (let [result (analyze-and-emit '(take-while (fn [x] (< x 10)) xs))]
      (is (clojure.string/includes? result "clel-take-while"))
      (is (clojure.string/includes? result "lambda"))))
  (testing "take-while with core predicate"
    (is (= "(clel-take-while cl-plusp nums)" (analyze-and-emit '(take-while pos? nums))))))

(deftest emit-seq-drop-while-test
  (testing "drop-while with predicate"
    (is (= "(clel-drop-while pred xs)" (analyze-and-emit '(drop-while pred xs)))))
  (testing "drop-while with lambda"
    (let [result (analyze-and-emit '(drop-while (fn [x] (< x 5)) xs))]
      (is (clojure.string/includes? result "clel-drop-while"))
      (is (clojure.string/includes? result "lambda"))))
  (testing "drop-while with core predicate"
    (is (= "(clel-drop-while cl-minusp nums)" (analyze-and-emit '(drop-while neg? nums))))))

;; ============================================================================
;; Full Pipeline Tests
;; ============================================================================

(deftest full-pipeline-test
  (testing "defn with arithmetic"
    (let [result (analyze-and-emit '(defn double [x] (* x 2)))]
      (is (clojure.string/includes? result "defun"))
      (is (clojure.string/includes? result "double"))
      (is (clojure.string/includes? result "(* x 2)"))))
  (testing "let with nested expressions"
    (let [result (analyze-and-emit '(let [a 1 b 2] (+ a b)))]
      (is (clojure.string/includes? result "let*"))
      (is (clojure.string/includes? result "(+ a b)"))))
  (testing "cond with comparisons and :else → t"
    (let [result (analyze-and-emit '(cond (> x 0) "positive"
                                          (< x 0) "negative"
                                          :else "zero"))]
      (is (clojure.string/includes? result "cond"))
      (is (clojure.string/includes? result "(> x 0)"))
      (is (clojure.string/includes? result "(t \"zero\")") ":else should map to t in Elisp cond"))))

;; ============================================================================
;; Edge Cases
;; ============================================================================

(deftest edge-cases-test
  (testing "deeply nested expressions"
    (let [result (analyze-and-emit '(if (and (> x 0) (< x 10))
                                      (+ x 1)
                                      (- x 1)))]
      (is (clojure.string/includes? result "if"))
      (is (clojure.string/includes? result "and"))))
  (testing "function as argument"
    (let [result (analyze-and-emit '(map inc [1 2 3]))]
      (is (clojure.string/includes? result "clel-map"))
      (is (clojure.string/includes? result "1+")))))

;; ============================================================================
;; Elisp Validity Tests (Basic Structure)
;; ============================================================================

(deftest elisp-syntax-validity-test
  (testing "parentheses are balanced"
    (let [forms ['(defn foo [x] x)
                 '(let [a 1] a)
                 '(if true 1 2)
                 '(cond true 1 false 2)
                 '(fn [x] (+ x 1))]]
      (doseq [form forms]
        (let [result (analyze-and-emit form)
              opens  (count (filter #(= % \() result))
              closes (count (filter #(= % \)) result))]
          (is (= opens closes)
              (str "Unbalanced parens in: " result))))))
  (testing "strings are properly quoted"
    (let [result (analyze-and-emit "hello world")]
      (is (clojure.string/starts-with? result "\""))
      (is (clojure.string/ends-with? result "\"")))))

;; ============================================================================
;; Multimethods - defmulti/defmethod
;; ============================================================================

(deftest emit-defmulti-test
  (testing "defmulti emits cl-defgeneric"
    (let [code (analyze-and-emit '(defmulti area :shape))]
      (is (clojure.string/includes? code "cl-defgeneric"))
      (is (clojure.string/includes? code "area"))))

  (testing "defmulti with function dispatch"
    (let [code (analyze-and-emit '(defmulti process class))]
      (is (clojure.string/includes? code "cl-defgeneric"))
      (is (clojure.string/includes? code "process")))))

(deftest emit-defmethod-test
  (testing "defmethod emits cl-defmethod with eql"
    (let [code (analyze-and-emit '(defmethod area :circle [{:keys [r]}] (* 3.14 r r)))]
      (is (clojure.string/includes? code "cl-defmethod"))
      (is (clojure.string/includes? code "eql"))
      (is (clojure.string/includes? code ":circle"))))

  (testing "defmethod with :default dispatch"
    (let [code (analyze-and-emit '(defmethod area :default [shape] 0))]
      (is (clojure.string/includes? code "cl-defmethod"))
      (is (clojure.string/includes? code "(shape t)")))) ;; :default maps to t in elisp

  (testing "defmethod body is emitted"
    (let [code (analyze-and-emit '(defmethod area :rectangle [{:keys [w h]}] (* w h)))]
      (is (clojure.string/includes? code "*"))))

  (testing "defmethod without destructuring"
    (let [code (analyze-and-emit '(defmethod greet :english [person] (str "Hello")))]
      (is (clojure.string/includes? code "(person (eql :english))"))
      (is (not (clojure.string/includes? code "let*"))))))

;; ============================================================================
;; Elisp Interop (clel-021)
;; ============================================================================

(deftest emit-interop-dot-call-test
  (testing "dot-call with no args"
    (is (= "(buffer-name)" (analyze-and-emit '(.buffer-name)))))
  (testing "dot-call with args"
    (is (= "(substring \"hello\" 0 3)" (analyze-and-emit '(.substring "hello" 0 3)))))
  (testing "dot-call with operator-like name"
    (is (= "(+ 1 2)" (analyze-and-emit '(.+ 1 2)))))
  (testing "dot-call with single arg"
    (is (= "(message \"hi\")" (analyze-and-emit '(.message "hi"))))))

(deftest emit-interop-elisp-ns-test
  (testing "elisp/ namespace call with one arg"
    (is (= "(message \"hi\")" (analyze-and-emit '(elisp/message "hi")))))
  (testing "elisp/ namespace call with multiple args"
    (is (= "(message \"hello %s\" user-name)" (analyze-and-emit '(elisp/message "hello %s" user-name)))))
  (testing "elisp/ namespace call no args"
    (is (= "(point)" (analyze-and-emit '(elisp/point)))))
  (testing "elisp/ namespace call with quoted arg"
    (is (= "(car '(1 2 3))" (analyze-and-emit '(elisp/car '(1 2 3)))))))

(deftest emit-interop-property-access-test
  (testing "property access zero-arg function"
    (is (= "(point)" (analyze-and-emit '(.-point)))))
  (testing "property access buffer-name"
    (is (= "(buffer-name)" (analyze-and-emit '(.-buffer-name))))))

;; ============================================================================
;; Lazy Sequences (clel-018)
;; ============================================================================

(deftest emit-lazy-seq-test
  (testing "lazy-seq wraps body in lambda"
    (is (= "(clel-lazy-seq-create (lambda () (cons 1 nil)))"
           (analyze-and-emit '(lazy-seq (cons 1 nil))))))
  (testing "lazy-seq with multiple body forms"
    (let [result (analyze-and-emit '(lazy-seq (+ 1 2)))]
      (is (clojure.string/includes? result "clel-lazy-seq-create"))
      (is (clojure.string/includes? result "lambda")))))

(deftest emit-realized-p-test
  (testing "realized? maps to clel-realized-p"
    (is (= "(clel-realized-p x)" (analyze-and-emit '(realized? x))))))

(deftest emit-doall-test
  (testing "doall maps to clel-doall"
    (is (= "(clel-doall xs)" (analyze-and-emit '(doall xs))))))

(deftest emit-dorun-test
  (testing "dorun maps to clel-dorun"
    (is (= "(clel-dorun xs)" (analyze-and-emit '(dorun xs))))))

;; ============================================================================
;; Source Location Comments (clel-020)
;; ============================================================================

(deftest emit-source-comments-test
  (testing "source comments disabled by default"
    (let [form   (with-meta '(defn foo [x] x) {:line 10 :column 1})
          result (-> form ana/analyze emit/emit)]
      (is (not (clojure.string/includes? result ";;; L10")))))

  (testing "source comments emitted when *emit-source-comments* is true"
    (let [form   (with-meta '(defn foo [x] x) {:line 10 :column 1})
          result (binding [emit/*emit-source-comments* true]
                   (-> form ana/analyze emit/emit))]
      (is (clojure.string/includes? result ";;; L10:C1"))))

  (testing "source comments for def forms"
    (let [form   (with-meta '(def bar 42) {:line 5 :column 0})
          result (binding [emit/*emit-source-comments* true]
                   (-> form ana/analyze emit/emit))]
      (is (clojure.string/includes? result ";;; L5:C0"))))

  (testing "no source comment when node lacks location"
    (let [result (binding [emit/*emit-source-comments* true]
                   (analyze-and-emit 42))]
      (is (not (clojure.string/includes? result ";;;")))))

  (testing "source comments for fn (lambda) forms"
    (let [form   (with-meta '(fn [x] x) {:line 20 :column 3})
          result (binding [emit/*emit-source-comments* true]
                   (-> form ana/analyze emit/emit))]
      (is (clojure.string/includes? result ";;; L20:C3")))))

;; ============================================================================
;; Protocols - defprotocol (clel-025)
;; ============================================================================

(deftest emit-defprotocol-test
  (testing "protocol emits cl-defgeneric for each method"
    (let [code (analyze-and-emit '(defprotocol IGreeter
                                    (greet [this name])))]
      (is (clojure.string/includes? code "cl-defgeneric"))
      (is (clojure.string/includes? code "greet"))))

  (testing "protocol with multiple methods emits multiple cl-defgeneric"
    (let [code (analyze-and-emit '(defprotocol IShape
                                    (area [this])
                                    (perimeter [this])))]
      (is (= 2 (count (re-seq #"cl-defgeneric" code))))))

  (testing "protocol method params are emitted"
    (let [code (analyze-and-emit '(defprotocol IWriter
                                    (write [this data])))]
      (is (clojure.string/includes? code "write (this data)")))))

;; ============================================================================
;; Records - defrecord (clel-025)
;; ============================================================================

(deftest emit-defrecord-test
  (testing "record emits cl-defstruct"
    (let [code (analyze-and-emit '(defrecord Point [x y]))]
      (is (clojure.string/includes? code "cl-defstruct"))
      (is (clojure.string/includes? code "Point"))))

  (testing "record emits positional constructor"
    (let [code (analyze-and-emit '(defrecord Point [x y]))]
      (is (clojure.string/includes? code "->Point"))
      (is (clojure.string/includes? code "Point--create"))))

  (testing "record emits map constructor"
    (let [code (analyze-and-emit '(defrecord Point [x y]))]
      (is (clojure.string/includes? code "map->Point"))))

  (testing "record with protocol emits cl-defmethod"
    (let [code (analyze-and-emit '(defrecord Person [first-name last-name]
                                    IGreeter
                                    (greet [this name] (str "Hello"))))]
      (is (clojure.string/includes? code "cl-defmethod"))
      (is (clojure.string/includes? code "greet"))
      (is (clojure.string/includes? code "(this Person)"))))

  (testing "record method body wraps fields in let*"
    (let [code (analyze-and-emit '(defrecord Person [name]
                                    IGreeter
                                    (greet [this] name)))]
      (is (clojure.string/includes? code "let*"))
      (is (clojure.string/includes? code "Person-name"))))

  (testing "record method does not bind fields shadowed by params"
    (let [code (analyze-and-emit '(defrecord Person [name]
                                    IGreeter
                                    (greet [this name] name)))]
      ;; name param shadows name field, so let* should not bind 'name'
      (is (not (clojure.string/includes? code "Person-name"))))))

;; ============================================================================
;; Types - deftype (clel-025)
;; ============================================================================

(deftest emit-deftype-test
  (testing "deftype emits cl-defstruct"
    (let [code (analyze-and-emit '(deftype Counter [^:mutable count]))]
      (is (clojure.string/includes? code "cl-defstruct"))
      (is (clojure.string/includes? code "Counter"))))

  (testing "deftype emits positional constructor"
    (let [code (analyze-and-emit '(deftype Counter [^:mutable count]))]
      (is (clojure.string/includes? code "->Counter"))))

  (testing "deftype does NOT emit map constructor"
    (let [code (analyze-and-emit '(deftype Counter [^:mutable count]))]
      (is (not (clojure.string/includes? code "map->Counter")))))

  (testing "deftype method uses cl-symbol-macrolet for fields"
    (let [code (analyze-and-emit '(deftype Counter [^:mutable count]
                                    ICounter
                                    (get-count [this] count)))]
      (is (clojure.string/includes? code "cl-symbol-macrolet"))
      (is (clojure.string/includes? code "Counter-count")))))

;; ============================================================================
;; set! (clel-025)
;; ============================================================================

(deftest emit-set!-test
  (testing "set! emits setf"
    (let [code (analyze-and-emit '(set! x 42))]
      (is (clojure.string/includes? code "setf"))
      (is (clojure.string/includes? code "x"))
      (is (clojure.string/includes? code "42")))))

;; ============================================================================
;; extend-type (clel-025)
;; ============================================================================

(deftest emit-extend-type-test
  (testing "extend-type emits cl-defmethod with type specializer"
    (let [code (analyze-and-emit '(extend-type String
                                    IGreeter
                                    (greet [this] this)))]
      (is (clojure.string/includes? code "cl-defmethod"))
      (is (clojure.string/includes? code "greet"))
      (is (clojure.string/includes? code "string"))))

  (testing "extend-type with multiple protocols"
    (let [code (analyze-and-emit '(extend-type Number
                                    IShow
                                    (show [this] this)
                                    IMath
                                    (double-val [this] this)))]
      (is (clojure.string/includes? code "cl-defmethod show"))
      (is (clojure.string/includes? code "cl-defmethod double-val"))
      (is (clojure.string/includes? code "number"))))

  (testing "extend-type with custom record type"
    (let [code (analyze-and-emit '(extend-type MyRecord
                                    IShow
                                    (show [this] this)))]
      (is (clojure.string/includes? code "cl-defmethod"))
      (is (clojure.string/includes? code "MyRecord")))))

;; ============================================================================
;; extend-protocol (clel-025)
;; ============================================================================

(deftest emit-extend-protocol-test
  (testing "extend-protocol emits cl-defmethod for each type"
    (let [code (analyze-and-emit '(extend-protocol IGreeter
                                    String
                                    (greet [this] this)
                                    Number
                                    (greet [this] this)))]
      (is (clojure.string/includes? code "cl-defmethod greet ((this string))"))
      (is (clojure.string/includes? code "cl-defmethod greet ((this number))"))))

  (testing "extend-protocol with multiple methods per type"
    (let [code (analyze-and-emit '(extend-protocol IShape
                                    String
                                    (area [this] 0)
                                    (perimeter [this] 0)))]
      (is (clojure.string/includes? code "cl-defmethod area"))
      (is (clojure.string/includes? code "cl-defmethod perimeter")))))

;; ============================================================================
;; satisfies? (clel-025)
;; ============================================================================

(deftest emit-satisfies?-test
  (testing "satisfies? emits runtime check"
    (let [code (analyze-and-emit '(satisfies? IGreeter x))]
      (is (clojure.string/includes? code "clel-satisfies-p"))
      (is (clojure.string/includes? code "'IGreeter"))
      (is (clojure.string/includes? code "x")))))

;; ============================================================================
;; reify (clel-025)
;; ============================================================================

(deftest emit-reify-test
  (testing "reify emits cl-defstruct and cl-defmethod"
    (let [code (analyze-and-emit '(reify IGreeter
                                    (greet [this] "hello")))]
      (is (clojure.string/includes? code "cl-defstruct"))
      (is (clojure.string/includes? code "clel--reify-"))
      (is (clojure.string/includes? code "cl-defmethod greet"))
      (is (clojure.string/includes? code "--create"))))

  (testing "reify with multiple methods"
    (let [code (analyze-and-emit '(reify IShape
                                    (area [this] 10)
                                    (perimeter [this] 20)))]
      (is (clojure.string/includes? code "cl-defmethod area"))
      (is (clojure.string/includes? code "cl-defmethod perimeter")))))

;; ============================================================================
;; Macro System (clel-027)
;; ============================================================================

(deftest emit-defmacro-test
  (testing "defmacro emits empty string (compile-time only)"
    (let [code (analyze-and-emit '(defmacro unless [pred body]
                                    (list 'if (list 'not pred) body nil)))]
      (is (= "" code))))

  (testing "defmacro with docstring emits empty string"
    (let [code (analyze-and-emit '(defmacro unless "Opposite of when" [pred body]
                                    (list 'if (list 'not pred) body nil)))]
      (is (= "" code))))

  (testing "macro usage emits expanded form, not macro call"
    (ana/clear-macros!)
    (analyze-and-emit '(defmacro double-it [x]
                         (list '* 2 x)))
    (let [code (analyze-and-emit '(double-it 5))]
      ;; Should emit (* 2 5), not (double-it 5)
      (is (clojure.string/includes? code "*"))
      (is (clojure.string/includes? code "2"))
      (is (clojure.string/includes? code "5"))
      (is (not (clojure.string/includes? code "double-it"))))))

;; ============================================================================
;; Namespace System - Qualified Var Emission (clel-028)
;; ============================================================================

(deftest emit-qualified-var-test
  (testing "var with :ns emits namespace-qualified mangled name"
    (let [node {:op :var :name 'join :ns 'clojure.string}
          code (emit/emit node)]
      ;; clojure.string/join is mapped to clel-str-join in core-fn-mapping
      (is (= "clel-str-join" code))))

  (testing "var without :ns uses core mapping or plain name"
    (let [node {:op :var :name 'first}
          code (emit/emit node)]
      (is (= "clel-first" code))))

  (testing "var with :ns for non-core namespace"
    (let [node {:op :var :name 'helper :ns 'my.utils}
          code (emit/emit node)]
      (is (= "my-utils-helper" code))))

  (testing "var with :ns clojure.core uses core mapping"
    (let [node {:op :var :name 'first :ns 'clojure.core}
          code (emit/emit node)]
      (is (= "clel-first" code)))))

(deftest emit-ns-definition-prefixing-test
  (testing "defn in namespace emits prefixed name"
    (let [forms     '[(ns my.app) (defn greet [name] name)]
          asts      (ana/analyze-file-forms forms)
          defn-code (emit/emit (second asts))]
      (is (clojure.string/includes? defn-code "defun"))
      (is (clojure.string/includes? defn-code "my-app-greet"))))

  (testing "def in namespace emits prefixed name"
    (let [forms    '[(ns my.app) (def pi 3.14)]
          asts     (ana/analyze-file-forms forms)
          def-code (emit/emit (second asts))]
      (is (clojure.string/includes? def-code "defvar"))
      (is (clojure.string/includes? def-code "my-app-pi"))))

  (testing "defn in user namespace has no prefix"
    (let [result (analyze-and-emit '(defn foo [x] x))]
      (is (clojure.string/includes? result "defun foo")))))

(deftest emit-ns-provide-test
  (testing "ns emission includes provide at end"
    (let [forms '[(ns my.app)]
          asts  (ana/analyze-file-forms forms)
          code  (emit/emit-file asts)]
      (is (clojure.string/includes? code "(provide 'my-app)"))
      ;; provide should be after the header
      (is (> (.indexOf code "(provide 'my-app)")
             (.indexOf code "lexical-binding"))))))

(deftest emit-alias-resolution-full-pipeline-test
  (testing "aliased call emits fully qualified name"
    (let [forms     '[(ns my.app
                        (:require [clojure.string :as str]))
                      (str/join ", " items)]
          asts      (ana/analyze-file-forms forms)
          call-code (emit/emit (second asts))]
      ;; clojure.string/join is mapped to clel-str-join
      (is (clojure.string/includes? call-code "clel-str-join"))))

  (testing "referred symbol emits fully qualified name"
    (let [forms     '[(ns my.app
                        (:require [my.utils :refer [helper]]))
                      (helper 42)]
          asts      (ana/analyze-file-forms forms)
          call-code (emit/emit (second asts))]
      (is (clojure.string/includes? call-code "my-utils-helper")))))

;; ============================================================================
;; with-eval-after-load (clel-033)
;; ============================================================================

(deftest emit-with-eval-after-load-test
  (testing "basic with-eval-after-load emits Elisp form"
    (let [code (analyze-and-emit '(with-eval-after-load 'hive-mcp-addons
                                    (register-addon 'eca)))]
      (is (clojure.string/includes? code "with-eval-after-load"))
      (is (clojure.string/includes? code "'hive-mcp-addons"))
      (is (clojure.string/includes? code "register-addon"))))

  (testing "with-eval-after-load with multiple body forms"
    (let [code (analyze-and-emit '(with-eval-after-load 'my-feature
                                    (setup)
                                    (configure :opt 1)))]
      (is (clojure.string/includes? code "with-eval-after-load"))
      (is (clojure.string/includes? code "setup"))
      (is (clojure.string/includes? code "configure"))))

  (testing "with-eval-after-load with string feature"
    (let [code (analyze-and-emit '(with-eval-after-load "my-package"
                                    (init)))]
      (is (clojure.string/includes? code "with-eval-after-load"))
      (is (clojure.string/includes? code "\"my-package\""))))

  (testing "with-eval-after-load preserves keyword args"
    (let [code (analyze-and-emit '(with-eval-after-load 'hive-mcp-addons
                                    (hive-mcp-addon-register
                                     'eca
                                     :version "0.1.0"
                                     :description "ECA integration")))]
      (is (clojure.string/includes? code "with-eval-after-load"))
      (is (clojure.string/includes? code ":version"))
      (is (clojure.string/includes? code "\"0.1.0\""))
      (is (clojure.string/includes? code ":description")))))

;; ============================================================================
;; define-minor-mode (clel-032)
;; ============================================================================

(deftest emit-define-minor-mode-test
  (testing "basic define-minor-mode emits Elisp form"
    (let [code (analyze-and-emit '(define-minor-mode my-mode
                                    "A test mode."
                                    :init-value nil
                                    :lighter " M"))]
      (is (clojure.string/includes? code "define-minor-mode"))
      (is (clojure.string/includes? code "my-mode"))
      (is (clojure.string/includes? code "\"A test mode.\""))
      (is (clojure.string/includes? code ":init-value nil"))
      (is (clojure.string/includes? code ":lighter \" M\""))))

  (testing "define-minor-mode with :global option"
    (let [code (analyze-and-emit '(define-minor-mode global-mode
                                    "Global mode."
                                    :global t))]
      (is (clojure.string/includes? code "define-minor-mode"))
      (is (clojure.string/includes? code ":global t"))))

  (testing "define-minor-mode with :group option"
    (let [code (analyze-and-emit '(define-minor-mode hive-mcp-eca-mode
                                    "ECA mode."
                                    :group 'hive-mcp-eca))]
      (is (clojure.string/includes? code "define-minor-mode"))
      (is (clojure.string/includes? code ":group"))
      (is (clojure.string/includes? code "'hive-mcp-eca"))))

  (testing "define-minor-mode with body"
    (let [code (analyze-and-emit '(define-minor-mode my-mode
                                    "Toggle mode."
                                    :lighter " M"
                                    (if my-mode
                                      (my-enable)
                                      (my-disable))))]
      (is (clojure.string/includes? code "define-minor-mode"))
      (is (clojure.string/includes? code "(if my-mode"))
      (is (clojure.string/includes? code "(my-enable)"))
      (is (clojure.string/includes? code "(my-disable)"))))

  (testing "define-minor-mode without docstring"
    (let [code (analyze-and-emit '(define-minor-mode simple-mode
                                    :lighter " S"))]
      (is (clojure.string/includes? code "define-minor-mode"))
      (is (clojure.string/includes? code "simple-mode"))
      (is (clojure.string/includes? code ":lighter \" S\""))))

  (testing "define-minor-mode full example"
    (let [code (analyze-and-emit '(define-minor-mode hive-mcp-eca-mode
                                    "Minor mode for ECA integration."
                                    :init-value nil
                                    :lighter " ECA"
                                    :global t
                                    :group 'hive-mcp-eca
                                    (if hive-mcp-eca-mode
                                      (hive-mcp-eca--enable)
                                      (hive-mcp-eca--disable))))]
      (is (clojure.string/includes? code "define-minor-mode"))
      (is (clojure.string/includes? code "hive-mcp-eca-mode"))
      (is (clojure.string/includes? code "\"Minor mode for ECA integration.\""))
      (is (clojure.string/includes? code ":init-value nil"))
      (is (clojure.string/includes? code ":lighter \" ECA\""))
      (is (clojure.string/includes? code ":global t"))
      (is (clojure.string/includes? code ":group"))
      (is (clojure.string/includes? code "(if hive-mcp-eca-mode")))))

;; ============================================================================
;; defgroup - Customization Groups (clel-030)
;; ============================================================================

(deftest emit-defgroup-test
  (testing "basic defgroup emits Elisp defgroup form"
    (let [code (analyze-and-emit '(defgroup hive-mcp-eca nil
                                    "Integration between hive-mcp and ECA."))]
      (is (clojure.string/includes? code "defgroup"))
      (is (clojure.string/includes? code "hive-mcp-eca"))
      (is (clojure.string/includes? code "nil"))
      (is (clojure.string/includes? code "\"Integration between hive-mcp and ECA.\""))))

  (testing "defgroup with :group option"
    (let [code (analyze-and-emit '(defgroup my-package nil
                                    "My package customizations."
                                    :group 'emacs))]
      (is (clojure.string/includes? code "defgroup"))
      (is (clojure.string/includes? code "my-package"))
      (is (clojure.string/includes? code ":group"))
      (is (clojure.string/includes? code "'emacs"))))

  (testing "defgroup with :prefix option"
    (let [code (analyze-and-emit '(defgroup my-package nil
                                    "My package."
                                    :prefix "my-package-"))]
      (is (clojure.string/includes? code "defgroup"))
      (is (clojure.string/includes? code ":prefix"))
      (is (clojure.string/includes? code "\"my-package-\""))))

  (testing "defgroup with multiple options"
    (let [code (analyze-and-emit '(defgroup hive-mcp-eca nil
                                    "Integration docs."
                                    :group 'hive-mcp
                                    :prefix "hive-mcp-eca-"
                                    :tag "HiveMCP ECA"))]
      (is (clojure.string/includes? code "defgroup"))
      (is (clojure.string/includes? code "hive-mcp-eca"))
      (is (clojure.string/includes? code ":group"))
      (is (clojure.string/includes? code "'hive-mcp"))
      (is (clojure.string/includes? code ":prefix"))
      (is (clojure.string/includes? code "\"hive-mcp-eca-\""))
      (is (clojure.string/includes? code ":tag"))
      (is (clojure.string/includes? code "\"HiveMCP ECA\""))))

  (testing "defgroup without docstring"
    (let [code (analyze-and-emit '(defgroup simple-group nil))]
      (is (clojure.string/includes? code "defgroup"))
      (is (clojure.string/includes? code "simple-group"))
      (is (clojure.string/includes? code "nil"))))

  (testing "defgroup full example from spec"
    (let [code (analyze-and-emit '(defgroup hive-mcp-eca nil
                                    "Integration between hive-mcp and ECA."
                                    :group 'hive-mcp
                                    :prefix "hive-mcp-eca-"))]
      (is (clojure.string/includes? code "(defgroup hive-mcp-eca nil"))
      (is (clojure.string/includes? code "\"Integration between hive-mcp and ECA.\""))
      (is (clojure.string/includes? code ":group 'hive-mcp"))
      (is (clojure.string/includes? code ":prefix \"hive-mcp-eca-\"")))))

;; ============================================================================
;; defcustom - User Customization Variables (clel-031)
;; ============================================================================

(deftest emit-defcustom-test
  (testing "basic defcustom emits Elisp defcustom form"
    (let [code (analyze-and-emit '(defcustom hive-mcp-eca-auto-context nil
                                    "When non-nil, automatically include MCP context."))]
      (is (clojure.string/includes? code "defcustom"))
      (is (clojure.string/includes? code "hive-mcp-eca-auto-context"))
      (is (clojure.string/includes? code "nil"))
      (is (clojure.string/includes? code "\"When non-nil, automatically include MCP context.\""))))

  (testing "defcustom with integer default"
    (let [code (analyze-and-emit '(defcustom hive-mcp-eca-timeout 30
                                    "Timeout in seconds."))]
      (is (clojure.string/includes? code "defcustom"))
      (is (clojure.string/includes? code "hive-mcp-eca-timeout"))
      (is (clojure.string/includes? code "30"))))

  (testing "defcustom with :type option"
    (let [code (analyze-and-emit '(defcustom my-var nil
                                    "My variable."
                                    :type 'boolean))]
      (is (clojure.string/includes? code "defcustom"))
      (is (clojure.string/includes? code ":type"))
      (is (clojure.string/includes? code "'boolean"))))

  (testing "defcustom with :group option"
    (let [code (analyze-and-emit '(defcustom my-var nil
                                    "My variable."
                                    :group 'my-group))]
      (is (clojure.string/includes? code "defcustom"))
      (is (clojure.string/includes? code ":group"))
      (is (clojure.string/includes? code "'my-group"))))

  (testing "defcustom with multiple options"
    (let [code (analyze-and-emit '(defcustom hive-mcp-eca-timeout 30
                                    "Timeout in seconds."
                                    :type 'integer
                                    :group 'hive-mcp-eca
                                    :safe 'integerp))]
      (is (clojure.string/includes? code "defcustom"))
      (is (clojure.string/includes? code "hive-mcp-eca-timeout"))
      (is (clojure.string/includes? code "30"))
      (is (clojure.string/includes? code ":type"))
      (is (clojure.string/includes? code "'integer"))
      (is (clojure.string/includes? code ":group"))
      (is (clojure.string/includes? code "'hive-mcp-eca"))
      (is (clojure.string/includes? code ":safe"))
      (is (clojure.string/includes? code "'integerp"))))

  (testing "defcustom with string default"
    (let [code (analyze-and-emit '(defcustom my-prefix "prefix-"
                                    "The prefix to use."
                                    :type 'string))]
      (is (clojure.string/includes? code "defcustom"))
      (is (clojure.string/includes? code "\"prefix-\""))
      (is (clojure.string/includes? code ":type 'string"))))

  (testing "defcustom without docstring"
    (let [code (analyze-and-emit '(defcustom simple-var true))]
      (is (clojure.string/includes? code "defcustom"))
      (is (clojure.string/includes? code "simple-var"))
      (is (clojure.string/includes? code " t"))))

  (testing "defcustom full example from spec"
    (let [code (analyze-and-emit '(defcustom hive-mcp-eca-auto-context nil
                                    "When non-nil, automatically include MCP context."
                                    :type 'boolean
                                    :group 'hive-mcp-eca))]
      (is (clojure.string/includes? code "(defcustom hive-mcp-eca-auto-context nil"))
      (is (clojure.string/includes? code "\"When non-nil, automatically include MCP context.\""))
      (is (clojure.string/includes? code ":type 'boolean"))
      (is (clojure.string/includes? code ":group 'hive-mcp-eca")))))

;; ============================================================================
;; Emacs Buffer/Process Interop (clel-031)
;; ============================================================================

(deftest emit-buffer-function-mappings-test
  (testing "buffer-string maps directly"
    (is (= "(buffer-string)" (analyze-and-emit '(buffer-string)))))

  (testing "buffer-substring maps directly"
    (is (= "(buffer-substring start end)" (analyze-and-emit '(buffer-substring start end)))))

  (testing "point maps directly"
    (is (= "(point)" (analyze-and-emit '(point)))))

  (testing "point-min maps directly"
    (is (= "(point-min)" (analyze-and-emit '(point-min)))))

  (testing "point-max maps directly"
    (is (= "(point-max)" (analyze-and-emit '(point-max)))))

  (testing "goto-char maps directly"
    (is (= "(goto-char pos)" (analyze-and-emit '(goto-char pos)))))

  (testing "insert maps directly"
    (is (= "(insert \"hello\")" (analyze-and-emit '(insert "hello")))))

  (testing "erase-buffer maps directly"
    (is (= "(erase-buffer)" (analyze-and-emit '(erase-buffer)))))

  (testing "delete-region maps directly"
    (is (= "(delete-region start end)" (analyze-and-emit '(delete-region start end)))))

  (testing "narrow-to-region maps directly"
    (is (= "(narrow-to-region start end)" (analyze-and-emit '(narrow-to-region start end)))))

  (testing "widen maps directly"
    (is (= "(widen)" (analyze-and-emit '(widen))))))

(deftest emit-process-function-mappings-test
  (testing "start-process maps directly"
    (is (= "(start-process \"name\" nil \"cmd\")"
           (analyze-and-emit '(start-process "name" nil "cmd")))))

  (testing "process-send-string maps directly"
    (is (= "(process-send-string proc \"input\")"
           (analyze-and-emit '(process-send-string proc "input")))))

  (testing "set-process-filter maps directly"
    (is (= "(set-process-filter proc handler)"
           (analyze-and-emit '(set-process-filter proc handler)))))

  (testing "process-live-p maps directly"
    (is (= "(process-live-p proc)"
           (analyze-and-emit '(process-live-p proc))))))

(deftest emit-file-function-mappings-test
  (testing "find-file maps directly"
    (is (= "(find-file path)" (analyze-and-emit '(find-file path)))))

  (testing "find-file-noselect maps directly"
    (is (= "(find-file-noselect path)" (analyze-and-emit '(find-file-noselect path)))))

  (testing "file-exists-p maps directly"
    (is (= "(file-exists-p path)" (analyze-and-emit '(file-exists-p path)))))

  (testing "expand-file-name maps directly"
    (is (= "(expand-file-name path)" (analyze-and-emit '(expand-file-name path))))))

(deftest emit-save-excursion-test
  (testing "basic save-excursion emits Elisp form"
    (let [code (analyze-and-emit '(save-excursion
                                   (goto-char (point-min))
                                   (insert "hello")))]
      (is (clojure.string/includes? code "(save-excursion"))
      (is (clojure.string/includes? code "(goto-char (point-min))"))
      (is (clojure.string/includes? code "(insert \"hello\")"))))

  (testing "save-excursion with single body form"
    (let [code (analyze-and-emit '(save-excursion (point)))]
      (is (clojure.string/includes? code "(save-excursion"))
      (is (clojure.string/includes? code "(point)")))))

(deftest emit-save-restriction-test
  (testing "basic save-restriction emits Elisp form"
    (let [code (analyze-and-emit '(save-restriction
                                   (narrow-to-region start end)
                                   (do-something)))]
      (is (clojure.string/includes? code "(save-restriction"))
      (is (clojure.string/includes? code "(narrow-to-region start end)"))
      (is (clojure.string/includes? code "(do-something)"))))

  (testing "save-restriction with widen"
    (let [code (analyze-and-emit '(save-restriction (widen)))]
      (is (clojure.string/includes? code "(save-restriction"))
      (is (clojure.string/includes? code "(widen)")))))

(deftest emit-with-current-buffer-test
  (testing "basic with-current-buffer emits Elisp form"
    (let [code (analyze-and-emit '(with-current-buffer buf
                                    (buffer-string)))]
      (is (clojure.string/includes? code "(with-current-buffer buf"))
      (is (clojure.string/includes? code "(buffer-string)"))))

  (testing "with-current-buffer with buffer name string"
    (let [code (analyze-and-emit '(with-current-buffer "*scratch*"
                                    (erase-buffer)))]
      (is (clojure.string/includes? code "(with-current-buffer \"*scratch*\""))
      (is (clojure.string/includes? code "(erase-buffer)"))))

  (testing "with-current-buffer with get-buffer-create"
    (let [code (analyze-and-emit '(with-current-buffer (get-buffer-create "*log*")
                                    (goto-char (point-max))
                                    (insert "log entry")))]
      (is (clojure.string/includes? code "(with-current-buffer (get-buffer-create \"*log*\")"))
      (is (clojure.string/includes? code "(goto-char (point-max))"))
      (is (clojure.string/includes? code "(insert \"log entry\")")))))

(deftest emit-with-temp-buffer-test
  (testing "basic with-temp-buffer emits Elisp form"
    (let [code (analyze-and-emit '(with-temp-buffer
                                    (insert "temp content")
                                    (buffer-string)))]
      (is (clojure.string/includes? code "(with-temp-buffer"))
      (is (clojure.string/includes? code "(insert \"temp content\")"))
      (is (clojure.string/includes? code "(buffer-string)"))))

  (testing "with-temp-buffer single form"
    (let [code (analyze-and-emit '(with-temp-buffer (point-max)))]
      (is (clojure.string/includes? code "(with-temp-buffer"))
      (is (clojure.string/includes? code "(point-max)")))))

(deftest emit-save-current-buffer-test
  (testing "basic save-current-buffer emits Elisp form"
    (let [code (analyze-and-emit '(save-current-buffer
                                   (set-buffer other-buf)
                                   (do-work)))]
      (is (clojure.string/includes? code "(save-current-buffer"))
      (is (clojure.string/includes? code "(set-buffer other-buf)"))
      (is (clojure.string/includes? code "(do-work)")))))

(deftest emit-with-output-to-string-test
  (testing "basic with-output-to-string emits Elisp form"
    (let [code (analyze-and-emit '(with-output-to-string
                                    (princ "output")))]
      (is (clojure.string/includes? code "(with-output-to-string"))
      (is (clojure.string/includes? code "(princ \"output\")"))))

  (testing "with-output-to-string multiple forms"
    (let [code (analyze-and-emit '(with-output-to-string
                                    (princ "hello ")
                                    (princ "world")))]
      (is (clojure.string/includes? code "(with-output-to-string"))
      (is (clojure.string/includes? code "hello"))
      (is (clojure.string/includes? code "world")))))

(deftest emit-combined-buffer-ops-test
  (testing "nested save-excursion and with-current-buffer"
    (let [code (analyze-and-emit '(save-excursion
                                   (with-current-buffer other
                                     (goto-char (point-min)))))]
      (is (clojure.string/includes? code "(save-excursion"))
      (is (clojure.string/includes? code "(with-current-buffer other"))
      (is (clojure.string/includes? code "(goto-char (point-min))"))))

  (testing "save-restriction inside save-excursion"
    (let [code (analyze-and-emit '(save-excursion
                                   (save-restriction
                                    (narrow-to-region start end)
                                    (buffer-string))))]
      (is (clojure.string/includes? code "(save-excursion"))
      (is (clojure.string/includes? code "(save-restriction"))
      (is (clojure.string/includes? code "(narrow-to-region start end)"))
      (is (clojure.string/includes? code "(buffer-string)")))))

;; ============================================================================
;; Iteration Forms - doseq/dotimes (clel-035)
;; ============================================================================

(deftest emit-doseq-test
  (testing "basic doseq emits dolist with clel-seq"
    (let [code (analyze-and-emit '(doseq [x items]
                                    (println x)))]
      (is (clojure.string/includes? code "(dolist"))
      (is (clojure.string/includes? code "(clel-seq items)"))
      (is (clojure.string/includes? code "(message x)"))))

  (testing "doseq with vector literal collection"
    (let [code (analyze-and-emit '(doseq [x [1 2 3]]
                                    (process x)))]
      (is (clojure.string/includes? code "(dolist"))
      (is (clojure.string/includes? code "(clel-seq (list 1 2 3))"))
      (is (clojure.string/includes? code "(process x)"))))

  (testing "doseq with multiple body forms"
    (let [code (analyze-and-emit '(doseq [item coll]
                                    (println "processing")
                                    (process item)))]
      (is (clojure.string/includes? code "(dolist (item (clel-seq coll))"))
      (is (clojure.string/includes? code "(message \"processing\")"))
      (is (clojure.string/includes? code "(process item)"))))

  (testing "doseq binding name is mangled"
    (let [code (analyze-and-emit '(doseq [my-item? items]
                                    (use my-item?)))]
      (is (clojure.string/includes? code "(dolist (my-item-p")))))

(deftest emit-dotimes-test
  (testing "basic dotimes emits cl-dotimes"
    (let [code (analyze-and-emit '(dotimes [i 5]
                                    (println i)))]
      (is (clojure.string/includes? code "(cl-dotimes"))
      (is (clojure.string/includes? code "(i 5)"))
      (is (clojure.string/includes? code "(message i)"))))

  (testing "dotimes with var count"
    (let [code (analyze-and-emit '(dotimes [i n]
                                    (process i)))]
      (is (clojure.string/includes? code "(cl-dotimes (i n)"))
      (is (clojure.string/includes? code "(process i)"))))

  (testing "dotimes with expression count"
    (let [code (analyze-and-emit '(dotimes [i (count items)]
                                    (process i)))]
      (is (clojure.string/includes? code "(cl-dotimes (i (length items))"))
      (is (clojure.string/includes? code "(process i)"))))

  (testing "dotimes with multiple body forms"
    (let [code (analyze-and-emit '(dotimes [i 3]
                                    (println "iteration")
                                    (do-work i)))]
      (is (clojure.string/includes? code "(cl-dotimes (i 3)"))
      (is (clojure.string/includes? code "(message \"iteration\")"))
      (is (clojure.string/includes? code "(do-work i)"))))

  (testing "dotimes binding name is mangled"
    (let [code (analyze-and-emit '(dotimes [loop-count! 10]
                                    (use loop-count!)))]
      (is (clojure.string/includes? code "(cl-dotimes (loop-count-bang 10)")))))

;; ============================================================================
;; For List Comprehension (clel-039)
;; ============================================================================

(deftest emit-for-basic-test
  (testing "basic for emits cl-mapcan with list wrapper"
    (let [code (analyze-and-emit '(for [x coll]
                                    (inc x)))]
      (is (clojure.string/includes? code "(cl-mapcan"))
      (is (clojure.string/includes? code "(lambda (x)"))
      (is (clojure.string/includes? code "(list (1+ x))"))
      (is (clojure.string/includes? code "(clel-seq coll)"))))

  (testing "for with literal collection"
    (let [code (analyze-and-emit '(for [x [1 2 3]]
                                    (* x 2)))]
      (is (clojure.string/includes? code "(cl-mapcan"))
      (is (clojure.string/includes? code "(lambda (x)"))
      (is (clojure.string/includes? code "(list (* x 2))"))))

  (testing "for binding name is mangled"
    (let [code (analyze-and-emit '(for [item? items]
                                    item?))]
      (is (clojure.string/includes? code "(lambda (item-p)")))))

(deftest emit-for-when-test
  (testing "for with :when emits cl-mapcan"
    (let [code (analyze-and-emit '(for [x coll :when (even? x)]
                                    x))]
      (is (clojure.string/includes? code "(cl-mapcan"))
      (is (clojure.string/includes? code "(lambda (x)"))
      (is (clojure.string/includes? code "(when (cl-evenp x)"))
      (is (clojure.string/includes? code "(list x)"))))

  (testing "for with :when filters correctly"
    (let [code (analyze-and-emit '(for [n numbers :when (pos? n)]
                                    (str n)))]
      (is (clojure.string/includes? code "(cl-mapcan"))
      (is (clojure.string/includes? code "(when (cl-plusp n)"))
      (is (clojure.string/includes? code "(list (clel-str n))")))))

(deftest emit-for-let-test
  (testing "for with :let emits let* wrapper"
    (let [code (analyze-and-emit '(for [x coll :let [y (inc x)]]
                                    y))]
      (is (clojure.string/includes? code "(cl-mapcan"))
      (is (clojure.string/includes? code "(let* ((y (1+ x)))"))
      (is (clojure.string/includes? code "(list y)"))))

  (testing "for with multiple :let bindings"
    (let [code (analyze-and-emit '(for [x coll :let [y (inc x) z (* x 2)]]
                                    (+ y z)))]
      (is (clojure.string/includes? code "(cl-mapcan"))
      (is (clojure.string/includes? code "(let*"))
      (is (clojure.string/includes? code "(y (1+ x))"))
      (is (clojure.string/includes? code "(z (* x 2))"))
      (is (clojure.string/includes? code "(list (+ y z))")))))

(deftest emit-for-combined-test
  (testing "for with :when and :let"
    (let [code (analyze-and-emit '(for [x coll :when (pos? x) :let [y (* x 2)]]
                                    y))]
      (is (clojure.string/includes? code "(cl-mapcan"))
      (is (clojure.string/includes? code "(when (cl-plusp x)"))
      (is (clojure.string/includes? code "(let* ((y (* x 2)))"))
      ;; The list call is inside the let wrapper
      (is (clojure.string/includes? code "(list y)")))))

;; ============================================================================
;; Set Operations (clel-044)
;; ============================================================================

(deftest emit-set-constructor-test
  (testing "set constructor maps to clel-set-from-coll"
    (is (= "(clel-set-from-coll xs)" (analyze-and-emit '(set xs)))))

  (testing "set with literal collection"
    (let [code (analyze-and-emit '(set [1 2 3]))]
      (is (clojure.string/includes? code "clel-set-from-coll"))))

  (testing "hash-set maps to clel-set"
    (is (clojure.string/includes? (analyze-and-emit '(hash-set 1 2 3)) "clel-set")))

  (testing "set? predicate maps to clel-set-p"
    (is (= "(clel-set-p x)" (analyze-and-emit '(set? x)))))

  (testing "disj maps to clel-set-remove"
    (is (= "(clel-set-remove s :a)" (analyze-and-emit '(disj s :a))))))

(deftest emit-set-union-test
  (testing "clojure.set/union maps to clel-set-union"
    (let [code (analyze-and-emit '(clojure.set/union s1 s2))]
      (is (clojure.string/includes? code "clel-set-union"))))

  (testing "union with multiple sets"
    (let [code (analyze-and-emit '(clojure.set/union a b c))]
      (is (clojure.string/includes? code "clel-set-union"))
      (is (clojure.string/includes? code "a"))
      (is (clojure.string/includes? code "b"))
      (is (clojure.string/includes? code "c")))))

(deftest emit-set-intersection-test
  (testing "clojure.set/intersection maps to clel-set-intersection"
    (let [code (analyze-and-emit '(clojure.set/intersection s1 s2))]
      (is (clojure.string/includes? code "clel-set-intersection"))))

  (testing "intersection with multiple sets"
    (let [code (analyze-and-emit '(clojure.set/intersection a b c))]
      (is (clojure.string/includes? code "clel-set-intersection")))))

(deftest emit-set-difference-test
  (testing "clojure.set/difference maps to clel-set-difference"
    (let [code (analyze-and-emit '(clojure.set/difference s1 s2))]
      (is (clojure.string/includes? code "clel-set-difference"))))

  (testing "difference with multiple sets"
    (let [code (analyze-and-emit '(clojure.set/difference a b c))]
      (is (clojure.string/includes? code "clel-set-difference")))))

(deftest emit-set-subset-superset-test
  (testing "clojure.set/subset? maps to clel-set-subset-p"
    (let [code (analyze-and-emit '(clojure.set/subset? s1 s2))]
      (is (clojure.string/includes? code "clel-set-subset-p"))))

  (testing "clojure.set/superset? maps to clel-set-superset-p"
    (let [code (analyze-and-emit '(clojure.set/superset? s1 s2))]
      (is (clojure.string/includes? code "clel-set-superset-p")))))

(deftest emit-set-select-test
  (testing "clojure.set/select maps to clel-set-select"
    (let [code (analyze-and-emit '(clojure.set/select even? s))]
      (is (clojure.string/includes? code "clel-set-select"))))

  (testing "select with lambda predicate"
    (let [code (analyze-and-emit '(clojure.set/select (fn [x] (> x 0)) s))]
      (is (clojure.string/includes? code "clel-set-select"))
      (is (clojure.string/includes? code "lambda")))))

(deftest emit-set-relational-test
  (testing "clojure.set/project maps to clel-set-project"
    (let [code (analyze-and-emit '(clojure.set/project xrel [:a :b]))]
      (is (clojure.string/includes? code "clel-set-project"))))

  (testing "clojure.set/rename maps to clel-set-rename"
    (let [code (analyze-and-emit '(clojure.set/rename xrel {:old :new}))]
      (is (clojure.string/includes? code "clel-set-rename"))))

  (testing "clojure.set/rename-keys maps to clel-rename-keys"
    (let [code (analyze-and-emit '(clojure.set/rename-keys m {:old :new}))]
      (is (clojure.string/includes? code "clel-rename-keys"))))

  (testing "clojure.set/join maps to clel-set-join"
    (let [code (analyze-and-emit '(clojure.set/join xrel yrel))]
      (is (clojure.string/includes? code "clel-set-join"))))

  (testing "clojure.set/index maps to clel-set-index"
    (let [code (analyze-and-emit '(clojure.set/index xrel [:a]))]
      (is (clojure.string/includes? code "clel-set-index"))))

  (testing "clojure.set/map-invert maps to clel-map-invert"
    (let [code (analyze-and-emit '(clojure.set/map-invert m))]
      (is (clojure.string/includes? code "clel-map-invert")))))

;; ============================================================================
;; Transducers (clel-043)
;; ============================================================================

(deftest emit-transduce-test
  (testing "transduce maps to clel-transduce"
    (let [code (analyze-and-emit '(transduce xform f coll))]
      (is (clojure.string/includes? code "clel-transduce"))))

  (testing "transduce with init value"
    (let [code (analyze-and-emit '(transduce xform f init coll))]
      (is (clojure.string/includes? code "clel-transduce"))
      (is (clojure.string/includes? code "xform"))
      (is (clojure.string/includes? code "f"))
      (is (clojure.string/includes? code "init"))
      (is (clojure.string/includes? code "coll"))))

  (testing "transduce with comp xform"
    (let [code (analyze-and-emit '(transduce (comp (map inc) (filter even?)) + coll))]
      (is (clojure.string/includes? code "clel-transduce"))
      (is (clojure.string/includes? code "clel-comp"))
      (is (clojure.string/includes? code "clel-map"))
      (is (clojure.string/includes? code "clel-filter")))))

(deftest emit-keep-test
  (testing "keep maps to clel-keep"
    (let [code (analyze-and-emit '(keep identity coll))]
      (is (clojure.string/includes? code "clel-keep"))))

  (testing "keep with lambda"
    (let [code (analyze-and-emit '(keep (fn [x] (when (pos? x) x)) nums))]
      (is (clojure.string/includes? code "clel-keep"))
      (is (clojure.string/includes? code "lambda"))))

  (testing "keep-indexed maps to clel-keep-indexed"
    (let [code (analyze-and-emit '(keep-indexed (fn [i x] x) coll))]
      (is (clojure.string/includes? code "clel-keep-indexed")))))

(deftest emit-reduced-test
  (testing "reduced maps to clel-reduced"
    (let [code (analyze-and-emit '(reduced x))]
      (is (= "(clel-reduced x)" code))))

  (testing "reduced? maps to clel-reduced-p"
    (let [code (analyze-and-emit '(reduced? x))]
      (is (= "(clel-reduced-p x)" code))))

  (testing "unreduced maps to clel-unreduced"
    (let [code (analyze-and-emit '(unreduced x))]
      (is (= "(clel-unreduced x)" code)))))

(deftest emit-eduction-test
  (testing "eduction maps to clel-eduction"
    (let [code (analyze-and-emit '(eduction xform coll))]
      (is (clojure.string/includes? code "clel-eduction"))))

  (testing "eduction with composed xform"
    (let [code (analyze-and-emit '(eduction (comp (map inc) (take 5)) data))]
      (is (clojure.string/includes? code "clel-eduction")))))

(deftest emit-cat-test
  (testing "cat maps to clel-cat-xf"
    (let [code (analyze-and-emit '(transduce cat conj nested))]
      (is (clojure.string/includes? code "clel-cat-xf")))))

(deftest emit-transducer-in-context-test
  (testing "transducer with reduce as step function"
    (let [code (analyze-and-emit '(transduce (map inc) + 0 [1 2 3]))]
      (is (clojure.string/includes? code "clel-transduce"))
      (is (clojure.string/includes? code "clel-map"))))

  (testing "transducer composition with comp"
    (let [code (analyze-and-emit '(let [xf (comp (filter even?) (take 5))]
                                    (transduce xf conj [] data)))]
      (is (clojure.string/includes? code "clel-comp"))
      (is (clojure.string/includes? code "clel-filter"))
      (is (clojure.string/includes? code "clel-take"))
      (is (clojure.string/includes? code "clel-transduce")))))

;; ============================================================================
;; Multi-binding for/doseq (clel-045)
;; ============================================================================

(deftest emit-for-multi-binding-test
  (testing "for with two bindings creates nested cl-mapcan"
    (let [code (analyze-and-emit '(for [x [1 2] y [3 4]] [x y]))]
      (is (clojure.string/includes? code "(cl-mapcan"))
      (is (clojure.string/includes? code "(lambda (x)"))
      (is (clojure.string/includes? code "(lambda (y)"))
      ;; Should have nested mapcan for cartesian product
      (is (= 2 (count (re-seq #"cl-mapcan" code))))))

  (testing "for with three bindings creates triple nesting"
    (let [code (analyze-and-emit '(for [x xs y ys z zs] [x y z]))]
      (is (= 3 (count (re-seq #"cl-mapcan" code))))
      (is (clojure.string/includes? code "(lambda (x)"))
      (is (clojure.string/includes? code "(lambda (y)"))
      (is (clojure.string/includes? code "(lambda (z)"))))

  (testing "multi-binding for with :when between bindings"
    (let [code (analyze-and-emit '(for [x xs :when (pos? x) y ys] [x y]))]
      (is (clojure.string/includes? code "(cl-mapcan"))
      (is (clojure.string/includes? code "(when (cl-plusp x)"))
      ;; Two mapcan levels + when filter
      (is (= 2 (count (re-seq #"cl-mapcan" code))))))

  (testing "multi-binding for with :let between bindings"
    (let [code (analyze-and-emit '(for [x xs :let [x2 (* x 2)] y ys] [x2 y]))]
      (is (clojure.string/includes? code "(cl-mapcan"))
      (is (clojure.string/includes? code "(let*"))
      (is (clojure.string/includes? code "(x2 (* x 2))"))))

  (testing "complex multi-binding for with :when and :let"
    (let [code (analyze-and-emit '(for [x     [1 2 3]
                                        y     [4 5 6]
                                        :when (even? (+ x y))
                                        :let  [z (* x y)]]
                                    [x y z]))]
      (is (clojure.string/includes? code "(cl-mapcan"))
      (is (clojure.string/includes? code "(lambda (x)"))
      (is (clojure.string/includes? code "(lambda (y)"))
      (is (clojure.string/includes? code "(when (cl-evenp (+ x y))"))
      (is (clojure.string/includes? code "(let* ((z (* x y)))"))
      (is (clojure.string/includes? code "(list (list x y z))")))))

(deftest emit-doseq-multi-binding-test
  (testing "doseq with two bindings creates nested dolist"
    (let [code (analyze-and-emit '(doseq [x [1 2] y [3 4]] (process x y)))]
      (is (clojure.string/includes? code "(dolist"))
      (is (clojure.string/includes? code "(x (clel-seq"))
      (is (clojure.string/includes? code "(y (clel-seq"))
      ;; Should have nested dolist
      (is (= 2 (count (re-seq #"dolist" code))))))

  (testing "doseq with three bindings creates triple nesting"
    (let [code (analyze-and-emit '(doseq [x xs y ys z zs] (process x y z)))]
      (is (= 3 (count (re-seq #"dolist" code))))))

  (testing "multi-binding doseq with :when between bindings"
    (let [code (analyze-and-emit '(doseq [x xs :when (pos? x) y ys] (process x y)))]
      (is (clojure.string/includes? code "(dolist"))
      (is (clojure.string/includes? code "(when (cl-plusp x)"))
      (is (= 2 (count (re-seq #"dolist" code))))))

  (testing "multi-binding doseq with :let between bindings"
    (let [code (analyze-and-emit '(doseq [x xs :let [x2 (* x 2)] y ys] (process x2 y)))]
      (is (clojure.string/includes? code "(dolist"))
      (is (clojure.string/includes? code "(let*"))
      (is (clojure.string/includes? code "(x2 (* x 2))"))))

  (testing "complex multi-binding doseq"
    (let [code (analyze-and-emit '(doseq [x     items
                                          :when (valid? x)
                                          y     (children x)
                                          :let  [name (get-name y)]]
                                    (println name)))]
      (is (clojure.string/includes? code "(dolist"))
      (is (clojure.string/includes? code "(when (valid-p x)"))
      (is (clojure.string/includes? code "(let*"))
      (is (clojure.string/includes? code "(name (get-name y))")))))

(deftest emit-for-while-test
  (testing "for with :while emits as when (approximation for map-based iteration)"
    (let [code (analyze-and-emit '(for [x xs :while (pos? x)] x))]
      ;; Note: :while is approximated as :when since map-based for
      ;; doesn't support true early termination
      (is (clojure.string/includes? code "(cl-mapcan"))
      (is (clojure.string/includes? code "(when (cl-plusp x)"))))

  (testing "for with :while and other modifiers"
    (let [code (analyze-and-emit '(for [x xs :while (pos? x) :let [y (* x 2)]] y))]
      (is (clojure.string/includes? code "(cl-mapcan"))
      (is (clojure.string/includes? code "(when"))
      (is (clojure.string/includes? code "(let*")))))

(deftest emit-doseq-while-test
  (testing "doseq with :while uses cl-block for early termination"
    (let [code (analyze-and-emit '(doseq [x xs :while (pos? x)] (process x)))]
      (is (clojure.string/includes? code "(cl-block nil"))
      (is (clojure.string/includes? code "(unless (cl-plusp x) (cl-return))"))))

  (testing "doseq with :while and multiple bindings"
    (let [code (analyze-and-emit '(doseq [x xs :while (< (count processed) 10) y ys] (process x y)))]
      (is (clojure.string/includes? code "(cl-block nil"))
      (is (clojure.string/includes? code "(dolist")))))

;; ============================================================================
;; New Mappings (clel-050)
;; ============================================================================

(deftest emit-utility-mappings-test
  (testing "zipmap maps to clel-zipmap"
    (is (clojure.string/includes? (analyze-and-emit '(zipmap [:a :b] [1 2])) "clel-zipmap")))
  (testing "select-keys maps to clel-select-keys"
    (is (clojure.string/includes? (analyze-and-emit '(select-keys m [:a])) "clel-select-keys"))))

(deftest emit-collection-extensions-test
  (testing "peek maps to clel-peek"
    (is (clojure.string/includes? (analyze-and-emit '(peek v)) "clel-peek")))
  (testing "pop maps to clel-pop"
    (is (clojure.string/includes? (analyze-and-emit '(pop v)) "clel-pop")))
  (testing "subvec maps to clel-subvec"
    (is (clojure.string/includes? (analyze-and-emit '(subvec v 1 3)) "clel-subvec"))))

(deftest emit-sequence-extensions-test
  (testing "cycle maps to clel-cycle"
    (is (clojure.string/includes? (analyze-and-emit '(cycle [1 2 3])) "clel-cycle")))
  (testing "iterate maps to clel-iterate"
    (is (clojure.string/includes? (analyze-and-emit '(iterate inc 0)) "clel-iterate")))
  (testing "reductions maps to clel-reductions"
    (is (clojure.string/includes? (analyze-and-emit '(reductions + [1 2 3])) "clel-reductions")))
  (testing "take-nth maps to clel-take-nth"
    (is (clojure.string/includes? (analyze-and-emit '(take-nth 2 coll)) "clel-take-nth")))
  (testing "take-last maps to clel-take-last"
    (is (clojure.string/includes? (analyze-and-emit '(take-last 3 coll)) "clel-take-last")))
  (testing "drop-last maps to clel-drop-last"
    (is (clojure.string/includes? (analyze-and-emit '(drop-last 2 coll)) "clel-drop-last"))))

(deftest emit-math-extensions-test
  (testing "abs maps to abs"
    (is (clojure.string/includes? (analyze-and-emit '(abs x)) "abs")))
  (testing "quot maps to truncate"
    (is (clojure.string/includes? (analyze-and-emit '(quot 10 3)) "truncate")))
  (testing "rand maps to clel-rand"
    (is (clojure.string/includes? (analyze-and-emit '(rand)) "clel-rand")))
  (testing "rand-int maps to clel-rand-int"
    (is (clojure.string/includes? (analyze-and-emit '(rand-int 10)) "clel-rand-int")))
  (testing "rand-nth maps to clel-rand-nth"
    (is (clojure.string/includes? (analyze-and-emit '(rand-nth coll)) "clel-rand-nth"))))

(deftest emit-function-extensions-test
  (testing "juxt maps to clel-juxt"
    (is (clojure.string/includes? (analyze-and-emit '(juxt inc dec)) "clel-juxt")))
  (testing "complement maps to clel-complement"
    (is (clojure.string/includes? (analyze-and-emit '(complement even?)) "clel-complement"))))

(deftest emit-io-mappings-test
  (testing "slurp maps to clel-slurp"
    (is (clojure.string/includes? (analyze-and-emit '(slurp "file.txt")) "clel-slurp")))
  (testing "spit maps to clel-spit"
    (is (clojure.string/includes? (analyze-and-emit '(spit "file.txt" "content")) "clel-spit")))
  (testing "read-string maps to clel-read-string"
    (is (clojure.string/includes? (analyze-and-emit '(read-string s)) "clel-read-string"))))

(deftest emit-string-extensions-test
  (testing "clojure.string/split-lines maps to clel-str-split-lines"
    (is (clojure.string/includes?
         (analyze-and-emit '(clojure.string/split-lines s))
         "clel-str-split-lines"))))

;; ============================================================================
;; Special Forms: comment, binding, assert (clel-050)
;; ============================================================================

(deftest emit-comment-test
  (testing "comment emits empty string"
    (is (= "" (analyze-and-emit '(comment (+ 1 2) (println "ignored"))))))
  (testing "comment with no body emits empty string"
    (is (= "" (analyze-and-emit '(comment))))))

(deftest emit-binding-test
  (testing "binding emits let form"
    (let [result (analyze-and-emit '(binding [x 1 y 2] (+ x y)))]
      (is (clojure.string/includes? result "(let "))
      (is (clojure.string/includes? result "x"))
      (is (clojure.string/includes? result "y"))))
  (testing "binding with single binding"
    (let [result (analyze-and-emit '(binding [x 42] x))]
      (is (clojure.string/includes? result "(let "))
      (is (clojure.string/includes? result "x"))
      (is (clojure.string/includes? result "42")))))

(deftest emit-assert-test
  (testing "assert emits cl-assert"
    (let [result (analyze-and-emit '(assert (> x 0)))]
      (is (clojure.string/includes? result "cl-assert"))))
  (testing "assert with message emits cl-assert with message"
    (let [result (analyze-and-emit '(assert (> x 0) "x must be positive"))]
      (is (clojure.string/includes? result "cl-assert"))
      (is (clojure.string/includes? result "x must be positive")))))

;; ============================================================================
;; Bug Fixes
;; ============================================================================

(deftest emit-defvar-no-double-namespace-prefix-test
  (testing "defvar uses mangle-name, not ns-qualify-name (no double prefix)"
    (let [result (analyze-and-emit '(defvar my-var 42))]
      (is (= "(defvar my-var 42)" result))))
  (testing "defvar with docstring and non-nil init"
    (let [result (analyze-and-emit '(defvar my-mode-enabled 0 "Whether mode is enabled."))]
      (is (clojure.string/includes? result "(defvar my-mode-enabled 0"))
      (is (clojure.string/includes? result "Whether mode is enabled."))))
  (testing "defvar with special chars gets mangled correctly"
    (let [result (analyze-and-emit '(defvar my-var? true))]
      (is (clojure.string/includes? result "my-var-p"))))
  (testing "defvar without init"
    (let [result (analyze-and-emit '(defvar my-var))]
      (is (= "(defvar my-var nil)" result)))))

(deftest emit-function-quote-test
  (testing "#'symbol emits Elisp function quote"
    (let [result (analyze-and-emit '(var my-func))]
      (is (= "#'my-func" result))))
  (testing "#'symbol with special chars gets mangled"
    (let [result (analyze-and-emit '(var some?))]
      (is (= "#'some-p" result))))
  (testing "function quote in context — e.g., (mapcar #'func list)"
    (let [result (analyze-and-emit '(mapcar (var my-func) items))]
      (is (clojure.string/includes? result "#'my-func")))))

;; ============================================================================
;; cl-defstruct - CL Struct Passthrough (cljel-fix)
;; ============================================================================

(deftest emit-cl-defstruct-test
  (testing "basic cl-defstruct emits Elisp cl-defstruct"
    (let [code (analyze-and-emit '(cl-defstruct person name age email))]
      (is (clojure.string/includes? code "cl-defstruct"))
      (is (clojure.string/includes? code "person"))
      (is (clojure.string/includes? code "name"))
      (is (clojure.string/includes? code "age"))
      (is (clojure.string/includes? code "email"))))

  (testing "cl-defstruct with no slots"
    (let [code (analyze-and-emit '(cl-defstruct empty-struct))]
      (is (clojure.string/includes? code "(cl-defstruct empty-struct)"))))

  (testing "cl-defstruct output starts with (cl-defstruct"
    (let [code (analyze-and-emit '(cl-defstruct point x y))]
      (is (clojure.string/starts-with? code "(cl-defstruct"))
      (is (clojure.string/includes? code "point"))
      (is (clojure.string/includes? code "x"))
      (is (clojure.string/includes? code "y")))))

;; ============================================================================
;; cl-defun - CL Function Passthrough (cljel-fix)
;; ============================================================================

(deftest emit-cl-defun-test
  (testing "basic cl-defun emits Elisp cl-defun"
    (let [code (analyze-and-emit '(cl-defun greet (name) (message name)))]
      (is (clojure.string/includes? code "cl-defun"))
      (is (clojure.string/includes? code "greet"))
      (is (clojure.string/includes? code "message"))))

  (testing "cl-defun with docstring"
    (let [code (analyze-and-emit '(cl-defun greet (name) "Greet someone." (message name)))]
      (is (clojure.string/includes? code "cl-defun"))
      (is (clojure.string/includes? code "Greet someone."))))

  (testing "cl-defun with &optional preserves CL keywords"
    (let [code (analyze-and-emit '(cl-defun greet (name &optional greeting) (message greeting name)))]
      (is (clojure.string/includes? code "&optional"))
      (is (clojure.string/includes? code "name"))
      (is (clojure.string/includes? code "greeting"))))

  (testing "cl-defun with &key preserves CL keywords"
    (let [code (analyze-and-emit '(cl-defun connect (host &key port timeout) (message host)))]
      (is (clojure.string/includes? code "&key"))
      (is (clojure.string/includes? code "host"))
      (is (clojure.string/includes? code "port"))
      (is (clojure.string/includes? code "timeout")))))

;; ============================================================================
;; defmacro - Emission Support (cljel-fix)
;; ============================================================================

(deftest emit-defmacro-test
  (testing "defmacro emits actual defmacro form (not empty string)"
    (let [code (analyze-and-emit '(defmacro my-when [test & body]
                                    (list 'if test (cons 'progn body))))]
      (is (not= "" code))
      (is (clojure.string/includes? code "defmacro"))
      (is (clojure.string/includes? code "my-when"))))

  (testing "defmacro with docstring"
    (let [code (analyze-and-emit '(defmacro my-macro [x] "Transform x." (list 'quote x)))]
      (is (clojure.string/includes? code "defmacro"))
      (is (clojure.string/includes? code "my-macro"))
      (is (clojure.string/includes? code "Transform x."))))

  (testing "defmacro output starts with (defmacro"
    (let [code (analyze-and-emit '(defmacro simple [x] x))]
      (is (clojure.string/starts-with? code "(defmacro")))))

;; ============================================================================
;; cl-lib Passthrough Mappings (clel-071)
;; ============================================================================

(deftest test-cl-lib-mappings
  (testing "cl-remove-if passes through"
    (is (= "(cl-remove-if pred xs)" (analyze-and-emit '(cl-remove-if pred xs)))))
  (testing "cl-remove-if-not passes through"
    (is (= "(cl-remove-if-not pred xs)" (analyze-and-emit '(cl-remove-if-not pred xs)))))
  (testing "cl-every passes through"
    (is (= "(cl-every pred xs)" (analyze-and-emit '(cl-every pred xs)))))
  (testing "cl-some passes through"
    (is (= "(cl-some pred xs)" (analyze-and-emit '(cl-some pred xs)))))
  (testing "cl-subseq passes through"
    (is (= "(cl-subseq xs 1 3)" (analyze-and-emit '(cl-subseq xs 1 3)))))
  (testing "cl-generic-p passes through"
    (is (= "(cl-generic-p sym)" (analyze-and-emit '(cl-generic-p sym)))))
  (testing "cl-loop passes through"
    (is (clojure.string/includes? (analyze-and-emit '(cl-loop x)) "cl-loop")))
  (testing "cl-labels passes through"
    (is (clojure.string/includes? (analyze-and-emit '(cl-labels x)) "cl-labels"))))

;; ============================================================================
;; Hash-Table Passthrough Mappings (clel-070)
;; ============================================================================

(deftest emit-hash-table-mappings-test
  (testing "make-hash-table passes through"
    (is (= "(make-hash-table :test 'equal)"
           (analyze-and-emit '(make-hash-table :test 'equal)))))

  (testing "puthash passes through"
    (is (= "(puthash key val table)"
           (analyze-and-emit '(puthash key val table)))))

  (testing "gethash passes through"
    (is (= "(gethash key table)"
           (analyze-and-emit '(gethash key table)))))

  (testing "remhash passes through"
    (is (= "(remhash key table)"
           (analyze-and-emit '(remhash key table)))))

  (testing "copy-hash-table passes through"
    (is (= "(copy-hash-table table)"
           (analyze-and-emit '(copy-hash-table table)))))

  (testing "maphash passes through"
    (is (= "(maphash f table)"
           (analyze-and-emit '(maphash f table)))))

  (testing "hash-table-keys passes through"
    (is (= "(hash-table-keys table)"
           (analyze-and-emit '(hash-table-keys table)))))

  (testing "hash-table-values passes through"
    (is (= "(hash-table-values table)"
           (analyze-and-emit '(hash-table-values table)))))

  (testing "hash-table-p passes through"
    (is (= "(hash-table-p x)"
           (analyze-and-emit '(hash-table-p x))))))
