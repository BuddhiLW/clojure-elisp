(ns clojure-elisp.emitter-test
  "Tests for the ClojureElisp emitter."
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-elisp.analyzer :as ana]
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
      (is (clojure.string/includes? result "(nil 2)")))))

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
  (testing "cond with comparisons"
    (let [result (analyze-and-emit '(cond (> x 0) "positive"
                                          (< x 0) "negative"
                                          :else "zero"))]
      (is (clojure.string/includes? result "cond"))
      (is (clojure.string/includes? result "(> x 0)")))))

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
              opens (count (filter #(= % \() result))
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
    (is (= "(message \"hello %s\" name)" (analyze-and-emit '(elisp/message "hello %s" name)))))
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
    (let [form (with-meta '(defn foo [x] x) {:line 10 :column 1})
          result (-> form ana/analyze emit/emit)]
      (is (not (clojure.string/includes? result ";;; L10")))))

  (testing "source comments emitted when *emit-source-comments* is true"
    (let [form (with-meta '(defn foo [x] x) {:line 10 :column 1})
          result (binding [emit/*emit-source-comments* true]
                   (-> form ana/analyze emit/emit))]
      (is (clojure.string/includes? result ";;; L10:C1"))))

  (testing "source comments for def forms"
    (let [form (with-meta '(def bar 42) {:line 5 :column 0})
          result (binding [emit/*emit-source-comments* true]
                   (-> form ana/analyze emit/emit))]
      (is (clojure.string/includes? result ";;; L5:C0"))))

  (testing "no source comment when node lacks location"
    (let [result (binding [emit/*emit-source-comments* true]
                   (analyze-and-emit 42))]
      (is (not (clojure.string/includes? result ";;;")))))

  (testing "source comments for fn (lambda) forms"
    (let [form (with-meta '(fn [x] x) {:line 20 :column 3})
          result (binding [emit/*emit-source-comments* true]
                   (-> form ana/analyze emit/emit))]
      (is (clojure.string/includes? result ";;; L20:C3")))))

