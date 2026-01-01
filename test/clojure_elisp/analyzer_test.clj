(ns clojure-elisp.analyzer-test
  "Tests for the ClojureElisp analyzer."
  (:require [clojure.test :refer [deftest is testing are]]
            [clojure-elisp.analyzer :as ana :refer [analyze]]))

;; ============================================================================
;; Constants
;; ============================================================================

(deftest analyze-nil-test
  (testing "nil constant"
    (let [result (analyze nil)]
      (is (= :const (:op result)))
      (is (= nil (:val result)))
      (is (= :nil (:type result))))))

(deftest analyze-boolean-test
  (testing "boolean true"
    (let [result (analyze true)]
      (is (= :const (:op result)))
      (is (= true (:val result)))
      (is (= :bool (:type result)))))
  (testing "boolean false"
    (let [result (analyze false)]
      (is (= :const (:op result)))
      (is (= false (:val result)))
      (is (= :bool (:type result))))))

(deftest analyze-number-test
  (testing "integer"
    (let [result (analyze 42)]
      (is (= :const (:op result)))
      (is (= 42 (:val result)))
      (is (= :number (:type result)))))
  (testing "negative integer"
    (let [result (analyze -17)]
      (is (= :const (:op result)))
      (is (= -17 (:val result)))
      (is (= :number (:type result)))))
  (testing "float"
    (let [result (analyze 3.14)]
      (is (= :const (:op result)))
      (is (= 3.14 (:val result)))
      (is (= :number (:type result)))))
  (testing "ratio"
    (let [result (analyze 1/2)]
      (is (= :const (:op result)))
      (is (= 1/2 (:val result)))
      (is (= :number (:type result))))))

(deftest analyze-string-test
  (testing "simple string"
    (let [result (analyze "hello")]
      (is (= :const (:op result)))
      (is (= "hello" (:val result)))
      (is (= :string (:type result)))))
  (testing "empty string"
    (let [result (analyze "")]
      (is (= :const (:op result)))
      (is (= "" (:val result)))
      (is (= :string (:type result)))))
  (testing "string with special chars"
    (let [result (analyze "hello\nworld")]
      (is (= :const (:op result)))
      (is (= "hello\nworld" (:val result)))
      (is (= :string (:type result))))))

(deftest analyze-keyword-test
  (testing "simple keyword"
    (let [result (analyze :foo)]
      (is (= :const (:op result)))
      (is (= :foo (:val result)))
      (is (= :keyword (:type result)))))
  (testing "namespaced keyword"
    (let [result (analyze :my.ns/bar)]
      (is (= :const (:op result)))
      (is (= :my.ns/bar (:val result)))
      (is (= :keyword (:type result))))))

;; ============================================================================
;; Symbols
;; ============================================================================

(deftest analyze-symbol-var-test
  (testing "symbol as var (not in locals)"
    (let [result (analyze 'foo)]
      (is (= :var (:op result)))
      (is (= 'foo (:name result))))))

(deftest analyze-symbol-local-test
  (testing "symbol as local (in locals)"
    (binding [ana/*env* {:ns 'user :locals #{'x 'y}}]
      (let [result (analyze 'x)]
        (is (= :local (:op result)))
        (is (= 'x (:name result)))))))

;; ============================================================================
;; Collections
;; ============================================================================

(deftest analyze-vector-test
  (testing "empty vector"
    (let [result (analyze [])]
      (is (= :vector (:op result)))
      (is (empty? (:items result)))))
  (testing "vector with elements"
    (let [result (analyze [1 2 3])]
      (is (= :vector (:op result)))
      (is (= 3 (count (:items result))))
      (is (every? #(= :const (:op %)) (:items result)))))
  (testing "nested vector"
    (let [result (analyze [[1 2] [3 4]])]
      (is (= :vector (:op result)))
      (is (= 2 (count (:items result))))
      (is (every? #(= :vector (:op %)) (:items result))))))

(deftest analyze-map-test
  (testing "empty map"
    (let [result (analyze {})]
      (is (= :map (:op result)))
      (is (empty? (:keys result)))
      (is (empty? (:vals result)))))
  (testing "map with entries"
    (let [result (analyze {:a 1 :b 2})]
      (is (= :map (:op result)))
      (is (= 2 (count (:keys result))))
      (is (= 2 (count (:vals result))))))
  (testing "nested map"
    (let [result (analyze {:outer {:inner 42}})]
      (is (= :map (:op result)))
      (is (= 1 (count (:keys result))))
      (let [val-node (first (:vals result))]
        (is (= :map (:op val-node)))))))

(deftest analyze-set-test
  (testing "empty set"
    (let [result (analyze #{})]
      (is (= :set (:op result)))
      (is (empty? (:items result)))))
  (testing "set with elements"
    (let [result (analyze #{1 2 3})]
      (is (= :set (:op result)))
      (is (= 3 (count (:items result)))))))

;; ============================================================================
;; Special Forms - def
;; ============================================================================

(deftest analyze-def-test
  (testing "simple def"
    (let [result (analyze '(def foo 42))]
      (is (= :def (:op result)))
      (is (= 'foo (:name result)))
      (is (nil? (:docstring result)))
      (is (= :const (:op (:init result))))
      (is (= 42 (:val (:init result))))))
  (testing "def with docstring"
    (let [result (analyze '(def bar "A bar value" 123))]
      (is (= :def (:op result)))
      (is (= 'bar (:name result)))
      (is (= "A bar value" (:docstring result)))
      (is (= 123 (:val (:init result))))))
  (testing "def without init"
    (let [result (analyze '(def baz))]
      (is (= :def (:op result)))
      (is (= 'baz (:name result)))
      (is (nil? (:init result))))))

;; ============================================================================
;; Special Forms - defn
;; ============================================================================

(deftest analyze-defn-test
  (testing "simple defn"
    (let [result (analyze '(defn foo [x] x))]
      (is (= :defn (:op result)))
      (is (= 'foo (:name result)))
      (is (= '[x] (:params result)))
      (is (= 1 (count (:body result))))
      ;; x should be analyzed as local since it's in params
      (is (= :local (:op (first (:body result)))))))
  (testing "defn with docstring"
    (let [result (analyze '(defn greet "Says hello" [name] (str "Hello " name)))]
      (is (= :defn (:op result)))
      (is (= "Says hello" (:docstring result)))
      (is (= '[name] (:params result)))))
  (testing "defn with multiple params"
    (let [result (analyze '(defn add [a b] (+ a b)))]
      (is (= :defn (:op result)))
      (is (= '[a b] (:params result)))
      (is (= :invoke (:op (first (:body result)))))))
  (testing "defn with multi-arity body"
    (let [result (analyze '(defn multi [x y] (+ x 1) (+ y 2)))]
      (is (= :defn (:op result)))
      (is (= 2 (count (:body result)))))))

(deftest analyze-defn-multi-arity-test
  (testing "multi-arity defn"
    (let [result (analyze '(defn foo ([x] x) ([x y] (+ x y))))]
      (is (= :defn (:op result)))
      (is (= 'foo (:name result)))
      (is (true? (:multi-arity? result)))
      (is (= 2 (count (:arities result))))
      ;; First arity
      (let [arity1 (first (:arities result))]
        (is (= '[x] (:params arity1)))
        (is (= 1 (:arity arity1)))
        (is (false? (:variadic? arity1))))
      ;; Second arity
      (let [arity2 (second (:arities result))]
        (is (= '[x y] (:params arity2)))
        (is (= 2 (:arity arity2)))
        (is (false? (:variadic? arity2))))))
  (testing "multi-arity with docstring"
    (let [result (analyze '(defn bar "A multi-arity fn" ([x] x) ([x y] (+ x y))))]
      (is (= :defn (:op result)))
      (is (= "A multi-arity fn" (:docstring result)))
      (is (true? (:multi-arity? result)))
      (is (= 2 (count (:arities result))))))
  (testing "multi-arity with variadic"
    (let [result (analyze '(defn baz ([x] x) ([x y] (+ x y)) ([x y & more] (apply + x y more))))]
      (is (= :defn (:op result)))
      (is (true? (:multi-arity? result)))
      (is (= 3 (count (:arities result))))
      ;; Variadic arity
      (let [variadic-arity (last (:arities result))]
        (is (= :variadic (:arity variadic-arity)))
        (is (true? (:variadic? variadic-arity)))
        (is (= '[x y] (:fixed-params variadic-arity)))
        (is (= 'more (:rest-param variadic-arity)))))))

(deftest analyze-defn-variadic-test
  (testing "single-arity variadic"
    (let [result (analyze '(defn varargs [x & rest] (cons x rest)))]
      (is (= :defn (:op result)))
      (is (true? (:variadic? result)))
      (is (= '[x] (:fixed-params result)))
      (is (= 'rest (:rest-param result)))
      (is (= '[x & rest] (:params result)))))
  (testing "variadic with no fixed params"
    (let [result (analyze '(defn all-args [& args] args))]
      (is (= :defn (:op result)))
      (is (true? (:variadic? result)))
      (is (= '[] (:fixed-params result)))
      (is (= 'args (:rest-param result))))))

;; ============================================================================
;; Special Forms - fn
;; ============================================================================

(deftest analyze-fn-test
  (testing "simple fn"
    (let [result (analyze '(fn [x] x))]
      (is (= :fn (:op result)))
      (is (= '[x] (:params result)))
      (is (= :local (:op (first (:body result)))))))
  (testing "fn with multiple params"
    (let [result (analyze '(fn [a b c] (+ a b c)))]
      (is (= :fn (:op result)))
      (is (= '[a b c] (:params result)))))
  (testing "fn* form"
    (let [result (analyze '(fn* [x] x))]
      (is (= :fn (:op result))))))

;; ============================================================================
;; Special Forms - let
;; ============================================================================

(deftest analyze-let-test
  (testing "simple let"
    (let [result (analyze '(let [x 1] x))]
      (is (= :let (:op result)))
      (is (= 1 (count (:bindings result))))
      (is (= 'x (:name (first (:bindings result)))))
      ;; x in body should be a local
      (is (= :local (:op (first (:body result)))))))
  (testing "let with multiple bindings"
    (let [result (analyze '(let [a 1 b 2 c 3] (+ a b c)))]
      (is (= :let (:op result)))
      (is (= 3 (count (:bindings result))))))
  (testing "let with dependent bindings"
    (let [result (analyze '(let [a 1 b a] b))]
      (is (= :let (:op result)))
      ;; b's init should reference a as a local
      (let [b-init (:init (second (:bindings result)))]
        (is (= :local (:op b-init)))
        (is (= 'a (:name b-init))))))
  (testing "let* form"
    (let [result (analyze '(let* [x 1] x))]
      (is (= :let (:op result))))))

;; ============================================================================
;; Special Forms - if
;; ============================================================================

(deftest analyze-if-test
  (testing "if with then and else"
    (let [result (analyze '(if true 1 2))]
      (is (= :if (:op result)))
      (is (= :const (:op (:test result))))
      (is (= true (:val (:test result))))
      (is (= 1 (:val (:then result))))
      (is (= 2 (:val (:else result))))))
  (testing "if without else"
    (let [result (analyze '(if true 1))]
      (is (= :if (:op result)))
      (is (some? (:then result)))
      (is (nil? (:else result)))))
  (testing "nested if"
    (let [result (analyze '(if true (if false 1 2) 3))]
      (is (= :if (:op result)))
      (is (= :if (:op (:then result)))))))

;; ============================================================================
;; Special Forms - when
;; ============================================================================

(deftest analyze-when-test
  (testing "simple when"
    (let [result (analyze '(when true 42))]
      (is (= :when (:op result)))
      (is (= :const (:op (:test result))))
      (is (= 1 (count (:body result))))))
  (testing "when with multiple body forms"
    (let [result (analyze '(when true (println "hi") 42))]
      (is (= :when (:op result)))
      (is (= 2 (count (:body result)))))))

;; ============================================================================
;; Special Forms - cond
;; ============================================================================

(deftest analyze-cond-test
  (testing "simple cond"
    (let [result (analyze '(cond true 1 false 2))]
      (is (= :cond (:op result)))
      (is (= 2 (count (:clauses result))))))
  (testing "cond with :else"
    (let [result (analyze '(cond (> x 0) "positive" :else "non-positive"))]
      (is (= :cond (:op result)))
      (is (= 2 (count (:clauses result))))))
  (testing "empty cond"
    (let [result (analyze '(cond))]
      (is (= :cond (:op result)))
      (is (empty? (:clauses result))))))

(deftest analyze-case-test
  (testing "case with keyword tests"
    (let [ast (analyze '(case x :a 1 :b 2))]
      (is (= :case (:op ast)))
      (is (= 2 (count (:clauses ast))))
      (is (nil? (:default ast)))))

  (testing "case with default"
    (let [ast (analyze '(case x :a 1 :b 2 :unknown))]
      (is (= :case (:op ast)))
      (is (= 2 (count (:clauses ast))))
      (is (some? (:default ast)))
      (is (= :const (get-in ast [:default :op])))))

  (testing "case with single clause and default"
    (let [ast (analyze '(case x :only 42 :fallback))]
      (is (= :case (:op ast)))
      (is (= 1 (count (:clauses ast))))
      (is (some? (:default ast)))))

  (testing "case expr is analyzed"
    (let [ast (analyze '(case (+ 1 2) :a 1))]
      (is (= :invoke (get-in ast [:expr :op]))))))

;; ============================================================================
;; Special Forms - do
;; ============================================================================

(deftest analyze-do-test
  (testing "simple do"
    (let [result (analyze '(do 1 2 3))]
      (is (= :do (:op result)))
      (is (= 3 (count (:body result))))))
  (testing "empty do"
    (let [result (analyze '(do))]
      (is (= :do (:op result)))
      (is (empty? (:body result)))))
  (testing "do with side effects"
    (let [result (analyze '(do (println "hi") (+ 1 2)))]
      (is (= :do (:op result)))
      (is (= 2 (count (:body result)))))))

(deftest analyze-and-test
  (testing "and with multiple expressions"
    (let [ast (analyze '(and a b c))]
      (is (= :and (:op ast)))
      (is (= 3 (count (:exprs ast))))))
  (testing "empty and"
    (let [ast (analyze '(and))]
      (is (= :and (:op ast)))
      (is (empty? (:exprs ast)))))
  (testing "and with single expression"
    (let [ast (analyze '(and true))]
      (is (= :and (:op ast)))
      (is (= 1 (count (:exprs ast)))))))

(deftest analyze-or-test
  (testing "or with multiple expressions"
    (let [ast (analyze '(or a b c))]
      (is (= :or (:op ast)))
      (is (= 3 (count (:exprs ast))))))
  (testing "empty or"
    (let [ast (analyze '(or))]
      (is (= :or (:op ast)))
      (is (empty? (:exprs ast)))))
  (testing "or with single expression"
    (let [ast (analyze '(or false))]
      (is (= :or (:op ast)))
      (is (= 1 (count (:exprs ast)))))))

;; ============================================================================
;; Special Forms - loop/recur
;; ============================================================================

(deftest analyze-loop-test
  (testing "simple loop"
    (let [result (analyze '(loop [x 0] x))]
      (is (= :loop (:op result)))
      (is (= 1 (count (:bindings result))))
      (is (= 'x (:name (first (:bindings result)))))
      (is (= :local (:op (first (:body result)))))))
  (testing "loop with multiple bindings"
    (let [result (analyze '(loop [a 1 b 2] (+ a b)))]
      (is (= :loop (:op result)))
      (is (= 2 (count (:bindings result)))))))

(deftest analyze-letfn-test
  (testing "letfn with single function"
    (let [ast (analyze '(letfn [(foo [x] (+ x 1))] (foo 5)))]
      (is (= :letfn (:op ast)))
      (is (= 1 (count (:fns ast))))
      (is (= 'foo (get-in ast [:fns 0 :name])))
      (is (= '[x] (get-in ast [:fns 0 :params])))))

  (testing "letfn with mutually recursive functions"
    (let [ast (analyze '(letfn [(even? [n] (if (= n 0) true (odd? (- n 1))))
                                (odd? [n] (if (= n 0) false (even? (- n 1))))]
                          (even? 10)))]
      (is (= :letfn (:op ast)))
      (is (= 2 (count (:fns ast))))
      (is (= '[even? odd?] (mapv :name (:fns ast))))))

  (testing "letfn function names are in scope in bodies"
    (let [ast (analyze '(letfn [(a [x] (b x))
                                (b [y] y)]
                          (a 1)))]
      ;; In 'a's body, 'b' should be resolved as a local
      (let [a-body (get-in ast [:fns 0 :body 0])
            b-ref (get-in a-body [:fn])]
        (is (= :local (:op b-ref)))
        (is (= 'b (:name b-ref))))))

  (testing "letfn function names are in scope in main body"
    (let [ast (analyze '(letfn [(foo [x] x)] (foo 5)))]
      (let [body-invoke (first (:body ast))
            foo-ref (:fn body-invoke)]
        (is (= :local (:op foo-ref)))
        (is (= 'foo (:name foo-ref)))))))

(deftest analyze-recur-test
  (testing "simple recur"
    (let [result (analyze '(recur 1))]
      (is (= :recur (:op result)))
      (is (= 1 (count (:args result))))))
  (testing "recur with multiple args"
    (let [result (analyze '(recur a b c))]
      (is (= :recur (:op result)))
      (is (= 3 (count (:args result)))))))

;; ============================================================================
;; Special Forms - quote
;; ============================================================================

(deftest analyze-quote-test
  (testing "quote symbol"
    (let [result (analyze '(quote foo))]
      (is (= :quote (:op result)))
      (is (= 'foo (:form result)))))
  (testing "quote list"
    (let [result (analyze '(quote (1 2 3)))]
      (is (= :quote (:op result)))
      (is (= '(1 2 3) (:form result))))))

;; ============================================================================
;; Special Forms - ns
;; ============================================================================

(deftest analyze-ns-test
  (testing "simple ns"
    (let [result (analyze '(ns my.namespace))]
      (is (= :ns (:op result)))
      (is (= 'my.namespace (:name result)))))
  (testing "ns with require"
    (let [result (analyze '(ns my.namespace (:require [clojure.string :as str])))]
      (is (= :ns (:op result)))
      (is (= 'my.namespace (:name result)))
      (is (seq (:clauses result))))))

(deftest analyze-ns-require-test
  (testing "parse simple require"
    (let [ast (analyze '(ns my.app (:require [clojure.string])))]
      (is (= 1 (count (:requires ast))))
      (is (= 'clojure.string (get-in ast [:requires 0 :ns])))))

  (testing "parse bare symbol require"
    (let [ast (analyze '(ns my.app (:require clojure.set)))]
      (is (= 1 (count (:requires ast))))
      (is (= 'clojure.set (get-in ast [:requires 0 :ns])))))

  (testing "parse require with :as"
    (let [ast (analyze '(ns my.app (:require [clojure.string :as str])))]
      (is (= 'str (get-in ast [:requires 0 :as])))))

  (testing "parse require with :refer"
    (let [ast (analyze '(ns my.app (:require [clojure.string :refer [join split]])))]
      (is (= '[join split] (get-in ast [:requires 0 :refer])))))

  (testing "parse require with :as and :refer"
    (let [ast (analyze '(ns my.app (:require [clojure.string :as str :refer [join]])))]
      (is (= 'str (get-in ast [:requires 0 :as])))
      (is (= '[join] (get-in ast [:requires 0 :refer])))))

  (testing "parse multiple requires"
    (let [ast (analyze '(ns my.app
                          (:require [clojure.string :as str]
                                    [clojure.set :as set])))]
      (is (= 2 (count (:requires ast))))
      (is (= 'clojure.string (get-in ast [:requires 0 :ns])))
      (is (= 'clojure.set (get-in ast [:requires 1 :ns])))))

  (testing "parse multiple require clauses"
    (let [ast (analyze '(ns my.app
                          (:require [clojure.string :as str])
                          (:require [clojure.set :as set])))]
      (is (= 2 (count (:requires ast)))))))

;; ============================================================================
;; Function Invocation
;; ============================================================================

(deftest analyze-invoke-test
  (testing "simple function call"
    (let [result (analyze '(+ 1 2))]
      (is (= :invoke (:op result)))
      (is (= :var (:op (:fn result))))
      (is (= '+ (:name (:fn result))))
      (is (= 2 (count (:args result))))))
  (testing "nested function call"
    (let [result (analyze '(+ (* 2 3) 4))]
      (is (= :invoke (:op result)))
      (let [first-arg (first (:args result))]
        (is (= :invoke (:op first-arg)))
        (is (= '* (:name (:fn first-arg)))))))
  (testing "no-arg function call"
    (let [result (analyze '(foo))]
      (is (= :invoke (:op result)))
      (is (empty? (:args result))))))

;; ============================================================================
;; Macroexpansion
;; ============================================================================

(deftest analyze-threading-macros-test
  (testing "-> threading macro gets expanded"
    (let [result (analyze '(-> x (inc) (+ 2)))]
      ;; After macroexpansion, this should be (+ (inc x) 2)
      (is (= :invoke (:op result)))))
  (testing "->> threading macro gets expanded"
    (let [result (analyze '(->> x (map inc) (filter even?)))]
      (is (= :invoke (:op result))))))

;; ============================================================================
;; Environment Tracking
;; ============================================================================

(deftest analyze-env-test
  (testing "env is included in AST nodes"
    (let [result (analyze 42)]
      (is (map? (:env result)))
      (is (contains? (:env result) :ns))
      (is (contains? (:env result) :locals)))))

;; ============================================================================
;; Multimethods - defmulti/defmethod
;; ============================================================================

(deftest analyze-defmulti-test
  (testing "basic defmulti with keyword dispatch"
    (let [ast (analyze '(defmulti area :shape))]
      (is (= :defmulti (:op ast)))
      (is (= 'area (:name ast)))
      (is (= :const (get-in ast [:dispatch-fn :op])))
      (is (= :keyword (get-in ast [:dispatch-fn :type])))))

  (testing "defmulti with function dispatch"
    (let [ast (analyze '(defmulti dispatch-example class))]
      (is (= :defmulti (:op ast)))
      (is (= 'dispatch-example (:name ast)))
      (is (= :var (get-in ast [:dispatch-fn :op])))))

  (testing "defmulti with lambda dispatch"
    (let [ast (analyze '(defmulti complex-dispatch (fn [x] (:type x))))]
      (is (= :defmulti (:op ast)))
      (is (= :fn (get-in ast [:dispatch-fn :op]))))))

(deftest analyze-defmethod-test
  (testing "basic defmethod with keyword dispatch value"
    (let [ast (analyze '(defmethod area :rectangle [{:keys [w h]}] (* w h)))]
      (is (= :defmethod (:op ast)))
      (is (= 'area (:name ast)))
      (is (= :rectangle (:dispatch-val ast)))
      (is (vector? (:params ast)))
      (is (= 1 (count (:body ast))))))

  (testing "defmethod with symbol dispatch value"
    (let [ast (analyze '(defmethod process String [s] (str s)))]
      (is (= :defmethod (:op ast)))
      (is (= 'process (:name ast)))
      (is (= 'String (:dispatch-val ast)))))

  (testing "defmethod with :default dispatch"
    (let [ast (analyze '(defmethod area :default [shape] 0))]
      (is (= :defmethod (:op ast)))
      (is (= :default (:dispatch-val ast)))))

  (testing "defmethod with multiple body forms"
    (let [ast (analyze '(defmethod area :circle [{:keys [r]}]
                          (println "calculating circle area")
                          (* 3.14159 r r)))]
      (is (= :defmethod (:op ast)))
      (is (= 2 (count (:body ast)))))))

