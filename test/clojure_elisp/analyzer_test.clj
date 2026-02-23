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
            b-ref  (get-in a-body [:fn])]
        (is (= :local (:op b-ref)))
        (is (= 'b (:name b-ref))))))

  (testing "letfn function names are in scope in main body"
    (let [ast (analyze '(letfn [(foo [x] x)] (foo 5)))]
      (let [body-invoke (first (:body ast))
            foo-ref     (:fn body-invoke)]
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

;; ============================================================================
;; Source Location Tracking (clel-020)
;; ============================================================================

(deftest analyze-source-location-from-metadata-test
  (testing "AST nodes carry :line/:column from form metadata"
    (let [form (with-meta '(+ 1 2) {:line 10 :column 5})
          ast  (analyze form)]
      (is (= 10 (:line ast)))
      (is (= 5 (:column ast)))))

  (testing "forms without metadata produce nodes without :line/:column"
    (let [ast (analyze 42)]
      (is (nil? (:line ast)))
      (is (nil? (:column ast)))))

  (testing "defn form preserves source location"
    (let [form (with-meta '(defn foo [x] x) {:line 3 :column 1})
          ast  (analyze form)]
      (is (= 3 (:line ast)))
      (is (= 1 (:column ast)))))

  (testing "let form preserves source location"
    (let [form (with-meta '(let [a 1] a) {:line 7 :column 0})
          ast  (analyze form)]
      (is (= 7 (:line ast)))
      (is (= 0 (:column ast))))))

(deftest analyze-source-location-propagation-test
  (testing "nested forms get their own source locations"
    (let [inner (with-meta '(+ 1 2) {:line 5 :column 10})
          outer (with-meta (list 'let ['x inner] 'x) {:line 4 :column 0})
          ast   (analyze outer)]
      ;; Outer let gets line 4
      (is (= 4 (:line ast)))
      ;; Inner (+ 1 2) in the binding init gets line 5
      (let [init-node (:init (first (:bindings ast)))]
        (is (= 5 (:line init-node)))
        (is (= 10 (:column init-node))))))

  (testing "child nodes inherit parent context when they lack metadata"
    ;; A symbol like 'x inside a let won't have its own metadata,
    ;; but it should inherit from the enclosing form's context
    (let [form (with-meta '(let [x 1] x) {:line 10 :column 2})
          ast  (analyze form)]
      ;; The body node (x as local) should have inherited context
      (let [body-node (first (:body ast))]
        (is (= 10 (:line body-node)))
        (is (= 2 (:column body-node)))))))

(deftest analyze-source-location-line-numbering-reader-test
  (testing "forms from LineNumberingPushbackReader carry reader-attached metadata"
    (let [rdr  (clojure.lang.LineNumberingPushbackReader.
                (java.io.StringReader. "(defn greet [name] (str \"Hello \" name))"))
          form (read rdr)
          ast  (analyze form)]
      (is (number? (:line ast)))
      (is (= 1 (:line ast)))))

  (testing "multiple forms from LineNumberingPushbackReader carry line info"
    (let [rdr   (clojure.lang.LineNumberingPushbackReader.
                 (java.io.StringReader. "(+ 1 2)\n(defn foo [x] x)"))
          form1 (read rdr)
          form2 (read rdr)
          ast1  (analyze form1)
          ast2  (analyze form2)]
      (is (= 1 (:line ast1)))
      (is (= 2 (:line ast2))))))

;; ============================================================================
;; Protocols - defprotocol (clel-025)
;; ============================================================================

(deftest analyze-defprotocol-test
  (testing "basic protocol with single method"
    (let [ast (analyze '(defprotocol IGreeter
                          (greet [this name])))]
      (is (= :defprotocol (:op ast)))
      (is (= 'IGreeter (:name ast)))
      (is (= 1 (count (:methods ast))))
      (is (= 'greet (get-in ast [:methods 0 :name])))
      (is (= '[this name] (get-in ast [:methods 0 :params])))))

  (testing "protocol with multiple methods"
    (let [ast (analyze '(defprotocol IShape
                          (area [this])
                          (perimeter [this])))]
      (is (= :defprotocol (:op ast)))
      (is (= 'IShape (:name ast)))
      (is (= 2 (count (:methods ast))))
      (is (= 'area (get-in ast [:methods 0 :name])))
      (is (= 'perimeter (get-in ast [:methods 1 :name])))))

  (testing "protocol method with multiple args"
    (let [ast (analyze '(defprotocol IWriter
                          (write [this data options])))]
      (is (= '[this data options] (get-in ast [:methods 0 :params]))))))

;; ============================================================================
;; Records - defrecord (clel-025)
;; ============================================================================

(deftest analyze-defrecord-test
  (testing "basic record without protocols"
    (let [ast (analyze '(defrecord Point [x y]))]
      (is (= :defrecord (:op ast)))
      (is (= 'Point (:name ast)))
      (is (= '[x y] (:fields ast)))
      (is (empty? (:protocols ast)))))

  (testing "record with protocol implementation"
    (let [ast (analyze '(defrecord Person [first-name last-name]
                          IGreeter
                          (greet [this name] (str "Hello " name))))]
      (is (= :defrecord (:op ast)))
      (is (= 'Person (:name ast)))
      (is (= '[first-name last-name] (:fields ast)))
      (is (= 1 (count (:protocols ast))))
      (is (= 'IGreeter (get-in ast [:protocols 0 :protocol])))
      (is (= 1 (count (get-in ast [:protocols 0 :methods]))))
      (is (= 'greet (get-in ast [:protocols 0 :methods 0 :name])))))

  (testing "record fields are locals in method body"
    (let [ast (analyze '(defrecord Person [name]
                          IGreeter
                          (greet [this] name)))]
      (let [body-node (get-in ast [:protocols 0 :methods 0 :body 0])]
        (is (= :local (:op body-node)))
        (is (= 'name (:name body-node))))))

  (testing "record with multiple protocols"
    (let [ast (analyze '(defrecord Square [side]
                          IShape
                          (area [this] (* side side))
                          IDrawable
                          (draw [this ctx] nil)))]
      (is (= 2 (count (:protocols ast))))
      (is (= 'IShape (get-in ast [:protocols 0 :protocol])))
      (is (= 'IDrawable (get-in ast [:protocols 1 :protocol]))))))

;; ============================================================================
;; Types - deftype (clel-025)
;; ============================================================================

(deftest analyze-deftype-test
  (testing "basic deftype with mutable field"
    (let [ast (analyze '(deftype Counter [^:mutable count]))]
      (is (= :deftype (:op ast)))
      (is (= 'Counter (:name ast)))
      (is (= 1 (count (:fields ast))))
      (is (contains? (:mutable-fields ast) 'count))))

  (testing "deftype with protocol implementation"
    (let [ast (analyze '(deftype Counter [^:mutable count]
                          ICounter
                          (increment [this] (set! count (inc count)))))]
      (is (= :deftype (:op ast)))
      (is (= 1 (count (:protocols ast))))
      (is (= 'increment (get-in ast [:protocols 0 :methods 0 :name])))))

  (testing "deftype with mixed mutable/immutable fields"
    (let [ast (analyze '(deftype Accum [name ^:mutable total]))]
      (is (= 2 (count (:fields ast))))
      (is (contains? (:mutable-fields ast) 'total))
      (is (not (contains? (:mutable-fields ast) 'name)))))

  (testing "deftype without mutable fields"
    (let [ast (analyze '(deftype Wrapper [value]))]
      (is (= :deftype (:op ast)))
      (is (empty? (:mutable-fields ast))))))

;; ============================================================================
;; set! (clel-025)
;; ============================================================================

(deftest analyze-set!-test
  (testing "basic set!"
    (let [ast (analyze '(set! x 42))]
      (is (= :set! (:op ast)))
      (is (= 'x (:target ast)))
      (is (= :const (get-in ast [:value :op])))))

  (testing "set! with expression value"
    (let [ast (analyze '(set! count (inc count)))]
      (is (= :set! (:op ast)))
      (is (= 'count (:target ast)))
      (is (= :invoke (get-in ast [:value :op]))))))

;; ============================================================================
;; extend-type (clel-025)
;; ============================================================================

(deftest analyze-extend-type-test
  (testing "basic extend-type"
    (let [ast (analyze '(extend-type String
                          IGreeter
                          (greet [this] "hello")))]
      (is (= :extend-type (:op ast)))
      (is (= 'String (:type ast)))
      (is (= 1 (count (:protocols ast))))
      (is (= 'IGreeter (get-in ast [:protocols 0 :protocol])))))

  (testing "extend-type with multiple protocols"
    (let [ast (analyze '(extend-type Number
                          IShow
                          (show [this] this)
                          IMath
                          (double-val [this] this)))]
      (is (= :extend-type (:op ast)))
      (is (= 'Number (:type ast)))
      (is (= 2 (count (:protocols ast))))))

  (testing "extend-type with multi-arity method"
    (let [ast (analyze '(extend-type String
                          IGreeter
                          (greet [this prefix] (str prefix this))))]
      (is (= :extend-type (:op ast)))
      (is (= 2 (count (get-in ast [:protocols 0 :methods 0 :params])))))))

;; ============================================================================
;; extend-protocol (clel-025)
;; ============================================================================

(deftest analyze-extend-protocol-test
  (testing "basic extend-protocol"
    (let [ast (analyze '(extend-protocol IGreeter
                          String
                          (greet [this] "hello")))]
      (is (= :extend-protocol (:op ast)))
      (is (= 'IGreeter (:name ast)))
      (is (= 1 (count (:extensions ast))))
      (is (= 'String (get-in ast [:extensions 0 :type])))))

  (testing "extend-protocol with multiple types"
    (let [ast (analyze '(extend-protocol IShow
                          String
                          (show [this] this)
                          Number
                          (show [this] this)))]
      (is (= :extend-protocol (:op ast)))
      (is (= 2 (count (:extensions ast))))
      (is (= 'String (get-in ast [:extensions 0 :type])))
      (is (= 'Number (get-in ast [:extensions 1 :type]))))))

;; ============================================================================
;; satisfies? (clel-025)
;; ============================================================================

(deftest analyze-satisfies?-test
  (testing "basic satisfies?"
    (let [ast (analyze '(satisfies? IGreeter x))]
      (is (= :satisfies? (:op ast)))
      (is (= 'IGreeter (:protocol ast)))
      (is (= :var (get-in ast [:value :op]))))))

;; ============================================================================
;; reify (clel-025)
;; ============================================================================

(deftest analyze-reify-test
  (testing "basic reify"
    (let [ast (analyze '(reify IGreeter
                          (greet [this] "hello")))]
      (is (= :reify (:op ast)))
      (is (= 1 (count (:protocols ast))))
      (is (= 'IGreeter (get-in ast [:protocols 0 :protocol])))))

  (testing "reify with multiple protocols"
    (let [ast (analyze '(reify
                          IGreeter
                          (greet [this] "hi")
                          IShow
                          (show [this] this)))]
      (is (= :reify (:op ast)))
      (is (= 2 (count (:protocols ast))))))

  (testing "reify captures closed-over locals"
    (let [ast (binding [ana/*env* (ana/with-locals ana/*env* #{'x 'y})]
                (analyze '(reify IGreeter
                            (greet [this] x))))]
      (is (= :reify (:op ast)))
      (is (contains? (set (:closed-over ast)) 'x))
      (is (contains? (set (:closed-over ast)) 'y)))))

;; ============================================================================
;; Macro System (clel-027)
;; ============================================================================

(deftest analyze-defmacro-test
  (testing "defmacro produces :defmacro AST node"
    (let [ast (analyze '(defmacro unless [pred body]
                          (list 'if (list 'not pred) body nil)))]
      (is (= :defmacro (:op ast)))
      (is (= 'unless (:name ast)))))

  (testing "defmacro with docstring"
    (let [ast (analyze '(defmacro unless "Opposite of when" [pred body]
                          (list 'if (list 'not pred) body nil)))]
      (is (= :defmacro (:op ast)))
      (is (= "Opposite of when" (:docstring ast)))))

  (testing "defmacro registers macro in registry"
    (ana/clear-macros!)
    (analyze '(defmacro my-macro [x] (list 'inc x)))
    (is (fn? (ana/get-macro 'my-macro)))))

(deftest analyze-macro-expansion-test
  (testing "macro invocation is expanded and analyzed"
    (ana/clear-macros!)
    (analyze '(defmacro my-unless [pred body]
                (list 'if (list 'not pred) body nil)))
    (let [ast (analyze '(my-unless true 42))]
      ;; Should expand to (if (not true) 42 nil) and analyze that
      (is (= :if (:op ast)))
      (is (= :invoke (:op (:test ast))))
      (is (= 'not (:name (:fn (:test ast)))))))

  (testing "macro with syntax-quote expansion"
    (ana/clear-macros!)
    (analyze '(defmacro when-not [test & body]
                `(if (not ~test) (do ~@body) nil)))
    (let [ast (analyze '(when-not false 1 2))]
      (is (= :if (:op ast)))))

  (testing "nested macro expansion"
    (ana/clear-macros!)
    (analyze '(defmacro double-it [x]
                (list '* 2 x)))
    (let [ast (analyze '(+ (double-it 5) 1))]
      ;; (+ (* 2 5) 1)
      (is (= :invoke (:op ast)))
      (is (= :invoke (:op (first (:args ast)))))))

  (testing "macros must be defined before use"
    (ana/clear-macros!)
    ;; Without defining the macro, it should be treated as a regular invocation
    (let [ast (analyze '(my-undefined-macro 1 2))]
      (is (= :invoke (:op ast))))))

(deftest analyze-macroexpand-1-test
  (testing "macroexpand-1 expands one level"
    (ana/clear-macros!)
    (analyze '(defmacro unless [pred body]
                (list 'if (list 'not pred) body nil)))
    (let [expanded (ana/macroexpand-1-clel '(unless true 42))]
      (is (= 'if (first expanded)))
      (is (list? expanded))))

  (testing "macroexpand-1 returns form unchanged if not a macro"
    (ana/clear-macros!)
    (let [form   '(+ 1 2)
          result (ana/macroexpand-1-clel form)]
      (is (= form result)))))

(deftest analyze-macroexpand-test
  (testing "macroexpand fully expands nested macros"
    (ana/clear-macros!)
    (analyze '(defmacro unless [pred body]
                (list 'if (list 'not pred) body nil)))
    (analyze '(defmacro unless2 [pred body]
                (list 'unless pred body)))
    (let [expanded (ana/macroexpand-clel '(unless2 true 42))]
      ;; unless2 -> unless -> if
      (is (= 'if (first expanded))))))

;; ============================================================================
;; Namespace System - build-ns-env (clel-028)
;; ============================================================================

(deftest build-ns-env-test
  (testing "empty requires produces empty env"
    (let [env (ana/build-ns-env [])]
      (is (= {} (:aliases env)))
      (is (= {} (:refers env)))))

  (testing "builds aliases from :as"
    (let [env (ana/build-ns-env [{:ns 'clojure.string :as 'str}
                                 {:ns 'my.utils :as 'u}])]
      (is (= {'str 'clojure.string
              'u 'my.utils}
             (:aliases env)))))

  (testing "builds refers from :refer"
    (let [env (ana/build-ns-env [{:ns 'clojure.string :refer ['join 'split]}])]
      (is (= {'join 'clojure.string
              'split 'clojure.string}
             (:refers env)))))

  (testing "builds both aliases and refers"
    (let [env (ana/build-ns-env [{:ns 'clojure.string :as 'str :refer ['join]}])]
      (is (= {'str 'clojure.string} (:aliases env)))
      (is (= {'join 'clojure.string} (:refers env)))))

  (testing "ignores requires without :as or :refer"
    (let [env (ana/build-ns-env [{:ns 'clojure.string}])]
      (is (= {} (:aliases env)))
      (is (= {} (:refers env))))))

;; ============================================================================
;; Namespace System - Symbol Resolution (clel-028)
;; ============================================================================

(deftest analyze-aliased-symbol-test
  (testing "aliased qualified symbol resolves to full namespace"
    (binding [ana/*env* (merge ana/*env*
                               {:aliases {'str 'clojure.string}})]
      (let [ast (analyze 'str/join)]
        (is (= :var (:op ast)))
        (is (= 'join (:name ast)))
        (is (= 'clojure.string (:ns ast))))))

  (testing "non-aliased qualified symbol preserves original namespace"
    (binding [ana/*env* (merge ana/*env*
                               {:aliases {'str 'clojure.string}})]
      (let [ast (analyze 'other.ns/foo)]
        (is (= :var (:op ast)))
        (is (= 'foo (:name ast)))
        (is (= 'other.ns (:ns ast))))))

  (testing "unqualified symbol is not affected by aliases"
    (binding [ana/*env* (merge ana/*env*
                               {:aliases {'str 'clojure.string}})]
      (let [ast (analyze 'foo)]
        (is (= :var (:op ast)))
        (is (= 'foo (:name ast)))
        (is (nil? (:ns ast)))))))

(deftest analyze-referred-symbol-test
  (testing "referred symbol resolves to source namespace"
    (binding [ana/*env* (merge ana/*env*
                               {:refers {'join 'clojure.string}})]
      (let [ast (analyze 'join)]
        (is (= :var (:op ast)))
        (is (= 'join (:name ast)))
        (is (= 'clojure.string (:ns ast))))))

  (testing "non-referred symbol is unaffected"
    (binding [ana/*env* (merge ana/*env*
                               {:refers {'join 'clojure.string}})]
      (let [ast (analyze 'split)]
        (is (= :var (:op ast)))
        (is (= 'split (:name ast)))
        (is (nil? (:ns ast))))))

  (testing "local shadows referred symbol"
    (binding [ana/*env* (merge ana/*env*
                               {:locals #{'join}
                                :refers {'join 'clojure.string}})]
      (let [ast (analyze 'join)]
        (is (= :local (:op ast)))
        (is (= 'join (:name ast)))))))

;; ============================================================================
;; Namespace System - analyze-file-forms (clel-028)
;; ============================================================================

(deftest analyze-file-forms-test
  (testing "file with ns form establishes context for subsequent forms"
    (let [forms '[(ns my.app
                    (:require [clojure.string :as str]))
                  (defn greet [name] (str/join ", " name))]
          asts  (ana/analyze-file-forms forms)]
      ;; First AST is the ns node
      (is (= :ns (:op (first asts))))
      (is (= 'my.app (:name (first asts))))
      ;; Second AST is defn, and its body should have resolved str/join
      (let [defn-ast (second asts)]
        (is (= :defn (:op defn-ast))))))

  (testing "file without ns form works normally"
    (let [forms '[(def x 1) (def y 2)]
          asts  (ana/analyze-file-forms forms)]
      (is (= 2 (count asts)))
      (is (every? #(= :def (:op %)) asts))))

  (testing "ns establishes current namespace in env"
    (let [forms '[(ns my.app) (def x 42)]
          asts  (ana/analyze-file-forms forms)]
      ;; The def node's env should have :ns set to my.app
      (is (= 'my.app (get-in (second asts) [:env :ns])))))

  (testing "ns with :refer makes symbols available"
    (let [forms '[(ns my.app
                    (:require [utils.helpers :refer [helper]]))
                  (helper 42)]
          asts  (ana/analyze-file-forms forms)]
      (let [invoke-ast (second asts)
            fn-node    (:fn invoke-ast)]
        (is (= :var (:op fn-node)))
        (is (= 'helper (:name fn-node)))
        (is (= 'utils.helpers (:ns fn-node)))))))

;; ============================================================================
;; with-eval-after-load (clel-033)
;; ============================================================================

(deftest analyze-with-eval-after-load-test
  (testing "basic with-eval-after-load with quoted symbol"
    (let [ast (analyze '(with-eval-after-load 'hive-mcp-addons
                          (register-addon 'eca)))]
      (is (= :with-eval-after-load (:op ast)))
      (is (= :quote (:op (:feature ast))))
      (is (= 'hive-mcp-addons (:form (:feature ast))))
      (is (= 1 (count (:body ast))))))

  (testing "with-eval-after-load with multiple body forms"
    (let [ast (analyze '(with-eval-after-load 'my-feature
                          (setup-feature)
                          (configure-feature :option 1)))]
      (is (= :with-eval-after-load (:op ast)))
      (is (= 2 (count (:body ast))))))

  (testing "with-eval-after-load with string feature"
    (let [ast (analyze '(with-eval-after-load "my-package"
                          (init)))]
      (is (= :with-eval-after-load (:op ast)))
      (is (= :const (:op (:feature ast))))
      (is (= "my-package" (:val (:feature ast))))))

  (testing "body forms are analyzed"
    (let [ast (analyze '(with-eval-after-load 'feature
                          (+ 1 2)))]
      (is (= :invoke (:op (first (:body ast))))))))

;; ============================================================================
;; define-minor-mode (clel-032)
;; ============================================================================

(deftest analyze-define-minor-mode-test
  (testing "basic define-minor-mode with docstring and options"
    (let [ast (analyze '(define-minor-mode my-mode
                          "A test minor mode."
                          :init-value nil
                          :lighter " M"
                          :global t))]
      (is (= :define-minor-mode (:op ast)))
      (is (= 'my-mode (:name ast)))
      (is (= "A test minor mode." (:docstring ast)))
      (is (= nil (get-in ast [:options :init-value])))
      (is (= " M" (get-in ast [:options :lighter])))
      (is (= 't (get-in ast [:options :global])))))

  (testing "define-minor-mode with :group option"
    (let [ast (analyze '(define-minor-mode hive-mcp-eca-mode
                          "Minor mode for ECA."
                          :group 'hive-mcp-eca))]
      (is (= :define-minor-mode (:op ast)))
      (is (= 'hive-mcp-eca-mode (:name ast)))
      (is (= '(quote hive-mcp-eca) (get-in ast [:options :group])))))

  (testing "define-minor-mode with body"
    (let [ast (analyze '(define-minor-mode my-mode
                          "Toggle my mode."
                          :lighter " My"
                          (if my-mode
                            (my-enable)
                            (my-disable))))]
      (is (= :define-minor-mode (:op ast)))
      (is (= 1 (count (:body ast))))
      (is (= :if (:op (first (:body ast)))))))

  (testing "define-minor-mode with multiple body forms"
    (let [ast (analyze '(define-minor-mode test-mode
                          "A test mode."
                          :init-value nil
                          (setup-stuff)
                          (when test-mode
                            (activate))))]
      (is (= :define-minor-mode (:op ast)))
      (is (= 2 (count (:body ast))))))

  (testing "define-minor-mode without docstring"
    (let [ast (analyze '(define-minor-mode simple-mode
                          :lighter " S"))]
      (is (= :define-minor-mode (:op ast)))
      (is (= 'simple-mode (:name ast)))
      (is (nil? (:docstring ast)))
      (is (= " S" (get-in ast [:options :lighter])))))

  (testing "define-minor-mode with :keymap option"
    (let [ast (analyze '(define-minor-mode keymap-mode
                          "Mode with keymap."
                          :keymap '(("C-c C-x" . my-command))))]
      (is (= :define-minor-mode (:op ast)))
      (is (some? (get-in ast [:options :keymap]))))))

;; ============================================================================
;; defgroup - Customization Groups (clel-030)
;; ============================================================================

(deftest analyze-defgroup-test
  (testing "basic defgroup with nil value and docstring"
    (let [ast (analyze '(defgroup hive-mcp-eca nil
                          "Integration between hive-mcp and ECA."))]
      (is (= :defgroup (:op ast)))
      (is (= 'hive-mcp-eca (:name ast)))
      (is (nil? (:value ast)))
      (is (= "Integration between hive-mcp and ECA." (:docstring ast)))
      (is (empty? (:options ast)))))

  (testing "defgroup with keyword options"
    (let [ast (analyze '(defgroup my-package nil
                          "My package customizations."
                          :group 'emacs
                          :prefix "my-package-"))]
      (is (= :defgroup (:op ast)))
      (is (= 'my-package (:name ast)))
      (is (= "My package customizations." (:docstring ast)))
      (is (= ''emacs (:group (:options ast))))
      (is (= "my-package-" (:prefix (:options ast))))))

  (testing "defgroup with multiple options"
    (let [ast (analyze '(defgroup hive-mcp-eca nil
                          "Integration docs."
                          :group 'hive-mcp
                          :prefix "hive-mcp-eca-"
                          :tag "HiveMCP ECA"))]
      (is (= :defgroup (:op ast)))
      (is (= ''hive-mcp (:group (:options ast))))
      (is (= "hive-mcp-eca-" (:prefix (:options ast))))
      (is (= "HiveMCP ECA" (:tag (:options ast))))))

  (testing "defgroup without docstring"
    (let [ast (analyze '(defgroup simple-group nil))]
      (is (= :defgroup (:op ast)))
      (is (= 'simple-group (:name ast)))
      (is (nil? (:docstring ast)))
      (is (empty? (:options ast)))))

  (testing "defgroup with :link option"
    (let [ast (analyze '(defgroup my-package nil
                          "My package."
                          :link '(url-link :tag "Homepage" "https://example.com")))]
      (is (= :defgroup (:op ast)))
      (is (some? (:link (:options ast)))))))

;; ============================================================================
;; defcustom - User Customization Variables (clel-031)
;; ============================================================================

(deftest analyze-defcustom-test
  (testing "basic defcustom with nil default and docstring"
    (let [ast (analyze '(defcustom hive-mcp-eca-auto-context nil
                          "When non-nil, automatically include MCP context."))]
      (is (= :defcustom (:op ast)))
      (is (= 'hive-mcp-eca-auto-context (:name ast)))
      (is (nil? (:default ast)))
      (is (= "When non-nil, automatically include MCP context." (:docstring ast)))
      (is (empty? (:options ast)))))

  (testing "defcustom with integer default"
    (let [ast (analyze '(defcustom hive-mcp-eca-timeout 30
                          "Timeout in seconds."))]
      (is (= :defcustom (:op ast)))
      (is (= 'hive-mcp-eca-timeout (:name ast)))
      (is (= 30 (:default ast)))
      (is (= "Timeout in seconds." (:docstring ast)))))

  (testing "defcustom with :type and :group options"
    (let [ast (analyze '(defcustom my-var nil
                          "My variable."
                          :type 'boolean
                          :group 'my-group))]
      (is (= :defcustom (:op ast)))
      (is (= ''boolean (:type (:options ast))))
      (is (= ''my-group (:group (:options ast))))))

  (testing "defcustom with multiple options"
    (let [ast (analyze '(defcustom hive-mcp-eca-timeout 30
                          "Timeout in seconds."
                          :type 'integer
                          :group 'hive-mcp-eca
                          :safe 'integerp))]
      (is (= :defcustom (:op ast)))
      (is (= ''integer (:type (:options ast))))
      (is (= ''hive-mcp-eca (:group (:options ast))))
      (is (= ''integerp (:safe (:options ast))))))

  (testing "defcustom with string default"
    (let [ast (analyze '(defcustom my-prefix "prefix-"
                          "The prefix to use."
                          :type 'string))]
      (is (= :defcustom (:op ast)))
      (is (= "prefix-" (:default ast)))
      (is (= ''string (:type (:options ast))))))

  (testing "defcustom without docstring"
    (let [ast (analyze '(defcustom simple-var true))]
      (is (= :defcustom (:op ast)))
      (is (= 'simple-var (:name ast)))
      (is (= true (:default ast)))
      (is (nil? (:docstring ast)))))

  (testing "defcustom with complex :type option"
    (let [ast (analyze '(defcustom my-choice nil
                          "A choice option."
                          :type '(choice (const nil) (string :tag "Custom"))))]
      (is (= :defcustom (:op ast)))
      (is (some? (:type (:options ast)))))))

;; ============================================================================
;; Iteration Forms - doseq/dotimes (clel-035, clel-045)
;; ============================================================================

(deftest analyze-doseq-test
  (testing "basic doseq with vector literal"
    (let [ast (analyze '(doseq [x [1 2 3]]
                          (println x)))]
      (is (= :doseq (:op ast)))
      (is (= 1 (count (:clauses ast))))
      (is (= :binding (:type (first (:clauses ast)))))
      (is (= 'x (:sym (first (:clauses ast)))))
      (is (= :vector (:op (:coll (first (:clauses ast))))))
      (is (= 1 (count (:body ast))))
      (is (= :invoke (:op (first (:body ast)))))))

  (testing "doseq with var collection"
    (let [ast (analyze '(doseq [item items]
                          (process item)))]
      (is (= :doseq (:op ast)))
      (is (= 'item (:sym (first (:clauses ast)))))
      (is (= :var (:op (:coll (first (:clauses ast))))))
      (is (= 'items (:name (:coll (first (:clauses ast))))))))

  (testing "doseq binding is local in body"
    (let [ast (analyze '(doseq [x coll]
                          x))]
      (is (= :doseq (:op ast)))
      (is (= :local (:op (first (:body ast)))))
      (is (= 'x (:name (first (:body ast)))))))

  (testing "doseq with multiple body expressions"
    (let [ast (analyze '(doseq [x coll]
                          (println "processing")
                          (process x)))]
      (is (= :doseq (:op ast)))
      (is (= 2 (count (:body ast))))))

  (testing "doseq with multiple bindings (clel-045)"
    (let [ast (analyze '(doseq [x xs y ys]
                          (process x y)))]
      (is (= :doseq (:op ast)))
      (is (= 2 (count (:clauses ast))))
      (is (= 'x (:sym (first (:clauses ast)))))
      (is (= 'y (:sym (second (:clauses ast)))))))

  (testing "doseq with :when modifier (clel-045)"
    (let [ast (analyze '(doseq [x coll :when (even? x)]
                          (process x)))]
      (is (= :doseq (:op ast)))
      (is (= 2 (count (:clauses ast))))
      (is (= :binding (:type (first (:clauses ast)))))
      (is (= :when (:type (second (:clauses ast)))))
      (is (= :invoke (:op (:pred (second (:clauses ast))))))))

  (testing "doseq with :let modifier (clel-045)"
    (let [ast (analyze '(doseq [x coll :let [y (inc x)]]
                          (process y)))]
      (is (= :doseq (:op ast)))
      (is (= 2 (count (:clauses ast))))
      (is (= :binding (:type (first (:clauses ast)))))
      (is (= :let (:type (second (:clauses ast)))))
      (is (= 1 (count (:bindings (second (:clauses ast))))))
      (is (= 'y (:name (first (:bindings (second (:clauses ast)))))))))

  (testing "doseq with multiple bindings and :when (clel-045)"
    (let [ast (analyze '(doseq [x xs :when (pos? x) y ys :when (even? y)]
                          (process x y)))]
      (is (= :doseq (:op ast)))
      (is (= 4 (count (:clauses ast))))
      ;; x xs :when (pos? x) y ys :when (even? y)
      (is (= [:binding :when :binding :when] (mapv :type (:clauses ast))))
      (is (= 'x (:sym (first (:clauses ast)))))
      (is (= 'y (:sym (nth (:clauses ast) 2)))))))

(deftest analyze-dotimes-test
  (testing "basic dotimes with literal count"
    (let [ast (analyze '(dotimes [i 5]
                          (println i)))]
      (is (= :dotimes (:op ast)))
      (is (= 'i (:binding ast)))
      (is (= :const (:op (:count ast))))
      (is (= 5 (:val (:count ast))))
      (is (= 1 (count (:body ast))))))

  (testing "dotimes with var count"
    (let [ast (analyze '(dotimes [i n]
                          (do-something i)))]
      (is (= :dotimes (:op ast)))
      (is (= 'i (:binding ast)))
      (is (= :var (:op (:count ast))))
      (is (= 'n (:name (:count ast))))))

  (testing "dotimes binding is local in body"
    (let [ast (analyze '(dotimes [i 10]
                          i))]
      (is (= :dotimes (:op ast)))
      (is (= :local (:op (first (:body ast)))))
      (is (= 'i (:name (first (:body ast)))))))

  (testing "dotimes with multiple body expressions"
    (let [ast (analyze '(dotimes [i 3]
                          (println "iteration")
                          (process i)))]
      (is (= :dotimes (:op ast)))
      (is (= 2 (count (:body ast))))))

  (testing "dotimes with expression count"
    (let [ast (analyze '(dotimes [i (count items)]
                          (process i)))]
      (is (= :dotimes (:op ast)))
      (is (= :invoke (:op (:count ast)))))))

;; ============================================================================
;; For List Comprehension (clel-039, clel-045)
;; ============================================================================

(deftest analyze-for-basic-test
  (testing "basic for with single binding"
    (let [ast (analyze '(for [x coll]
                          (inc x)))]
      (is (= :for (:op ast)))
      (is (= 1 (count (:clauses ast))))
      (is (= :binding (:type (first (:clauses ast)))))
      (is (= 'x (:sym (first (:clauses ast)))))
      (is (= :var (:op (:coll (first (:clauses ast))))))
      (is (= 'coll (:name (:coll (first (:clauses ast))))))
      (is (= 1 (count (:body ast))))))

  (testing "for binding is local in body"
    (let [ast (analyze '(for [x items]
                          x))]
      (is (= :for (:op ast)))
      (is (= :local (:op (first (:body ast)))))
      (is (= 'x (:name (first (:body ast)))))))

  (testing "for with literal collection"
    (let [ast (analyze '(for [x [1 2 3]]
                          (* x x)))]
      (is (= :for (:op ast)))
      (is (= :vector (:op (:coll (first (:clauses ast)))))))))

(deftest analyze-for-when-test
  (testing "for with :when modifier"
    (let [ast (analyze '(for [x coll :when (even? x)]
                          x))]
      (is (= :for (:op ast)))
      (is (= 2 (count (:clauses ast))))
      (is (= :binding (:type (first (:clauses ast)))))
      (is (= 'x (:sym (first (:clauses ast)))))
      (is (= :when (:type (second (:clauses ast)))))
      (is (= :invoke (:op (:pred (second (:clauses ast))))))))

  (testing "for :when can reference binding"
    (let [ast (analyze '(for [n numbers :when (pos? n)]
                          (str n)))]
      (is (= :for (:op ast)))
      (is (= :when (:type (second (:clauses ast)))))
      ;; The 'n' in the when clause should be analyzed as local
      (let [when-args (:args (:pred (second (:clauses ast))))]
        (is (= :local (:op (first when-args))))
        (is (= 'n (:name (first when-args))))))))

(deftest analyze-for-let-test
  (testing "for with :let modifier"
    (let [ast (analyze '(for [x coll :let [y (inc x)]]
                          y))]
      (is (= :for (:op ast)))
      (is (= 2 (count (:clauses ast))))
      (is (= :binding (:type (first (:clauses ast)))))
      (is (= 'x (:sym (first (:clauses ast)))))
      (is (= :let (:type (second (:clauses ast)))))
      (is (= 1 (count (:bindings (second (:clauses ast))))))
      (is (= 'y (:name (first (:bindings (second (:clauses ast)))))))))

  (testing "for with multiple :let bindings"
    (let [ast (analyze '(for [x coll :let [y (inc x) z (* x 2)]]
                          (+ y z)))]
      (is (= :for (:op ast)))
      (let [let-clause (second (:clauses ast))]
        (is (= :let (:type let-clause)))
        (is (= 2 (count (:bindings let-clause))))
        (is (= 'y (:name (first (:bindings let-clause)))))
        (is (= 'z (:name (second (:bindings let-clause))))))))

  (testing "for :let binding is local in body"
    (let [ast (analyze '(for [x coll :let [squared (* x x)]]
                          squared))]
      (is (= :for (:op ast)))
      ;; Body should reference 'squared' as local
      (is (= :local (:op (first (:body ast)))))
      (is (= 'squared (:name (first (:body ast))))))))

(deftest analyze-for-combined-modifiers-test
  (testing "for with :when and :let"
    (let [ast (analyze '(for [x coll :when (pos? x) :let [y (* x 2)]]
                          y))]
      (is (= :for (:op ast)))
      (is (= 3 (count (:clauses ast))))
      (is (= [:binding :when :let] (mapv :type (:clauses ast))))
      (let [let-clause (nth (:clauses ast) 2)]
        (is (= 'y (:name (first (:bindings let-clause))))))))

  (testing "for with :let before :when"
    (let [ast (analyze '(for [x coll :let [y (inc x)] :when (even? y)]
                          y))]
      (is (= :for (:op ast)))
      (is (= 3 (count (:clauses ast))))
      (is (= [:binding :let :when] (mapv :type (:clauses ast)))))))

(deftest analyze-for-multi-binding-test
  (testing "for with multiple bindings (clel-045)"
    (let [ast (analyze '(for [x xs y ys]
                          [x y]))]
      (is (= :for (:op ast)))
      (is (= 2 (count (:clauses ast))))
      (is (= 'x (:sym (first (:clauses ast)))))
      (is (= 'y (:sym (second (:clauses ast)))))))

  (testing "for with multiple bindings and :when on each (clel-045)"
    (let [ast (analyze '(for [x xs :when (even? x)
                              y ys :when (odd? y)]
                          (* x y)))]
      (is (= :for (:op ast)))
      (is (= 4 (count (:clauses ast))))
      (is (= [:binding :when :binding :when] (mapv :type (:clauses ast))))))

  (testing "for with multiple bindings and :let (clel-045)"
    (let [ast (analyze '(for [x    xs
                              y    ys
                              :let [sum (+ x y)]]
                          sum))]
      (is (= :for (:op ast)))
      (is (= 3 (count (:clauses ast))))
      (is (= [:binding :binding :let] (mapv :type (:clauses ast))))))

  (testing "for with complex multi-binding (clel-045)"
    (let [ast (analyze '(for [x     [1 2 3]
                              :let  [x2 (* x 2)]
                              y     [4 5 6]
                              :when (even? (+ x y))]
                          [x y x2]))]
      (is (= :for (:op ast)))
      (is (= 4 (count (:clauses ast))))
      (is (= [:binding :let :binding :when] (mapv :type (:clauses ast)))))))

(deftest analyze-for-while-test
  (testing "for with :while modifier (clel-045)"
    (let [ast (analyze '(for [x xs :while (pos? x)] x))]
      (is (= :for (:op ast)))
      (is (= 2 (count (:clauses ast))))
      (is (= :binding (:type (first (:clauses ast)))))
      (is (= :while (:type (second (:clauses ast)))))
      (is (= :invoke (:op (:pred (second (:clauses ast))))))))

  (testing "for with :while after binding (clel-045)"
    (let [ast (analyze '(for [x xs :while (< x 10)] (* x 2)))]
      (is (= :for (:op ast)))
      (is (= :while (:type (second (:clauses ast)))))
      (is (= :invoke (:op (:pred (second (:clauses ast))))))))

  (testing "for with :while and :let combined (clel-045)"
    (let [ast (analyze '(for [x xs :while (pos? x) :let [y (inc x)]] y))]
      (is (= :for (:op ast)))
      (is (= 3 (count (:clauses ast))))
      (is (= [:binding :while :let] (mapv :type (:clauses ast))))))

  (testing "for with :while in multi-binding (clel-045)"
    (let [ast (analyze '(for [x xs :while (some? x) y ys] [x y]))]
      (is (= :for (:op ast)))
      (is (= 3 (count (:clauses ast))))
      (is (= [:binding :while :binding] (mapv :type (:clauses ast)))))))

(deftest analyze-doseq-while-test
  (testing "doseq with :while modifier (clel-045)"
    (let [ast (analyze '(doseq [x xs :while (pos? x)] (process x)))]
      (is (= :doseq (:op ast)))
      (is (= 2 (count (:clauses ast))))
      (is (= :binding (:type (first (:clauses ast)))))
      (is (= :while (:type (second (:clauses ast)))))
      (is (= :invoke (:op (:pred (second (:clauses ast))))))))

  (testing "doseq with :while and :when combined (clel-045)"
    (let [ast (analyze '(doseq [x xs :while (pos? x) :when (even? x)] (process x)))]
      (is (= :doseq (:op ast)))
      (is (= 3 (count (:clauses ast))))
      (is (= [:binding :while :when] (mapv :type (:clauses ast))))))

  (testing "doseq with :while in multi-binding (clel-045)"
    (let [ast (analyze '(doseq [x xs :while (valid? x) y (children x)] (process y)))]
      (is (= :doseq (:op ast)))
      (is (= 3 (count (:clauses ast))))
      (is (= [:binding :while :binding] (mapv :type (:clauses ast)))))))

;; ============================================================================
;; Bug Fixes
;; ============================================================================

(deftest analyze-var-function-quote-test
  (testing "var form produces :function-quote AST node"
    (let [ast (analyze '(var my-func))]
      (is (= :function-quote (:op ast)))
      (is (= 'my-func (:symbol ast)))))
  (testing "var form with predicate symbol"
    (let [ast (analyze '(var nil?))]
      (is (= :function-quote (:op ast)))
      (is (= 'nil? (:symbol ast))))))

(deftest analyze-defvar-ast-test
  (testing "defvar produces :defvar-elisp AST node"
    (let [ast (analyze '(defvar my-var 42))]
      (is (= :defvar-elisp (:op ast)))
      (is (= 'my-var (:name ast)))
      (is (some? (:init ast)))))
  (testing "defvar with docstring"
    (let [ast (analyze '(defvar my-var nil "A docstring."))]
      (is (= :defvar-elisp (:op ast)))
      (is (= "A docstring." (:docstring ast))))))

;; ============================================================================
;; cl-defstruct - CL Struct Passthrough (cljel-fix)
;; ============================================================================

(deftest analyze-cl-defstruct-test
  (testing "basic cl-defstruct with simple name and slots"
    (let [ast (analyze '(cl-defstruct person name age email))]
      (is (= :cl-defstruct (:op ast)))
      (is (= 'person (:name-or-opts ast)))
      (is (= '[name age email] (:slots ast)))))

  (testing "cl-defstruct with no slots"
    (let [ast (analyze '(cl-defstruct empty-struct))]
      (is (= :cl-defstruct (:op ast)))
      (is (= 'empty-struct (:name-or-opts ast)))
      (is (empty? (:slots ast)))))

  (testing "cl-defstruct with list name+options"
    (let [ast (analyze '(cl-defstruct (person (:constructor make-person)) name age))]
      (is (= :cl-defstruct (:op ast)))
      (is (seq? (:name-or-opts ast)))
      (is (= '[name age] (:slots ast))))))

;; ============================================================================
;; cl-defun - CL Function Passthrough (cljel-fix)
;; ============================================================================

(deftest analyze-cl-defun-test
  (testing "basic cl-defun with body"
    (let [ast (analyze '(cl-defun greet (name) (message name)))]
      (is (= :cl-defun (:op ast)))
      (is (= 'greet (:name ast)))
      (is (= '(name) (:arglist ast)))
      (is (nil? (:docstring ast)))
      (is (= 1 (count (:body ast))))))

  (testing "cl-defun with docstring"
    (let [ast (analyze '(cl-defun greet (name) "Greet someone." (message name)))]
      (is (= :cl-defun (:op ast)))
      (is (= "Greet someone." (:docstring ast)))
      (is (= 1 (count (:body ast))))))

  (testing "cl-defun with &optional params"
    (let [ast (analyze '(cl-defun greet (name &optional greeting) (message greeting name)))]
      (is (= :cl-defun (:op ast)))
      (is (= '(name &optional greeting) (:arglist ast))))))

;; ============================================================================
;; defmacro - Body Analysis (cljel-fix)
;; ============================================================================

(deftest analyze-defmacro-body-test
  (testing "defmacro stores analyzed body"
    (let [ast (analyze '(defmacro my-when [test & body]
                          (list 'if test (cons 'progn body))))]
      (is (= :defmacro (:op ast)))
      (is (= 'my-when (:name ast)))
      (is (vector? (:body ast)))
      (is (pos? (count (:body ast))))))

  (testing "defmacro with docstring stores body"
    (let [ast (analyze '(defmacro my-macro "A macro." [x] (list 'quote x)))]
      (is (= :defmacro (:op ast)))
      (is (= "A macro." (:docstring ast)))
      (is (vector? (:body ast)))
      (is (pos? (count (:body ast)))))))
