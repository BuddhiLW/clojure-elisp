(ns clojure-elisp.analyzer
  "Analyzer for ClojureElisp.

   Transforms Clojure forms into an AST suitable for Elisp emission.
   We use a simplified approach rather than tools.analyzer for now,
   keeping it pragmatic and easy to understand."
  (:require [clojure.walk :as walk]))

;; ============================================================================
;; Environment
;; ============================================================================

(def ^:dynamic *env*
  "Current compilation environment."
  {:ns 'user
   :locals #{}
   :in-tail-position? false})

(defn with-locals
  "Add locals to the environment."
  [env locals]
  (update env :locals into locals))

;; ============================================================================
;; AST Node Constructors
;; ============================================================================

(defn ast-node
  "Create an AST node with common fields."
  [op & {:as fields}]
  (merge {:op op :env *env*} fields))

;; ============================================================================
;; Special Form Analyzers
;; ============================================================================

(declare analyze)

(defn analyze-def
  "Analyze (def name expr) or (def name docstring expr)."
  [[_ name & body]]
  (let [[docstring init] (if (and (string? (first body))
                                  (second body))
                           [(first body) (second body)]
                           [nil (first body)])]
    (ast-node :def
              :name name
              :docstring docstring
              :init (when init (analyze init)))))

(defn analyze-defn
  "Analyze (defn name [args] body) forms."
  [[_ name & fdecl]]
  (let [[docstring fdecl] (if (string? (first fdecl))
                            [(first fdecl) (rest fdecl)]
                            [nil fdecl])
        [params & body] fdecl
        params-vec (if (vector? params) params (first params))]
    (ast-node :defn
              :name name
              :docstring docstring
              :params params-vec
              :body (binding [*env* (with-locals *env* (set params-vec))]
                      (mapv analyze body)))))

(defn analyze-fn
  "Analyze (fn [args] body) forms."
  [[_ & fdecl]]
  (let [[params & body] (if (vector? (first fdecl))
                          fdecl
                          (first fdecl))]
    (ast-node :fn
              :params params
              :body (binding [*env* (with-locals *env* (set params))]
                      (mapv analyze body)))))

(defn analyze-let
  "Analyze (let [bindings] body) forms."
  [[_ bindings & body]]
  (let [pairs (partition 2 bindings)
        binding-nodes (loop [remaining pairs
                             nodes []
                             env *env*]
                        (if (empty? remaining)
                          nodes
                          (let [[sym init] (first remaining)
                                analyzed (binding [*env* env]
                                           (analyze init))
                                new-env (with-locals env #{sym})]
                            (recur (rest remaining)
                                   (conj nodes {:name sym :init analyzed})
                                   new-env))))
        all-locals (into (:locals *env*) (map first pairs))]
    (ast-node :let
              :bindings binding-nodes
              :body (binding [*env* (with-locals *env* all-locals)]
                      (mapv analyze body)))))

(defn analyze-if
  "Analyze (if test then else?) forms."
  [[_ test then else]]
  (ast-node :if
            :test (analyze test)
            :then (analyze then)
            :else (when else (analyze else))))

(defn analyze-when
  "Analyze (when test body...) forms."
  [[_ test & body]]
  (ast-node :when
            :test (analyze test)
            :body (mapv analyze body)))

(defn analyze-cond
  "Analyze (cond clause...) forms."
  [[_ & clauses]]
  (ast-node :cond
            :clauses (->> (partition 2 clauses)
                          (mapv (fn [[test expr]]
                                  {:test (analyze test)
                                   :expr (analyze expr)})))))

(defn analyze-do
  "Analyze (do expr...) forms."
  [[_ & body]]
  (ast-node :do
            :body (mapv analyze body)))

(defn analyze-ns
  "Analyze (ns name ...) forms."
  [[_ ns-name & clauses]]
  (ast-node :ns
            :name ns-name
            :clauses clauses))

(defn analyze-quote
  "Analyze (quote form) forms."
  [[_ form]]
  (ast-node :quote
            :form form))

(defn analyze-loop
  "Analyze (loop [bindings] body) forms."
  [[_ bindings & body]]
  (let [pairs (partition 2 bindings)
        syms (mapv first pairs)
        inits (mapv (comp analyze second) pairs)]
    (ast-node :loop
              :bindings (mapv (fn [s i] {:name s :init i}) syms inits)
              :body (binding [*env* (with-locals *env* (set syms))]
                      (mapv analyze body)))))

(defn analyze-recur
  "Analyze (recur expr...) forms."
  [[_ & args]]
  (ast-node :recur
            :args (mapv analyze args)))

;; ============================================================================
;; Collection Analyzers
;; ============================================================================

(defn analyze-vector
  "Analyze vector literals."
  [v]
  (ast-node :vector
            :items (mapv analyze v)))

(defn analyze-map
  "Analyze map literals."
  [m]
  (ast-node :map
            :keys (mapv analyze (keys m))
            :vals (mapv analyze (vals m))))

(defn analyze-set
  "Analyze set literals."
  [s]
  (ast-node :set
            :items (mapv analyze s)))

;; ============================================================================
;; Invocation Analyzer
;; ============================================================================

(defn analyze-invoke
  "Analyze function invocation (f args...)."
  [[f & args]]
  (ast-node :invoke
            :fn (analyze f)
            :args (mapv analyze args)))

;; ============================================================================
;; Main Analyzer
;; ============================================================================

(def special-forms
  "Map of special form symbols to their analyzers."
  {'def     analyze-def
   'defn    analyze-defn
   'fn      analyze-fn
   'fn*     analyze-fn
   'let     analyze-let
   'let*    analyze-let
   'if      analyze-if
   'when    analyze-when
   'cond    analyze-cond
   'do      analyze-do
   'ns      analyze-ns
   'quote   analyze-quote
   'loop    analyze-loop
   'recur   analyze-recur})

(defn analyze
  "Analyze a Clojure form into an AST node."
  [form]
  ;; Macroexpand first to handle ->, ->>, doto, cond->, etc.
  (let [form (if (and (seq? form)
                      (symbol? (first form))
                      (not (contains? special-forms (first form))))
               (macroexpand form)
               form)]
    (cond
      ;; Nil
      (nil? form)
      (ast-node :const :val nil :type :nil)

      ;; Boolean
      (boolean? form)
      (ast-node :const :val form :type :bool)

      ;; Number
      (number? form)
      (ast-node :const :val form :type :number)

      ;; String
      (string? form)
      (ast-node :const :val form :type :string)

      ;; Keyword
      (keyword? form)
      (ast-node :const :val form :type :keyword)

      ;; Symbol
      (symbol? form)
      (if (contains? (:locals *env*) form)
        (ast-node :local :name form)
        (ast-node :var :name form))

      ;; Vector
      (vector? form)
      (analyze-vector form)

      ;; Map
      (map? form)
      (analyze-map form)

      ;; Set
      (set? form)
      (analyze-set form)

      ;; List (special form or invocation)
      (seq? form)
      (let [op (first form)]
        (if-let [analyzer (get special-forms op)]
          (analyzer form)
          (analyze-invoke form)))

      :else
      (ast-node :unknown :form form))))

(comment
  (analyze '(defn foo [x] (+ x 1)))
  (analyze '(let [a 1 b 2] (+ a b)))
  (analyze '(if (> x 0) "yes" "no")))
