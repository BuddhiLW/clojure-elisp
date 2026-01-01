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

;; ============================================================================
;; Destructuring Support
;; ============================================================================

(declare expand-destructuring)

(defn destructure-pattern?
  "Returns true if pattern requires destructuring (is a vector or map)."
  [pattern]
  (or (vector? pattern) (map? pattern)))

(defn- expand-vector-destructuring
  "Expand vector destructuring pattern into simple bindings.
   Returns a vector of [symbol init-form] pairs.
   
   Examples:
   - [a b] with coll -> [[a (nth coll 0)] [b (nth coll 1)]]
   - [a & rest] -> [[a (first coll)] [rest (rest coll)]]
   - [_ x] -> [[x (nth coll 1)]]  (ignores _)
   - [:as all] -> [[all coll]]"
  [pattern coll-sym]
  (loop [items (seq pattern)
         idx 0
         bindings []
         as-binding nil]
    (cond
      ;; Done processing items
      (empty? items)
      (if as-binding
        (conj bindings as-binding)
        bindings)

      ;; Handle :as keyword
      (= :as (first items))
      (let [as-sym (second items)]
        (recur (drop 2 items) idx bindings [as-sym coll-sym]))

      ;; Handle & rest
      (= '& (first items))
      (let [rest-sym (second items)
            remaining (drop 2 items)
            rest-binding (when (and rest-sym (not= rest-sym '_))
                           [rest-sym (list 'nthrest coll-sym idx)])]
        (recur remaining idx
               (if rest-binding (conj bindings rest-binding) bindings)
               as-binding))

      ;; Handle _ (ignore binding)
      (= '_ (first items))
      (recur (rest items) (inc idx) bindings as-binding)

      ;; Handle nested destructuring
      (destructure-pattern? (first items))
      (let [nested-pattern (first items)
            temp-sym (gensym "vec__")
            nested-bindings (expand-destructuring nested-pattern temp-sym)]
        (recur (rest items)
               (inc idx)
               (into (conj bindings [temp-sym (list 'nth coll-sym idx)])
                     nested-bindings)
               as-binding))

      ;; Simple symbol binding
      :else
      (let [sym (first items)]
        (recur (rest items)
               (inc idx)
               (conj bindings [sym (list 'nth coll-sym idx)])
               as-binding)))))

(defn- expand-map-destructuring
  "Expand map destructuring pattern into simple bindings.
   Returns a vector of [symbol init-form] pairs.
   
   Examples:
   - {:keys [x y]} with m -> [[x (get m :x)] [y (get m :y)]]
   - {:strs [x y]} -> [[x (get m \"x\")] [y (get m \"y\")]]
   - {a :a b :b} -> [[a (get m :a)] [b (get m :b)]]
   - {:keys [x] :or {x 0}} -> [[x (or (get m :x) 0)]]
   - {:keys [x] :as all} -> [[x (get m :x)] [all m]]"
  [pattern map-sym]
  (let [as-sym (:as pattern)
        or-map (:or pattern)
        keys-syms (:keys pattern)
        strs-syms (:strs pattern)
        syms-syms (:syms pattern)
        ;; Remove special keys to get explicit bindings
        explicit-bindings (dissoc pattern :as :or :keys :strs :syms)]
    (cond-> []
      ;; Handle :keys [x y] -> bind x to (get m :x)
      keys-syms
      (into (for [sym keys-syms
                  :let [k (keyword (name sym))
                        default (get or-map sym)]]
              [sym (if default
                     (list 'clojure.core/or (list 'get map-sym k) default)
                     (list 'get map-sym k))]))

      ;; Handle :strs [x y] -> bind x to (get m "x")
      strs-syms
      (into (for [sym strs-syms
                  :let [k (name sym)
                        default (get or-map sym)]]
              [sym (if default
                     (list 'clojure.core/or (list 'get map-sym k) default)
                     (list 'get map-sym k))]))

      ;; Handle :syms [x y] -> bind x to (get m 'x)
      syms-syms
      (into (for [sym syms-syms
                  :let [default (get or-map sym)]]
              [sym (if default
                     (list 'clojure.core/or (list 'get map-sym (list 'quote sym)) default)
                     (list 'get map-sym (list 'quote sym)))]))

      ;; Handle explicit bindings {a :a b :b}
      (seq explicit-bindings)
      (into (for [[sym k] explicit-bindings
                  :when (not= sym '_)
                  :let [default (get or-map sym)]]
              (if (destructure-pattern? sym)
                ;; Nested destructuring
                (let [temp-sym (gensym "map__")]
                  [temp-sym (if default
                              (list 'clojure.core/or (list 'get map-sym k) default)
                              (list 'get map-sym k))])
                ;; Simple binding
                [sym (if default
                       (list 'clojure.core/or (list 'get map-sym k) default)
                       (list 'get map-sym k))])))

      ;; Handle :as binding
      as-sym
      (conj [as-sym map-sym]))))

(defn expand-destructuring
  "Expand a destructuring pattern into simple bindings.
   Takes a pattern and a value form, returns a vector of [symbol init-form] pairs.
   
   For simple symbols, returns [[sym value]].
   For vectors/maps, returns the expanded bindings with gensyms for temp values."
  [pattern value]
  (cond
    ;; Simple symbol - no destructuring needed
    (symbol? pattern)
    [[pattern value]]

    ;; Vector destructuring
    (vector? pattern)
    (let [coll-sym (gensym "vec__")]
      (into [[coll-sym value]]
            (expand-vector-destructuring pattern coll-sym)))

    ;; Map destructuring  
    (map? pattern)
    (let [map-sym (gensym "map__")]
      (into [[map-sym value]]
            (expand-map-destructuring pattern map-sym)))

    :else
    (throw (ex-info (str "Invalid binding pattern: " pattern)
                    {:pattern pattern}))))

(defn expand-bindings
  "Expand a let binding vector, handling destructuring.
   Returns a flat vector suitable for a simple let form."
  [bindings]
  (->> (partition 2 bindings)
       (mapcat (fn [[pattern init]]
                 (expand-destructuring pattern init)))
       vec))

(defn analyze-let
  "Analyze (let [bindings] body) forms.
   Supports destructuring patterns in bindings."
  [[_ bindings & body]]
  (let [;; Expand destructuring into simple bindings
        expanded-pairs (expand-bindings bindings)
        ;; Analyze each binding sequentially, updating env
        binding-nodes (loop [remaining expanded-pairs
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
        all-locals (into (:locals *env*) (map first expanded-pairs))]
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

(defn analyze-try
  "Analyze (try body... (catch ExType e handler) (finally cleanup)) forms.
   Parses body expressions, catch clauses, and optional finally clause."
  [[_ & exprs]]
  (let [;; Separate body from catch/finally clauses
        catch? #(and (seq? %) (= 'catch (first %)))
        finally? #(and (seq? %) (= 'finally (first %)))
        special-clause? #(or (catch? %) (finally? %))

        body-exprs (vec (take-while (complement special-clause?) exprs))
        clauses (drop-while (complement special-clause?) exprs)

        ;; Parse catch clauses: (catch ExType e body...)
        catch-clauses (filter catch? clauses)
        catches (mapv (fn [[_ ex-type binding & handler]]
                        (let [local-env (with-locals *env* #{binding})]
                          {:type ex-type
                           :name binding
                           :body (binding [*env* local-env]
                                   (mapv analyze handler))}))
                      catch-clauses)

        ;; Parse finally clause: (finally body...)
        finally-clause (first (filter finally? clauses))
        finally-body (when finally-clause
                       (mapv analyze (rest finally-clause)))]

    (ast-node :try
              :body (mapv analyze body-exprs)
              :catches catches
              :finally finally-body)))

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
  {'def analyze-def
   'defn analyze-defn
   'fn analyze-fn
   'fn* analyze-fn
   'let analyze-let
   'let* analyze-let
   'if analyze-if
   'when analyze-when
   'cond analyze-cond
   'do analyze-do
   'ns analyze-ns
   'quote analyze-quote
   'loop analyze-loop
   'recur analyze-recur
   'try analyze-try})

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
