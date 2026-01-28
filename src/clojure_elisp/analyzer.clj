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

(declare analyze expand-destructuring process-fn-params)

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
  "Analyze (defn name [args] body) and (defn name ([args1] body1) ([args2] body2)) forms.

   Multi-arity detection: if after name/docstring, the first form is a list
   starting with a vector, we have multi-arity syntax."
  [[_ name & fdecl]]
  (let [[docstring fdecl] (if (string? (first fdecl))
                            [(first fdecl) (rest fdecl)]
                            [nil fdecl])
        ;; Detect multi-arity: first element is a list starting with a vector
        multi-arity? (and (seq? (first fdecl))
                          (vector? (ffirst fdecl)))]
    (if multi-arity?
      ;; Multi-arity form: (defn foo ([x] x) ([x y] (+ x y)))
      (let [arities (for [arity fdecl]
                      (let [[params & body] arity
                            ;; Check for variadic: params contains &
                            variadic? (some #{'&} params)
                            ;; For variadic, get the fixed params and rest param
                            fixed-params (if variadic?
                                           (vec (take-while #(not= '& %) params))
                                           params)
                            rest-param (when variadic?
                                         (last params))
                            ;; All param symbols for locals (excluding &)
                            all-param-syms (set (remove #{'&} params))]
                        {:params params
                         :fixed-params fixed-params
                         :rest-param rest-param
                         :variadic? (boolean variadic?)
                         :arity (if variadic?
                                  :variadic
                                  (count params))
                         :body (binding [*env* (with-locals *env* all-param-syms)]
                                 (mapv analyze body))}))]
        (ast-node :defn
                  :name name
                  :docstring docstring
                  :multi-arity? true
                  :arities (vec arities)))
      ;; Single-arity form: (defn foo [x] body)
      (let [[params & body] fdecl
            params-vec (if (vector? params) params (first params))
            {:keys [simple-params rest-param destructure-bindings all-locals]}
            (process-fn-params params-vec)

            ;; Build effective params for AST
            effective-params (if rest-param
                               (conj simple-params '& rest-param)
                               simple-params)

            ;; Wrap body in let if destructuring needed
            effective-body
            (if (seq destructure-bindings)
              (let [let-bindings (vec (mapcat (fn [[pattern gsym]]
                                                (expand-destructuring pattern gsym))
                                              destructure-bindings))]
                [(list 'let (vec (mapcat (fn [[sym init]] [sym init]) let-bindings))
                       (cons 'do body))])
              body)]
        (ast-node :defn
                  :name name
                  :docstring docstring
                  :params effective-params
                  :fixed-params simple-params
                  :rest-param rest-param
                  :variadic? (boolean rest-param)
                  :body (binding [*env* (with-locals *env* all-locals)]
                          (mapv analyze effective-body)))))))

(defn analyze-fn
  "Analyze (fn [args] body) forms.
   Supports destructuring patterns and & rest args in parameters."
  [[_ & fdecl]]
  (let [[params & body] (if (vector? (first fdecl))
                          fdecl
                          (first fdecl))
        {:keys [simple-params rest-param destructure-bindings all-locals]}
        (process-fn-params params)

        ;; Build the effective params for the AST
        effective-params (if rest-param
                           (conj simple-params '& rest-param)
                           simple-params)

        ;; If there are destructuring bindings, wrap body in a let
        effective-body
        (if (seq destructure-bindings)
          ;; Create a let form with all the destructure bindings
          (let [let-bindings (vec (mapcat (fn [[pattern gsym]]
                                            (expand-destructuring pattern gsym))
                                          destructure-bindings))]
            [(list 'let (vec (mapcat (fn [[sym init]] [sym init]) let-bindings))
                   (cons 'do body))])
          body)]
    (ast-node :fn
              :params effective-params
              :rest-param rest-param
              :body (binding [*env* (with-locals *env* all-locals)]
                      (mapv analyze effective-body)))))

;; ============================================================================
;; Destructuring Support
;; ============================================================================

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
(defn- extract-rest-param
  "Extract rest parameter from params vector.
   Returns [regular-params rest-sym] where rest-sym is nil if no & rest."
  [params]
  (let [amp-idx (.indexOf (vec params) '&)]
    (if (neg? amp-idx)
      [params nil]
      [(subvec (vec params) 0 amp-idx)
       (nth params (inc amp-idx))])))

(defn process-fn-params
  "Process function parameters, handling destructuring and rest args.
   Returns a map with:
   - :simple-params - vector of simple symbols for the Elisp function signature
   - :rest-param - the rest parameter symbol (or nil)
   - :destructure-bindings - vector of [pattern gensym] pairs needing expansion
   - :all-locals - set of all local symbols that will be bound"
  [params]
  (let [[regular-params rest-sym] (extract-rest-param params)
        ;; Process regular params
        regular-result
        (reduce (fn [acc param]
                  (if (destructure-pattern? param)
                    ;; Destructuring param - use gensym
                    (let [gsym (gensym "p__")]
                      (-> acc
                          (update :simple-params conj gsym)
                          (update :destructure-bindings conj [param gsym])))
                    ;; Simple param
                    (update acc :simple-params conj param)))
                {:simple-params []
                 :destructure-bindings []}
                regular-params)

        ;; Process rest param if present
        rest-result
        (if rest-sym
          (if (destructure-pattern? rest-sym)
            ;; Rest param with destructuring
            (let [gsym (gensym "rest__")]
              (-> regular-result
                  (assoc :rest-param gsym)
                  (update :destructure-bindings conj [rest-sym gsym])))
            ;; Simple rest param
            (assoc regular-result :rest-param rest-sym))
          regular-result)

        ;; Calculate all locals from destructure bindings
        all-destructure-locals
        (->> (:destructure-bindings rest-result)
             (mapcat (fn [[pattern gsym]]
                       (map first (expand-destructuring pattern gsym))))
             set)]
    (assoc rest-result
           :all-locals (into (set (:simple-params rest-result))
                             (if (:rest-param rest-result)
                               (conj all-destructure-locals (:rest-param rest-result))
                               all-destructure-locals)))))

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

(defn analyze-case
  "Analyze (case expr clause...) forms.
   Clauses are test-val expr pairs, with optional default at the end."
  [[_ expr & clauses]]
  (let [;; If odd number of clauses, last is default
        has-default? (odd? (count clauses))
        pairs (if has-default?
                (partition 2 (butlast clauses))
                (partition 2 clauses))
        default (when has-default? (last clauses))]
    (ast-node :case
              :expr (analyze expr)
              :clauses (mapv (fn [[test-val result]]
                               {:test test-val ;; Keep raw value, not analyzed (it's a constant)
                                :expr (analyze result)})
                             pairs)
              :default (when default (analyze default)))))

(defn analyze-do
  "Analyze (do expr...) forms."
  [[_ & body]]
  (ast-node :do
            :body (mapv analyze body)))

(defn analyze-and
  "Analyze (and expr...) forms for short-circuit evaluation."
  [[_ & exprs]]
  (ast-node :and
            :exprs (mapv analyze exprs)))

(defn analyze-or
  "Analyze (or expr...) forms for short-circuit evaluation."
  [[_ & exprs]]
  (ast-node :or
            :exprs (mapv analyze exprs)))

(defn parse-require-spec
  "Parse a single require spec.
   Examples:
     clojure.string                    -> {:ns clojure.string}
     [clojure.string]                  -> {:ns clojure.string}
     [clojure.string :as str]          -> {:ns clojure.string :as str}
     [clojure.string :refer [join]]    -> {:ns clojure.string :refer [join]}"
  [spec]
  (if (vector? spec)
    (let [[ns-name & opts] spec
          opts-map (apply hash-map opts)]
      {:ns ns-name
       :as (:as opts-map)
       :refer (:refer opts-map)})
    {:ns spec}))

(defn analyze-ns
  "Analyze (ns name ...) forms.
   Parses :require clauses into structured data with :as and :refer options."
  [[_ ns-name & clauses]]
  (let [requires (->> clauses
                      (filter #(and (sequential? %) (= :require (first %))))
                      (mapcat rest)
                      (map parse-require-spec)
                      vec)]
    (ast-node :ns
              :name ns-name
              :requires requires
              :clauses clauses)))

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

(defn analyze-lazy-seq
  "Analyze (lazy-seq body) forms."
  [[_ & body]]
  (ast-node :lazy-seq
            :body (mapv analyze body)))

(defn analyze-throw
  "Analyze (throw exception) forms.
   Supports:
   - (throw (ex-info message data)) - exception with message and data
   - (throw (Exception. message)) - simple exception with message
   - (throw e) - re-throw a caught exception"
  [[_ exception]]
  (let [analyzed-ex (analyze exception)
        ex-type (cond
                  (and (= :invoke (:op analyzed-ex))
                       (= 'ex-info (get-in analyzed-ex [:fn :name])))
                  :ex-info

                  (and (= :invoke (:op analyzed-ex))
                       (let [fn-name (get-in analyzed-ex [:fn :name])]
                         (and fn-name (.endsWith (name fn-name) "."))))
                  :constructor

                  (#{:local :var} (:op analyzed-ex))
                  :rethrow

                  :else
                  :expression)]
    (ast-node :throw
              :exception analyzed-ex
              :exception-type ex-type)))

(defn analyze-letfn
  "Analyze (letfn [(name [params] body)...] body) forms.
   Creates local recursive function bindings that can reference each other."
  [[_ fn-specs & body]]
  (let [;; First pass: collect all function names for mutual recursion
        fn-names (mapv first fn-specs)
        ;; Add all fn names to environment before analyzing bodies
        new-env (with-locals *env* (set fn-names))
        ;; Analyze each function spec
        fns (binding [*env* new-env]
              (mapv (fn [[fname params & fn-body]]
                      {:name fname
                       :params (vec params)
                       :body (mapv analyze fn-body)})
                    fn-specs))]
    (ast-node :letfn
              :fns fns
              :body (binding [*env* new-env]
                      (mapv analyze body)))))

(defn analyze-defmulti
  "Analyze (defmulti name dispatch-fn & options) forms.
   Creates a multimethod definition with a dispatch function."
  [[_ name dispatch-fn & options]]
  (ast-node :defmulti
            :name name
            :dispatch-fn (analyze dispatch-fn)
            :options options))

(defn analyze-defmethod
  "Analyze (defmethod name dispatch-val [params] body) forms.
   Creates a method implementation for a multimethod."
  [[_ name dispatch-val params & body]]
  (let [;; Check if any param element needs destructuring
        ;; params is a vector like [shape] or [{:keys [w h]}]
        has-destructure? (some destructure-pattern? params)
        ;; For simple case, just use the params directly
        ;; For destructuring, we need to expand each param
        expanded-pairs (when has-destructure?
                         ;; Create bindings for each param
                         (loop [idx 0
                                remaining params
                                bindings []]
                           (if (empty? remaining)
                             bindings
                             (let [param (first remaining)
                                   arg-sym (symbol (str "arg" idx))]
                               (if (destructure-pattern? param)
                                 (recur (inc idx)
                                        (rest remaining)
                                        (into bindings (expand-destructuring param arg-sym)))
                                 (recur (inc idx)
                                        (rest remaining)
                                        (conj bindings [param arg-sym])))))))
        ;; Analyze bindings sequentially like let does
        binding-nodes (when has-destructure?
                        (loop [remaining expanded-pairs
                               nodes []
                               env (with-locals *env* (set (map #(symbol (str "arg" %)) (range (count params)))))]
                          (if (empty? remaining)
                            nodes
                            (let [[sym init] (first remaining)
                                  analyzed (binding [*env* env]
                                             (analyze init))
                                  new-env (with-locals env #{sym})]
                              (recur (rest remaining)
                                     (conj nodes {:name sym :init analyzed})
                                     new-env)))))
        ;; Generate param names
        processed-params (if has-destructure?
                           (mapv #(symbol (str "arg" %)) (range (count params)))
                           params)
        ;; All locals for body analysis
        all-locals (if has-destructure?
                     (into (set processed-params) (map first expanded-pairs))
                     (set params))]
    (ast-node :defmethod
              :name name
              :dispatch-val dispatch-val
              :params processed-params
              :destructure-bindings binding-nodes
              :body (binding [*env* (with-locals *env* all-locals)]
                      (mapv analyze body)))))

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
;; Interop Detection
;; ============================================================================

(defn- interop-symbol?
  "Returns true if symbol represents an Elisp interop call.
   Matches: .method, .-field, and elisp/fn patterns."
  [sym]
  (when (symbol? sym)
    (let [sym-name (name sym)
          sym-ns (namespace sym)]
      (or (.startsWith sym-name ".")
          (= sym-ns "elisp")))))

;; ============================================================================
;; Invocation Analyzer
;; ============================================================================

(defn analyze-invoke
  "Analyze function invocation (f args...).
   Detects Elisp interop patterns:
   - (.method args...) → :interop-call with :method stripped of leading dot
   - (.-field)         → :interop-call with :method stripped of leading .-
   - (elisp/fn args..) → :elisp-call with :fn as raw Elisp name"
  [[f & args]]
  (let [f-name (when (symbol? f) (name f))
        f-ns (when (symbol? f) (namespace f))]
    (cond
      ;; Property access: (.-point) → zero-arg Elisp function call
      (and f-name (.startsWith f-name ".-"))
      (ast-node :interop-call
                :method (subs f-name 2)
                :args (mapv analyze args))

      ;; Dot-call: (.buffer-name args...) → Elisp function call
      (and f-name (.startsWith f-name "."))
      (ast-node :interop-call
                :method (subs f-name 1)
                :args (mapv analyze args))

      ;; Elisp namespace: (elisp/message "hi") → raw Elisp call
      (= f-ns "elisp")
      (ast-node :elisp-call
                :fn f-name
                :args (mapv analyze args))

      ;; Default invocation
      :else
      (ast-node :invoke
                :fn (analyze f)
                :args (mapv analyze args)))))

;; ============================================================================
;; Main Analyzer
;; ============================================================================

(def special-forms
  "Map of special form symbols to their analyzers."
  {'def analyze-def
   'defn analyze-defn
   'defmulti analyze-defmulti
   'defmethod analyze-defmethod
   'fn analyze-fn
   'fn* analyze-fn
   'let analyze-let
   'let* analyze-let
   'letfn analyze-letfn
   'if analyze-if
   'when analyze-when
   'cond analyze-cond
   'case analyze-case
   'do analyze-do
   'and analyze-and
   'or analyze-or
   'ns analyze-ns
   'quote analyze-quote
   'loop analyze-loop
   'recur analyze-recur
   'try analyze-try
   'throw analyze-throw
   'lazy-seq analyze-lazy-seq})

(defn analyze
  "Analyze a Clojure form into an AST node."
  [form]
  ;; Macroexpand first to handle ->, ->>, doto, cond->, etc.
  ;; Skip macroexpand for interop forms (.method, .-field, elisp/fn)
  ;; since Clojure would try to handle them as Java interop.
  (let [form (if (and (seq? form)
                      (symbol? (first form))
                      (not (contains? special-forms (first form)))
                      (not (interop-symbol? (first form))))
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
