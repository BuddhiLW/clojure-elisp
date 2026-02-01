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
   :aliases {}
   :refers {}
   :in-tail-position? false})

(def ^:dynamic *source-context*
  "Current source location context {:line N :column N}, or nil."
  nil)

(defn with-locals
  "Add locals to the environment."
  [env locals]
  (update env :locals into locals))

;; ============================================================================
;; Macro Registry
;; ============================================================================

(defonce ^{:doc "Compile-time macro registry. Maps symbol -> macro fn."}
  macro-registry
  (atom {}))

(defn clear-macros!
  "Clear all registered macros. Useful for tests."
  []
  (reset! macro-registry {}))

(defn get-macro
  "Look up a macro by name. Returns the macro fn or nil."
  [sym]
  (get @macro-registry sym))

(defn register-macro!
  "Register a macro fn under the given name."
  [sym macro-fn]
  (swap! macro-registry assoc sym macro-fn))

(defn macroexpand-1-clel
  "Expand a ClojureElisp macro form by one step.
   If the form is a list whose first element is a registered macro,
   applies the macro fn and returns the result. Otherwise returns
   the form unchanged."
  [form]
  (if (and (seq? form)
           (symbol? (first form)))
    (if-let [macro-fn (get-macro (first form))]
      (apply macro-fn (rest form))
      form)
    form))

(defn macroexpand-clel
  "Fully expand a ClojureElisp macro form.
   Repeatedly applies macroexpand-1-clel until the form stops changing."
  [form]
  (let [expanded (macroexpand-1-clel form)]
    (if (identical? expanded form)
      form
      (recur expanded))))

;; ============================================================================
;; Source Location
;; ============================================================================

(defn extract-source-location
  "Extract :line and :column from form metadata, if available."
  [form]
  (when-let [m (meta form)]
    (when (or (:line m) (:column m))
      (cond-> {}
        (:line m)   (assoc :line (:line m))
        (:column m) (assoc :column (:column m))))))

(defn analysis-error
  "Create an ex-info with source location context."
  [msg data]
  (let [loc *source-context*]
    (ex-info (if loc
               (format "%s at %s:%s"
                       msg
                       (or (:line loc) "?")
                       (or (:column loc) "?"))
               msg)
             (merge data loc))))

;; ============================================================================
;; AST Node Constructors
;; ============================================================================

(defn ast-node
  "Create an AST node with common fields.
   Includes :line and :column from *source-context* when available."
  [op & {:as fields}]
  (merge {:op op :env *env*}
         *source-context*
         fields))

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

(defn build-ns-env
  "Build environment entries from parsed require specs.
   Returns a map with :aliases and :refers suitable for merging into *env*."
  [requires]
  (let [aliases (into {} (for [{:keys [ns as]} requires
                               :when as]
                           [as ns]))
        refers (into {} (for [{:keys [ns refer]} requires
                              :when refer
                              sym refer]
                          [sym ns]))]
    {:aliases aliases
     :refers refers}))

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
;; Protocol / Record / Type Support (clel-025)
;; ============================================================================

(defn- parse-protocol-impls
  "Parse protocol implementations from defrecord/deftype body.
   Body alternates between protocol name symbols and method implementations.
   Returns a vector of {:protocol name :methods [{:name :params :body}]}."
  [body fields]
  (loop [remaining (seq body)
         current-protocol nil
         protocols []
         current-methods []]
    (if (nil? remaining)
      (if current-protocol
        (conj protocols {:protocol current-protocol :methods current-methods})
        protocols)
      (let [item (first remaining)]
        (if (symbol? item)
          ;; New protocol name — flush previous
          (let [protocols (if current-protocol
                            (conj protocols {:protocol current-protocol :methods current-methods})
                            protocols)]
            (recur (next remaining) item protocols []))
          ;; Method implementation: (method-name [this args...] body...)
          (let [[method-name params & method-body] item
                all-locals (into (set fields) (set params))]
            (recur (next remaining)
                   current-protocol
                   protocols
                   (conj current-methods
                         {:name method-name
                          :params (vec params)
                          :body (binding [*env* (with-locals *env* all-locals)]
                                  (mapv analyze method-body))}))))))))

(defn analyze-defprotocol
  "Analyze (defprotocol Name (method [this args...]) ...) forms.
   Each method signature is (name [params...])."
  [[_ proto-name & method-sigs]]
  (let [methods (mapv (fn [sig]
                        (let [[method-name params] sig]
                          {:name method-name
                           :params (vec params)}))
                      (remove string? method-sigs))]
    (ast-node :defprotocol
              :name proto-name
              :methods methods)))

(defn analyze-defrecord
  "Analyze (defrecord Name [fields...] Protocol (method [this] body) ...) forms."
  [[_ record-name fields & body]]
  (let [field-syms (vec fields)
        protocols (parse-protocol-impls body field-syms)]
    (ast-node :defrecord
              :name record-name
              :fields field-syms
              :protocols protocols)))

(defn analyze-deftype
  "Analyze (deftype Name [fields...] Protocol (method [this] body) ...) forms.
   Fields may have ^:mutable metadata."
  [[_ type-name fields & body]]
  (let [field-syms (vec fields)
        mutable-fields (set (filter #(:mutable (meta %)) field-syms))
        ;; Strip metadata for clean field names
        clean-fields (mapv #(with-meta % nil) field-syms)
        protocols (parse-protocol-impls body clean-fields)]
    (ast-node :deftype
              :name type-name
              :fields clean-fields
              :mutable-fields mutable-fields
              :protocols protocols)))

(defn analyze-set!
  "Analyze (set! target value) forms.
   Used within deftype methods to mutate mutable fields."
  [[_ target value]]
  (ast-node :set!
            :target target
            :value (analyze value)))

(defn analyze-extend-type
  "Analyze (extend-type Type Protocol (method [this args] body) ...) forms.
   Extends multiple protocols to an existing type."
  [[_ type-sym & body]]
  (let [protocols (parse-protocol-impls body [])]
    (ast-node :extend-type
              :type type-sym
              :protocols protocols)))

(defn analyze-extend-protocol
  "Analyze (extend-protocol Protocol Type1 (method [this] body) Type2 ...) forms.
   Extends a single protocol to multiple types."
  [[_ protocol-name & body]]
  (let [;; Parse body: alternating Type symbols and method implementations
        parse-extensions
        (fn [forms]
          (loop [remaining (seq forms)
                 current-type nil
                 extensions []
                 current-methods []]
            (if (nil? remaining)
              (if current-type
                (conj extensions {:type current-type :methods current-methods})
                extensions)
              (let [item (first remaining)]
                (if (symbol? item)
                  ;; New type name - flush previous
                  (let [extensions (if current-type
                                     (conj extensions {:type current-type :methods current-methods})
                                     extensions)]
                    (recur (next remaining) item extensions []))
                  ;; Method implementation: (method-name [this args...] body...)
                  (let [[method-name params & method-body] item]
                    (recur (next remaining)
                           current-type
                           extensions
                           (conj current-methods
                                 {:name method-name
                                  :params (vec params)
                                  :body (binding [*env* (with-locals *env* (set params))]
                                          (mapv analyze method-body))}))))))))]
    (ast-node :extend-protocol
              :name protocol-name
              :extensions (parse-extensions body))))

(defn analyze-satisfies?
  "Analyze (satisfies? Protocol value) forms.
   Checks if a value satisfies a protocol at runtime."
  [[_ protocol-name value]]
  (ast-node :satisfies?
            :protocol protocol-name
            :value (analyze value)))

(defn- analyze-reify-protocols
  "Parse reify protocol implementations."
  [body closed-locals]
  (loop [remaining (seq body)
         current-protocol nil
         protocols []
         current-methods []]
    (if (nil? remaining)
      (if current-protocol
        (conj protocols {:protocol current-protocol :methods current-methods})
        protocols)
      (let [item (first remaining)]
        (if (symbol? item)
          ;; New protocol name - flush previous
          (let [protocols (if current-protocol
                            (conj protocols {:protocol current-protocol :methods current-methods})
                            protocols)]
            (recur (next remaining) item protocols []))
          ;; Method implementation
          (let [[method-name params & method-body] item
                all-locals (into (set params) closed-locals)]
            (recur (next remaining)
                   current-protocol
                   protocols
                   (conj current-methods
                         {:name method-name
                          :params (vec params)
                          :body (binding [*env* (with-locals *env* all-locals)]
                                  (mapv analyze method-body))}))))))))

(defn analyze-reify
  "Analyze (reify Protocol (method [this] body) ...) forms.
   Creates an anonymous type implementing protocols."
  [[_ & body]]
  (let [;; Capture closed-over locals from current environment
        closed-locals (or (:locals *env*) #{})
        protocols (analyze-reify-protocols body closed-locals)]
    (ast-node :reify
              :protocols protocols
              :closed-over (vec closed-locals))))

;; ============================================================================
;; Emacs Buffer/Process Interop (clel-031)
;; ============================================================================

(defn analyze-save-excursion
  "Analyze (save-excursion body...) forms.
   Saves point and mark, executes body, then restores them."
  [[_ & body]]
  (ast-node :save-excursion
            :body (mapv analyze body)))

(defn analyze-save-restriction
  "Analyze (save-restriction body...) forms.
   Saves the current narrowing state, executes body, then restores it."
  [[_ & body]]
  (ast-node :save-restriction
            :body (mapv analyze body)))

(defn analyze-with-current-buffer
  "Analyze (with-current-buffer buffer body...) forms.
   Executes body with buffer as the current buffer."
  [[_ buffer & body]]
  (ast-node :with-current-buffer
            :buffer (analyze buffer)
            :body (mapv analyze body)))

(defn analyze-with-temp-buffer
  "Analyze (with-temp-buffer body...) forms.
   Creates a temporary buffer, executes body in it, then kills the buffer."
  [[_ & body]]
  (ast-node :with-temp-buffer
            :body (mapv analyze body)))

(defn analyze-save-current-buffer
  "Analyze (save-current-buffer body...) forms.
   Saves the current buffer, executes body, then restores current buffer."
  [[_ & body]]
  (ast-node :save-current-buffer
            :body (mapv analyze body)))

(defn analyze-with-output-to-string
  "Analyze (with-output-to-string body...) forms.
   Captures all output to a string and returns it."
  [[_ & body]]
  (ast-node :with-output-to-string
            :body (mapv analyze body)))

;; ============================================================================
;; Iteration Forms (clel-035)
;; ============================================================================

(defn analyze-doseq
  "Analyze (doseq [x coll] body...) forms.
   Iterates over coll, binding each element to x and executing body for side effects."
  [[_ bindings & body]]
  (let [[sym coll-form] (take 2 bindings)
        coll-ast (analyze coll-form)]
    (ast-node :doseq
              :binding sym
              :coll coll-ast
              :body (binding [*env* (with-locals *env* #{sym})]
                      (mapv analyze body)))))

(defn analyze-dotimes
  "Analyze (dotimes [i n] body...) forms.
   Executes body n times, with i bound to 0, 1, ..., n-1."
  [[_ bindings & body]]
  (let [[sym count-form] (take 2 bindings)
        count-ast (analyze count-form)]
    (ast-node :dotimes
              :binding sym
              :count count-ast
              :body (binding [*env* (with-locals *env* #{sym})]
                      (mapv analyze body)))))

(defn parse-for-bindings
  "Parse for binding vector into structured form.
   Returns {:binding sym :coll ast :when ast|nil :let [{:name :init}]|nil}
   Supports: [x coll], [x coll :when pred], [x coll :let [y expr]], [x coll :when pred :let [y expr]]"
  [bindings]
  (let [[sym coll-form & modifiers] bindings
        coll-ast (analyze coll-form)]
    (loop [mods modifiers
           when-clause nil
           let-bindings nil]
      (if (empty? mods)
        {:binding sym
         :coll coll-ast
         :when when-clause
         :let let-bindings}
        (let [[kw & rest-mods] mods]
          (case kw
            :when (let [[pred & remaining] rest-mods]
                    (recur remaining
                           (binding [*env* (with-locals *env* #{sym})]
                             (analyze pred))
                           let-bindings))
            :let (let [[let-vec & remaining] rest-mods
                       pairs (partition 2 let-vec)
                       analyzed-lets (binding [*env* (with-locals *env* #{sym})]
                                       (mapv (fn [[n v]]
                                               {:name n :init (analyze v)})
                                             pairs))]
                   (recur remaining
                          when-clause
                          analyzed-lets))
            :while (let [[pred & remaining] rest-mods]
                     ;; :while is more complex - for now, treat like :when
                     ;; Full implementation would need early termination
                     (recur remaining
                            (binding [*env* (with-locals *env* #{sym})]
                              (analyze pred))
                            let-bindings))
            ;; Unknown modifier, skip
            (recur (rest mods) when-clause let-bindings)))))))

(defn analyze-for
  "Analyze (for [x coll :when pred :let [y expr]] body) forms.
   List comprehension that returns a lazy sequence.
   Supports :when for filtering and :let for intermediate bindings."
  [[_ bindings & body]]
  (let [parsed (parse-for-bindings bindings)
        for-binding (:binding parsed)
        when-clause (:when parsed)
        let-clause (:let parsed)
        ;; Build environment with binding and any :let bindings
        let-syms (if let-clause (set (map :name let-clause)) #{})
        all-locals (into #{for-binding} let-syms)]
    (ast-node :for
              :binding for-binding
              :coll (:coll parsed)
              :when when-clause
              :let let-clause
              :body (binding [*env* (with-locals *env* all-locals)]
                      (mapv analyze body)))))

;; ============================================================================
;; Macro System
;; ============================================================================

(defn analyze-with-eval-after-load
  "Analyze (with-eval-after-load feature body...) forms.
   The feature is typically a quoted symbol or string."
  [[_ feature & body]]
  (ast-node :with-eval-after-load
            :feature (analyze feature)
            :body (mapv analyze body)))

(defn analyze-define-minor-mode
  "Analyze (define-minor-mode name docstring? options... body...) forms.
   Options are keyword-value pairs like :init-value, :lighter, :global, :group, :keymap.
   Body forms are executed when the mode is toggled."
  [[_ mode-name & rest-forms]]
  (let [;; Check if first element is a docstring
        [docstring rest-forms] (if (string? (first rest-forms))
                                 [(first rest-forms) (rest rest-forms)]
                                 [nil rest-forms])
        ;; Parse keyword options until we hit a non-keyword or run out
        parse-options (fn [forms]
                        (loop [remaining forms
                               options {}]
                          (if (and (seq remaining)
                                   (keyword? (first remaining))
                                   (seq (rest remaining)))
                            (recur (drop 2 remaining)
                                   (assoc options (first remaining) (second remaining)))
                            [options remaining])))
        [options body-forms] (parse-options rest-forms)]
    (ast-node :define-minor-mode
              :name mode-name
              :docstring docstring
              :options options
              :body (mapv analyze body-forms))))

(defn analyze-defgroup
  "Analyze (defgroup name value docstring? keyword-value-options...) forms.
   Options are keyword-value pairs like :group, :prefix, :tag, :link.

   Example:
   (defgroup hive-mcp-eca nil
     \"Integration between hive-mcp and ECA.\"
     :group 'hive-mcp
     :prefix \"hive-mcp-eca-\")"
  [[_ group-name value & rest-forms]]
  (let [;; Check if first element is a docstring
        [docstring rest-forms] (if (string? (first rest-forms))
                                 [(first rest-forms) (rest rest-forms)]
                                 [nil rest-forms])
        ;; Parse keyword options into a map
        options (apply hash-map rest-forms)]
    (ast-node :defgroup
              :name group-name
              :value value
              :docstring docstring
              :options options)))

(defn analyze-defcustom
  "Analyze (defcustom name default docstring? keyword-value-options...) forms.
   Options are keyword-value pairs like :type, :group, :safe, :set, :get, etc.

   Example:
   (defcustom hive-mcp-eca-timeout 30
     \"Timeout in seconds.\"
     :type 'integer
     :group 'hive-mcp-eca
     :safe 'integerp)"
  [[_ var-name default & rest-forms]]
  (let [;; Check if first element is a docstring
        [docstring rest-forms] (if (string? (first rest-forms))
                                 [(first rest-forms) (rest rest-forms)]
                                 [nil rest-forms])
        ;; Parse keyword options into a map
        options (apply hash-map rest-forms)]
    (ast-node :defcustom
              :name var-name
              :default default
              :docstring docstring
              :options options)))

(defn analyze-defmacro
  "Analyze (defmacro name [params] body) forms.
   Evaluates the macro body as a Clojure function and registers it
   in the compile-time macro registry. Returns a :defmacro AST node
   that the emitter should skip (macros are compile-time only)."
  [[_ name & fdecl]]
  (let [[docstring fdecl] (if (string? (first fdecl))
                            [(first fdecl) (rest fdecl)]
                            [nil fdecl])
        [params & body] fdecl
        ;; Build and eval a Clojure fn from the macro body.
        ;; Since the compiler runs on the JVM, the macro fn executes
        ;; at compile time and produces forms for analysis.
        macro-fn (eval (list* 'fn params body))]
    (register-macro! name macro-fn)
    (ast-node :defmacro
              :name name
              :docstring docstring
              :params params)))

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
   'defmacro analyze-defmacro
   'defmulti analyze-defmulti
   'defmethod analyze-defmethod
   'defprotocol analyze-defprotocol
   'defrecord analyze-defrecord
   'deftype analyze-deftype
   'set! analyze-set!
   'extend-type analyze-extend-type
   'extend-protocol analyze-extend-protocol
   'satisfies? analyze-satisfies?
   'reify analyze-reify
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
   'lazy-seq analyze-lazy-seq
   'with-eval-after-load analyze-with-eval-after-load
   'define-minor-mode analyze-define-minor-mode
   'defgroup analyze-defgroup
   'defcustom analyze-defcustom
   ;; Emacs buffer/process interop (clel-031)
   'save-excursion analyze-save-excursion
   'save-restriction analyze-save-restriction
   'with-current-buffer analyze-with-current-buffer
   'with-temp-buffer analyze-with-temp-buffer
   'save-current-buffer analyze-save-current-buffer
   'with-output-to-string analyze-with-output-to-string
   ;; Iteration forms (clel-035, clel-039)
   'doseq analyze-doseq
   'dotimes analyze-dotimes
   'for analyze-for})

(defn analyze
  "Analyze a Clojure form into an AST node.
   Captures source location from form metadata and propagates it
   through *source-context* so child nodes inherit location context."
  [form]
  ;; Extract source location from this form's metadata (if any).
  ;; If the form has location, use it; otherwise keep the parent's context.
  (let [loc (extract-source-location form)
        ctx (or loc *source-context*)]
    (binding [*source-context* ctx]
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
          (let [sym-ns-str (namespace form)
                sym-name (symbol (name form))]
            (cond
              ;; Local takes priority
              (contains? (:locals *env*) form)
              (ast-node :local :name form)

              ;; Aliased qualified symbol: str/join -> clojure.string/join
              (and sym-ns-str
                   (get (:aliases *env*) (symbol sym-ns-str)))
              (let [resolved-ns (get (:aliases *env*) (symbol sym-ns-str))]
                (ast-node :var :name sym-name :ns resolved-ns))

              ;; Already qualified symbol: clojure.string/join
              sym-ns-str
              (ast-node :var :name sym-name :ns (symbol sym-ns-str))

              ;; Referred symbol: join -> clojure.string/join
              (get (:refers *env*) form)
              (let [resolved-ns (get (:refers *env*) form)]
                (ast-node :var :name form :ns resolved-ns))

              ;; Unqualified, unresolved
              :else
              (ast-node :var :name form)))

          ;; Vector
          (vector? form)
          (analyze-vector form)

          ;; Map
          (map? form)
          (analyze-map form)

          ;; Set
          (set? form)
          (analyze-set form)

          ;; List (special form, macro, or invocation)
          (seq? form)
          (let [op (first form)]
            (if-let [analyzer (get special-forms op)]
              (analyzer form)
              ;; Check ClojureElisp macro registry before treating as invoke
              (if-let [macro-fn (when (symbol? op) (get-macro op))]
                (let [expanded (apply macro-fn (rest form))]
                  (analyze expanded))
                (analyze-invoke form))))

          :else
          (ast-node :unknown :form form))))))

;; ============================================================================
;; File-Level Analysis
;; ============================================================================

(defn analyze-file-forms
  "Analyze a sequence of forms as they appear in a file.
   If the first form is (ns ...), it establishes the namespace context
   (current ns, aliases, refers) for all subsequent forms."
  [forms]
  (if (and (seq forms)
           (seq? (first forms))
           (= 'ns (first (first forms))))
    (let [ns-ast (analyze (first forms))
          ns-env (build-ns-env (:requires ns-ast))]
      (binding [*env* (merge *env* {:ns (:name ns-ast)} ns-env)]
        (into [ns-ast] (mapv analyze (rest forms)))))
    (mapv analyze forms)))

(comment
  (analyze '(defn foo [x] (+ x 1)))
  (analyze '(let [a 1 b 2] (+ a b)))
  (analyze '(if (> x 0) "yes" "no")))
