(ns clojure-elisp.analyzer
  "Analyzer for ClojureElisp.

   Transforms Clojure forms into an AST suitable for Elisp emission.
   We use a simplified approach rather than tools.analyzer for now,
   keeping it pragmatic and easy to understand."
  (:require [clojure-elisp.macros :as macros]
            [clojure-elisp.destructure :as destructure]))

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
;; Macro Registry (delegated to clojure-elisp.macros)
;; ============================================================================

(def macro-registry macros/macro-registry)
(def clear-macros! macros/clear-macros!)
(def get-macro macros/get-macro)
(def register-macro! macros/register-macro!)
(def macroexpand-1-clel macros/macroexpand-1-clel)
(def macroexpand-clel macros/macroexpand-clel)

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

(defn- analyze-multi-arity-defn
  "Analyze multi-arity defn: (defn foo ([x] x) ([x y] (+ x y)))."
  [name docstring fdecl]
  (let [arities (for [arity fdecl]
                  (let [[params & body] arity
                        variadic?       (some #{'&} params)
                        fixed-params    (if variadic?
                                          (vec (take-while #(not= '& %) params))
                                          params)
                        rest-param      (when variadic?
                                          (last params))
                        all-param-syms  (set (remove #{'&} params))]
                    {:params params
                     :fixed-params fixed-params
                     :rest-param rest-param
                     :variadic? (boolean variadic?)
                     :arity (if variadic? :variadic (count params))
                     :body (binding [*env* (with-locals *env* all-param-syms)]
                             (mapv analyze body))}))]
    (ast-node :defn
              :name name
              :docstring docstring
              :multi-arity? true
              :arities (vec arities))))

(defn- analyze-single-arity-defn
  "Analyze single-arity defn: (defn foo [x] body)."
  [name docstring fdecl]
  (let [[params & body]                                                                      fdecl
        params-vec                                                                           (if (vector? params) params (first params))
        {:keys [simple-params rest-param destructure-bindings all-locals]}
        (destructure/process-fn-params params-vec)

        effective-params                                                                     (if rest-param
                                                                                               (conj simple-params '& rest-param)
                                                                                               simple-params)

        effective-body
        (if (seq destructure-bindings)
          (let [let-bindings (vec (mapcat (fn [[pattern gsym]]
                                            (destructure/expand-destructuring pattern gsym))
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
                      (mapv analyze effective-body)))))

(defn analyze-defn
  "Analyze (defn name [args] body) and (defn name ([args1] body1) ([args2] body2)) forms.
   Also handles (defn- name ...) — the head symbol is checked for private semantics."
  [[head name & fdecl]]
  (let [private?          (or (= head 'defn-)
                              (:private (meta name)))
        [docstring fdecl] (if (string? (first fdecl))
                            [(first fdecl) (rest fdecl)]
                            [nil fdecl])
        multi-arity?      (and (seq? (first fdecl))
                               (vector? (ffirst fdecl)))
        base-node         (if multi-arity?
                            (analyze-multi-arity-defn name docstring fdecl)
                            (analyze-single-arity-defn name docstring fdecl))]
    (if private?
      (assoc base-node :private? true)
      base-node)))

(defn analyze-fn
  "Analyze (fn [args] body) forms.
   Supports destructuring patterns and & rest args in parameters."
  [[_ & fdecl]]
  (let [[params & body]                                                                      (if (vector? (first fdecl))
                                                                                               fdecl
                                                                                               (first fdecl))
        {:keys [simple-params rest-param destructure-bindings all-locals]}
        (destructure/process-fn-params params)

        ;; Build the effective params for the AST
        effective-params                                                                     (if rest-param
                                                                                               (conj simple-params '& rest-param)
                                                                                               simple-params)

        ;; If there are destructuring bindings, wrap body in a let
        effective-body
        (if (seq destructure-bindings)
          ;; Create a let form with all the destructure bindings
          (let [let-bindings (vec (mapcat (fn [[pattern gsym]]
                                            (destructure/expand-destructuring pattern gsym))
                                          destructure-bindings))]
            [(list 'let (vec (mapcat (fn [[sym init]] [sym init]) let-bindings))
                   (cons 'do body))])
          body)]
    (ast-node :fn
              :params effective-params
              :rest-param rest-param
              :body (binding [*env* (with-locals *env* all-locals)]
                      (mapv analyze effective-body)))))

(defn analyze-lambda
  "Analyze (lambda (args) body) forms — Elisp-style lambda with list params.
   Normalizes to fn AST node by converting list params to vector."
  [[_ params & body]]
  (let [params-vec (vec (if (seq? params) params []))]
    (analyze-fn (list* 'fn params-vec body))))

;; ============================================================================
;; Destructuring Support (delegated to clojure-elisp.destructure)
;; ============================================================================

(defn analyze-let
  "Analyze (let [bindings] body) forms.
   Supports destructuring patterns in bindings."
  [[_ bindings & body]]
  (let [;; Expand destructuring into simple bindings
        expanded-pairs (destructure/expand-bindings bindings)
        ;; Analyze each binding sequentially, updating env
        binding-nodes  (loop [remaining expanded-pairs
                              nodes     []
                              env       *env*]
                         (if (empty? remaining)
                           nodes
                           (let [[sym init] (first remaining)
                                 analyzed   (binding [*env* env]
                                              (analyze init))
                                 new-env    (with-locals env #{sym})]
                             (recur (rest remaining)
                                    (conj nodes {:name sym :init analyzed})
                                    new-env))))
        all-locals     (into (:locals *env*) (map first expanded-pairs))]
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
        pairs        (if has-default?
                       (partition 2 (butlast clauses))
                       (partition 2 clauses))
        default      (when has-default? (last clauses))]
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
          opts-map         (apply hash-map opts)]
      {:ns ns-name
       :as (:as opts-map)
       :refer (:refer opts-map)})
    {:ns spec}))

(defn build-ns-env
  "Build environment entries from parsed require specs.
   Returns a map with :aliases and :refers suitable for merging into *env*."
  [requires]
  (let [aliases (into {} (for [{:keys [ns as]} requires
                               :when           as]
                           [as ns]))
        refers  (into {} (for [{:keys [ns refer]} requires
                               :when              refer
                               sym                refer]
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
        syms  (mapv first pairs)
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
        catch?          #(and (seq? %) (= 'catch (first %)))
        finally?        #(and (seq? %) (= 'finally (first %)))
        special-clause? #(or (catch? %) (finally? %))

        body-exprs      (vec (take-while (complement special-clause?) exprs))
        clauses         (drop-while (complement special-clause?) exprs)

        ;; Parse catch clauses: (catch ExType e body...)
        catch-clauses   (filter catch? clauses)
        catches         (mapv (fn [[_ ex-type binding & handler]]
                                (let [local-env (with-locals *env* #{binding})]
                                  {:type ex-type
                                   :name binding
                                   :body (binding [*env* local-env]
                                           (mapv analyze handler))}))
                              catch-clauses)

        ;; Parse finally clause: (finally body...)
        finally-clause  (first (filter finally? clauses))
        finally-body    (when finally-clause
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
        ex-type     (cond
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
        new-env  (with-locals *env* (set fn-names))
        ;; Analyze each function spec
        fns      (binding [*env* new-env]
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
        has-destructure? (some destructure/destructure-pattern? params)
        ;; For simple case, just use the params directly
        ;; For destructuring, we need to expand each param
        expanded-pairs   (when has-destructure?
                         ;; Create bindings for each param
                           (loop [idx       0
                                  remaining params
                                  bindings  []]
                             (if (empty? remaining)
                               bindings
                               (let [param   (first remaining)
                                     arg-sym (symbol (str "arg" idx))]
                                 (if (destructure/destructure-pattern? param)
                                   (recur (inc idx)
                                          (rest remaining)
                                          (into bindings (destructure/expand-destructuring param arg-sym)))
                                   (recur (inc idx)
                                          (rest remaining)
                                          (conj bindings [param arg-sym])))))))
        ;; Analyze bindings sequentially like let does
        binding-nodes    (when has-destructure?
                           (loop [remaining expanded-pairs
                                  nodes     []
                                  env       (with-locals *env* (set (map #(symbol (str "arg" %)) (range (count params)))))]
                             (if (empty? remaining)
                               nodes
                               (let [[sym init] (first remaining)
                                     analyzed   (binding [*env* env]
                                                  (analyze init))
                                     new-env    (with-locals env #{sym})]
                                 (recur (rest remaining)
                                        (conj nodes {:name sym :init analyzed})
                                        new-env)))))
        ;; Generate param names
        processed-params (if has-destructure?
                           (mapv #(symbol (str "arg" %)) (range (count params)))
                           params)
        ;; All locals for body analysis
        all-locals       (if has-destructure?
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
  (loop [remaining        (seq body)
         current-protocol nil
         protocols        []
         current-methods  []]
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
                all-locals                         (into (set fields) (set params))]
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
        protocols  (parse-protocol-impls body field-syms)]
    (ast-node :defrecord
              :name record-name
              :fields field-syms
              :protocols protocols)))

(defn analyze-deftype
  "Analyze (deftype Name [fields...] Protocol (method [this] body) ...) forms.
   Fields may have ^:mutable metadata."
  [[_ type-name fields & body]]
  (let [field-syms     (vec fields)
        mutable-fields (set (filter #(:mutable (meta %)) field-syms))
        ;; Strip metadata for clean field names
        clean-fields   (mapv #(with-meta % nil) field-syms)
        protocols      (parse-protocol-impls body clean-fields)]
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
          (loop [remaining       (seq forms)
                 current-type    nil
                 extensions      []
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
  (loop [remaining        (seq body)
         current-protocol nil
         protocols        []
         current-methods  []]
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
                all-locals                         (into (set params) closed-locals)]
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
        protocols     (analyze-reify-protocols body closed-locals)]
    (ast-node :reify
              :protocols protocols
              :closed-over (vec closed-locals))))

;; ============================================================================
;; Comment, Binding, Assert (clel-050)
;; ============================================================================

(defn analyze-comment
  "Analyze (comment ...) forms. Returns a no-op AST node."
  [_form]
  (ast-node :comment))

(defn analyze-binding
  "Analyze (binding [var val ...] body...) forms.
   In Elisp, dynamically-scoped variables are rebound via let,
   so this maps directly to a let form with dynamic binding semantics."
  [[_ bindings & body]]
  (let [pairs (partition 2 bindings)
        analyzed-bindings (mapv (fn [[sym val]]
                                  {:name sym :init (analyze val)})
                                pairs)
        analyzed-body (mapv analyze body)]
    (ast-node :binding
              :bindings analyzed-bindings
              :body analyzed-body)))

(defn analyze-assert
  "Analyze (assert test) or (assert test message) forms."
  [[_ test & [message]]]
  (ast-node :assert
            :test (analyze test)
            :message (when message (analyze message))))

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
;; Iteration Forms (clel-035, clel-045)
;; ============================================================================

(defn- parse-iteration-clauses
  "Parse a binding vector into a sequence of clauses.
   Each clause is one of:
   - {:type :binding :sym symbol :coll form}  - x coll
   - {:type :let :bindings [[sym form]...]}   - :let [x expr y expr2]
   - {:type :when :pred form}                 - :when predicate
   - {:type :while :pred form}                - :while predicate

   Handles: [x xs :when p y ys :let [z expr] :when q]"
  [bindings]
  (loop [remaining (seq bindings)
         clauses   []]
    (if (empty? remaining)
      clauses
      (let [item (first remaining)]
        (cond
          ;; :when modifier
          (= :when item)
          (recur (drop 2 remaining)
                 (conj clauses {:type :when :pred (second remaining)}))

          ;; :while modifier
          (= :while item)
          (recur (drop 2 remaining)
                 (conj clauses {:type :while :pred (second remaining)}))

          ;; :let modifier
          (= :let item)
          (let [let-vec (second remaining)
                pairs   (vec (partition 2 let-vec))]
            (recur (drop 2 remaining)
                   (conj clauses {:type :let :bindings pairs})))

          ;; Regular binding: sym coll
          (symbol? item)
          (recur (drop 2 remaining)
                 (conj clauses {:type :binding :sym item :coll (second remaining)}))

          ;; Unknown, skip
          :else
          (recur (rest remaining) clauses))))))

(defn- analyze-iteration-clause
  "Analyze a single iteration clause, updating env as needed.
   Returns [analyzed-clause updated-locals]."
  [clause current-locals]
  (case (:type clause)
    :binding
    (let [coll-ast   (binding [*env* (with-locals *env* current-locals)]
                       (analyze (:coll clause)))
          new-locals (conj current-locals (:sym clause))]
      [{:type :binding
        :sym (:sym clause)
        :coll coll-ast}
       new-locals])

    :let
    (let [pairs                                                                (:bindings clause)
          ;; Analyze let bindings sequentially
          [analyzed-lets final-locals]
          (reduce (fn [[lets locals] [sym form]]
                    (let [init-ast   (binding [*env* (with-locals *env* locals)]
                                       (analyze form))
                          new-locals (conj locals sym)]
                      [(conj lets {:name sym :init init-ast}) new-locals]))
                  [[] current-locals]
                  pairs)]
      [{:type :let :bindings analyzed-lets} final-locals])

    :when
    (let [pred-ast (binding [*env* (with-locals *env* current-locals)]
                     (analyze (:pred clause)))]
      [{:type :when :pred pred-ast} current-locals])

    :while
    (let [pred-ast (binding [*env* (with-locals *env* current-locals)]
                     (analyze (:pred clause)))]
      [{:type :while :pred pred-ast} current-locals])))

(defn- analyze-iteration-clauses
  "Analyze all iteration clauses, threading environment through.
   Returns [analyzed-clauses all-locals]."
  [clauses initial-locals]
  (reduce (fn [[analyzed locals] clause]
            (let [[analyzed-clause new-locals] (analyze-iteration-clause clause locals)]
              [(conj analyzed analyzed-clause) new-locals]))
          [[] initial-locals]
          clauses))

(defn analyze-doseq
  "Analyze (doseq [x coll :when p y coll2 :let [z expr]] body...) forms.
   Supports multiple bindings with :when, :while, and :let modifiers.
   Iterates over collections, executing body for side effects."
  [[_ bindings & body]]
  (let [clauses                       (parse-iteration-clauses bindings)
        [analyzed-clauses all-locals] (analyze-iteration-clauses clauses #{})]
    (ast-node :doseq
              :clauses analyzed-clauses
              :body (binding [*env* (with-locals *env* all-locals)]
                      (mapv analyze body)))))

(defn analyze-dotimes
  "Analyze (dotimes [i n] body...) forms.
   Executes body n times, with i bound to 0, 1, ..., n-1."
  [[_ bindings & body]]
  (let [[sym count-form] (take 2 bindings)
        count-ast        (analyze count-form)]
    (ast-node :dotimes
              :binding sym
              :count count-ast
              :body (binding [*env* (with-locals *env* #{sym})]
                      (mapv analyze body)))))

(defn analyze-for
  "Analyze (for [x coll :when pred y coll2 :let [z expr]] body) forms.
   List comprehension that returns a lazy sequence.
   Supports multiple bindings with :when, :while, and :let modifiers."
  [[_ bindings & body]]
  (let [clauses                       (parse-iteration-clauses bindings)
        [analyzed-clauses all-locals] (analyze-iteration-clauses clauses #{})]
    (ast-node :for
              :clauses analyzed-clauses
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
        parse-options          (fn [forms]
                                 (loop [remaining forms
                                        options   {}]
                                   (if (and (seq remaining)
                                            (keyword? (first remaining))
                                            (seq (rest remaining)))
                                     (recur (drop 2 remaining)
                                            (assoc options (first remaining) (second remaining)))
                                     [options remaining])))
        [options body-forms]   (parse-options rest-forms)]
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
        options                (apply hash-map rest-forms)]
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
        options                (apply hash-map rest-forms)]
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
        [params & body]   fdecl
        ;; Build and eval a Clojure fn from the macro body.
        ;; Since the compiler runs on the JVM, the macro fn executes
        ;; at compile time and produces forms for analysis.
        macro-fn          (eval (list* 'fn params body))]
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
          sym-ns   (namespace sym)]
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
        f-ns   (when (symbol? f) (namespace f))]
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
   'defn- analyze-defn
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
   'lambda analyze-lambda
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
   ;; Comment, binding, assert (clel-050)
   'comment analyze-comment
   'binding analyze-binding
   'assert analyze-assert
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

(defn- analyze-symbol
  "Analyze a symbol form, resolving locals, aliases, refers, and vars."
  [form]
  (let [sym-ns-str (namespace form)
        sym-name   (symbol (name form))]
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
      (ast-node :var :name form))))

(defn- analyze-seq
  "Analyze a seq form: dispatch to special form, macro, or invocation."
  [form]
  (let [op (first form)]
    (if-let [analyzer (get special-forms op)]
      (analyzer form)
      ;; Check ClojureElisp macro registry before treating as invoke
      (if-let [macro-fn (when (symbol? op) (get-macro op))]
        (let [expanded (apply macro-fn (rest form))]
          (analyze expanded))
        (analyze-invoke form)))))

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
          (analyze-symbol form)

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
          (analyze-seq form)

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
