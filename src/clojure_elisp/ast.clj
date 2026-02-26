(ns clojure-elisp.ast
  "AST node schema registry and validation.

   Provides lightweight structural validation for AST nodes produced
   by the analyzer. Not a full ADT (40+ ops would be unwieldy), but
   a schema map for catching missing keys early in development/testing.")

;; ============================================================================
;; AST Node Schemas
;; ============================================================================

(def ast-schemas
  "Map of :op -> required keys for each AST node type.
   Every AST node implicitly has :op and :env."
  {:const               #{:val :type}
   :local               #{:name}
   :var                 #{:name}
   :def                 #{:name}
   :defn                #{:name :body}
   :fn                  #{:params :body}
   :let                 #{:bindings :body}
   :letfn               #{:fns :body}
   :if                  #{:test :then}
   :when                #{:test :body}
   :cond                #{:clauses}
   :case                #{:expr :clauses}
   :do                  #{:body}
   :and                 #{:exprs}
   :or                  #{:exprs}
   :ns                  #{:name :requires}
   :quote               #{:form}
   :loop                #{:bindings :body}
   :recur               #{:args}
   :try                 #{:body}
   :throw               #{:exception :exception-type}
   :lazy-seq            #{:body}
   :invoke              #{:fn :args}
   :interop-call        #{:method :args}
   :elisp-call          #{:fn :args}
   :vector              #{:items}
   :map                 #{:keys :vals}
   :set                 #{:items}
   :defmulti            #{:name :dispatch-fn}
   :defmethod           #{:name :dispatch-val :params :body}
   :defprotocol         #{:name :methods}
   :defrecord           #{:name :fields :protocols}
   :deftype             #{:name :fields :protocols}
   :set!                #{:target :value}
   :extend-type         #{:type :protocols}
   :extend-protocol     #{:name :extensions}
   :satisfies?          #{:protocol :value}
   :reify               #{:protocols :closed-over}
   :defmacro            #{:name}
   :doseq               #{:clauses :body}
   :dotimes             #{:binding :count :body}
   :for                 #{:clauses :body}
   :save-excursion      #{:body}
   :save-restriction    #{:body}
   :with-current-buffer #{:buffer :body}
   :with-temp-buffer    #{:body}
   :save-current-buffer #{:body}
   :with-output-to-string #{:body}
   :with-eval-after-load #{:feature :body}
   :define-minor-mode   #{:name}
   :defgroup            #{:name}
   :defcustom           #{:name}
   :literal-vector        #{:items}
   :transient-define-prefix #{:name}
   :unknown             #{:form}})

;; ============================================================================
;; Validation
;; ============================================================================

(defn validate-ast-node
  "Validate an AST node has required keys for its :op.
   Returns the node if valid, throws ex-info if keys are missing.
   Nodes with unrecognized :op values pass through (open extension)."
  [node]
  (when-let [required (get ast-schemas (:op node))]
    (let [missing (remove #(contains? node %) required)]
      (when (seq missing)
        (throw (ex-info (str "AST node :" (:op node) " missing keys: " (vec missing))
                        {:op (:op node) :missing (vec missing) :node node})))))
  node)
