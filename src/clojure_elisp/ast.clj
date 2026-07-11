(ns clojure-elisp.ast
  "AST node schema registry and validation, backed by Malli.

   Provides structural validation for AST nodes produced by the analyzer:
   required-key presence plus value-shape checks. Child positions that hold
   nested AST nodes are typed via the shared `clojure-elisp.schema` registry
   (`[:ref ::schema/node]`), so `valid-ast-node?` is RECURSIVE — a node with a
   mis-shaped child is rejected. Also exposes Malli generators for leaf nodes.

   `op->required` is the required-key table; it MUST stay exhaustive over the
   emitter's `emit-node` dispatch (locked by a drift-guard test) so no op slips
   through validation as an unregistered open-extension node."
  (:require [clojure.set :as set]
            [clojure-elisp.schema :as schema]
            [malli.core :as m]
            [malli.error :as me]
            [malli.generator :as mg]))

;; ============================================================================
;; Required keys per :op
;; ============================================================================

(def op->required
  "Map of :op -> required keys for each AST node type.
   Every AST node implicitly carries :op and :env.

   Kept exhaustive over `clojure-elisp.emitter/emit-node`'s dispatch keys — the
   final block below registers the emitter-only ops (verified required-key sets;
   the three surface-unreachable ops :function-quote/:pcase/:defvar-elisp are
   presence-only). The drift-guard test enforces this invariant."
  {:const               #{:val :type}
   :local               #{:name}
   :var                 #{:name}
   :def                 #{:name}
   ;; :body is required for single-arity defns but ABSENT on multi-arity nodes
   ;; (which carry :multi-arity? + :arities instead), so only :name is required.
   :defn                #{:name}
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
   :ns                  #{:name :requires :load-paths}
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
   :unknown             #{:form}
   ;; -- emitter-only ops (verified against the analyzer 2026-07-11) -----------
   :setq                #{:pairs}
   :setf                #{:pairs}
   :push                #{:place :value}
   :when-let            #{:body :val :var}
   :when-let*           #{:bindings :body}
   :if-let              #{:then :val :var}       ; :else optional
   :if-let*             #{:bindings :then}       ; :else optional
   :condition-case      #{:body :handlers :var}
   :unwind-protect      #{:body :cleanup}
   :while               #{:body :test}
   :binding             #{:bindings :body}
   :assert              #{:test}
   :dolist              #{:body :list-form :var}
   :unless              #{:body :test}
   :cl-defun            #{:arglist :body :name}
   :cl-defstruct        #{:name-or-opts :slots}
   :function-quote      #{}                       ; surface-unreachable — presence only
   :pcase               #{}                       ; surface-unreachable — presence only
   :defvar-elisp        #{}                       ; surface-unreachable — presence only
   :comment             #{}})

;; Backward-compatible alias.
(def ast-schemas op->required)

;; ============================================================================
;; Malli schema registry
;; ============================================================================

(def ^:private key->value-schema
  "Value schema for SCALAR keys whose shape the analyzer reliably produces
   across every :op that requires them. Node-valued child keys are typed
   per-op via `clojure-elisp.schema/op->child-schema` (which takes precedence);
   keys absent from both maps default to :any (presence-only). Keys whose shape
   varies by :op (e.g. :type is :keyword for :const but a symbol for
   :extend-type) MUST stay out of this map."
  {:op          :keyword
   :name        :symbol
   ;; collection-valued keys — always vectors where required, but see
   ;; schema/op->child-schema for the ops whose elements are typed nodes
   :body        [:vector :any]
   :args        [:vector :any]
   :exprs       [:vector :any]
   :items       [:vector :any]
   :params      [:vector :any]
   :bindings    [:vector :any]
   :clauses     [:vector :any]
   :fns         [:vector :any]
   :fields      [:vector :any]
   :protocols   [:vector :any]
   :methods     [:vector :any]
   :extensions  [:vector :any]
   :closed-over [:vector :any]
   :requires    [:vector :any]
   :load-paths  [:vector :any]
   :keys        [:vector :any]
   :vals        [:vector :any]})

(defn- child-schema
  "Resolve the Malli schema for key `k` of an :op node. Per-op nested-node
   typing (recursive, from the shared registry) wins over the global scalar
   shape, which wins over :any (presence-only)."
  [op k]
  (or (get-in schema/op->child-schema [op k])
      (get key->value-schema k)
      :any))

(defn- ->map-schema
  "Build an open Malli :map schema for an :op from its required-key set. Required
   keys are typed via `child-schema`; the op's remaining nested-node child keys
   (present but not required) are added as optional entries so they are validated
   when present. Compiled against the shared registry so [:ref …] resolves."
  [op required]
  (let [child-keys (set (keys (get schema/op->child-schema op)))
        req-keys   (sort required)
        opt-keys   (sort (set/difference child-keys required))]
    (schema/schema
     (into [:map [:op (child-schema op :op)]]
           (concat (map (fn [k] [k (child-schema op k)]) req-keys)
                   (map (fn [k] [k {:optional true} (child-schema op k)]) opt-keys))))))

(def node-schemas
  "Map of :op -> compiled Malli :map schema (required keys + recursive child
   typing). Compiled against `clojure-elisp.schema/registry`."
  (into {} (map (fn [[op required]] [op (->map-schema op required)])) op->required))

(defn node-schema
  "Return the Malli schema for an :op, or nil for unregistered ops."
  [op]
  (get node-schemas op))

;; ============================================================================
;; Validation
;; ============================================================================

(defn valid-ast-node?
  "True when node has a registered :op and satisfies its schema (recursively —
   nested child nodes are validated too). Unregistered ops are treated as valid
   (open extension)."
  [node]
  (if-let [schema (node-schema (:op node))]
    (m/validate schema node)
    true))

(defn explain-ast-node
  "Return a humanized explanation of why node is invalid, or nil when valid
   or unregistered."
  [node]
  (when-let [schema (node-schema (:op node))]
    (when-not (m/validate schema node)
      (me/humanize (m/explain schema node)))))

(defn validate-ast-node
  "Validate an AST node against its :op schema.
   Returns the node if valid, throws ex-info otherwise.
   Nodes with unregistered :op values pass through (open extension).

   Driven entirely by the Malli schema — required-key presence and
   value-shape (including recursive child structure) are one check. Missing
   keys are surfaced with a distinct message (and :missing ex-data) by reading
   them back out of the humanized explanation."
  [node]
  (if-let [schema (node-schema (:op node))]
    (if (m/validate schema node)
      node
      (let [errors  (me/humanize (m/explain schema node))
            missing (keys (into {} (filter (comp #{["missing required key"]} val)) errors))]
        (if (seq missing)
          (throw (ex-info (str "AST node :" (:op node) " missing keys: " (vec missing))
                          {:op (:op node) :missing (vec missing) :node node}))
          (throw (ex-info (str "AST node :" (:op node) " failed schema: " (pr-str errors))
                          {:op (:op node) :errors errors :node node})))))
    node))

(defn valid-ast-tree?
  "True when `node` and every nested node beneath it conform to the shared
   recursive ::schema/node. A single-call whole-tree check (complements the
   per-node `valid-ast-node?`)."
  [node]
  (schema/validate ::schema/node node))

;; ============================================================================
;; Generators (leaf nodes)
;; ============================================================================

(def leaf-node-schemas
  "Self-contained node schemas suitable for generation (no child AST nodes)."
  {:const [:map [:op [:= :const]] [:val [:maybe [:or :int :double :string :boolean :keyword]]] [:type :keyword]]
   :local [:map [:op [:= :local]] [:name :symbol]]
   :var   [:map [:op [:= :var]] [:name :symbol]]})

(defn gen-node
  "Return a test.check generator for leaf AST nodes of the given :op.
   Supported ops: keys of leaf-node-schemas."
  [op]
  (mg/generator (get leaf-node-schemas op)))
