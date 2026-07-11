(ns clojure-elisp.schema
  "Shared Malli registry — the single schema home for clojure-elisp.

   Every namespace that needs a cross-cutting schema references THIS registry
   rather than inventing local `[:ref …]` targets. It hosts:

     ::node         the recursive AST node (open [:multi {:dispatch :op}])
     ::env          the analyzer environment map
     ::require-spec an ns-form require clause
     ::error-result the failed-Result shape (hive-dsl try-effect*)
     sub-map refs   ::name-init ::var-val ::setq-pair ::setf-pair
                    ::cond-clause ::case-clause ::handler ::catch ::fn-spec
                    ::iter-clause — the binding/clause records the analyzer
                    produces inside AST nodes (these are NOT themselves nodes).

   The `op->child-schema` table (which AST child keys hold nested nodes) is
   EMPIRICALLY DERIVED from the analyzer and verified to (a) accept every node
   the analyzer produces across a broad form corpus and (b) reject mis-shaped
   children. Regenerate it from the analyzer if node shapes change; do not
   hand-edit entries.

   Use `schema`/`validate`/`explain`/`humanize` (registry pre-bound) or
   `registry` directly for `{:registry …}` options."
  (:require [malli.core :as m]
            [malli.error :as me]
            [malli.registry :as mr]
            [malli.util :as mu]))

;; ============================================================================
;; Sub-map schemas — analyzer records nested inside AST nodes (no :op/:env)
;; ============================================================================

(def sub-schemas
  "Binding/clause record schemas referenced from `op->child-schema`. Only the
   node-valued positions are typed; names/keywords/raw forms stay :any."
  {::name-init   [:map [:name :any] [:init [:ref ::node]]]                          ; let/loop/binding bindings
   ::var-val     [:map [:var :any] [:val [:ref ::node]]]                            ; when-let*/if-let* bindings
   ::setq-pair   [:map [:name :any] [:value [:ref ::node]]]                         ; setq pairs
   ::setf-pair   [:map [:place [:ref ::node]] [:value [:ref ::node]]]               ; setf pairs
   ::cond-clause [:map [:test [:ref ::node]] [:expr [:ref ::node]]]                 ; cond clause
   ::case-clause [:map [:test :any] [:expr [:ref ::node]]]                          ; case clause (:test is a raw constant)
   ::handler     [:map [:condition :any] [:body [:vector [:ref ::node]]]]           ; condition-case handler
   ::catch       [:map [:type :any] [:name :any] [:body [:vector [:ref ::node]]]]   ; try catch
   ::fn-spec     [:map [:name :any] [:params [:vector :any]] [:body [:vector [:ref ::node]]]] ; letfn fn
   ::iter-clause [:map [:type :keyword]]})                                          ; doseq/for iteration clause (kept open)

;; ============================================================================
;; Per-op child-node typing — DERIVED from the analyzer, do not hand-edit
;; ============================================================================

(def op->child-schema
  "Map of :op -> {child-key -> Malli schema} for the keys the analyzer fills
   with nested AST nodes. `[:ref ::node]` = single node; `[:vector [:ref
   ::node]]` = node vector; `[:maybe …]` = optional/nilable single node; a
   sub-map ref (::name-init …) for binding/clause records. Keys absent here are
   raw (symbols, keywords, literals) and stay untyped.

   Two verified caveats live in this table: :condition-case/:unwind-protect
   :body is a SINGLE node (not a vector), and optional child keys
   (:if/:if-let/:if-let* :else, :case :default, :def :init, :try :finally) are
   [:maybe …] because the analyzer leaves them present-but-nil when the source
   omits them."
  {:and {:exprs [:vector [:ref ::node]]}
   :assert {:test [:ref ::node]}
   :binding {:bindings [:vector [:ref ::name-init]]
             :body [:vector [:ref ::node]]}
   :case {:clauses [:vector [:ref ::case-clause]]
          :default [:maybe [:ref ::node]]
          :expr [:ref ::node]}
   :cl-defun {:body [:vector [:ref ::node]]}
   :cond {:clauses [:vector [:ref ::cond-clause]]}
   :condition-case {:body [:ref ::node]
                    :handlers [:vector [:ref ::handler]]}
   :def {:init [:maybe [:ref ::node]]}
   :defmacro {:body [:vector [:ref ::node]]}
   :defmethod {:body [:vector [:ref ::node]]}
   :defmulti {:dispatch-fn [:ref ::node]}
   :defn {:body [:vector [:ref ::node]]}
   :do {:body [:vector [:ref ::node]]}
   :dolist {:body [:vector [:ref ::node]]
            :list-form [:ref ::node]}
   :doseq {:body [:vector [:ref ::node]]
           :clauses [:vector [:ref ::iter-clause]]}
   :dotimes {:body [:vector [:ref ::node]]
             :count [:ref ::node]}
   :fn {:body [:vector [:ref ::node]]}
   :for {:body [:vector [:ref ::node]]
         :clauses [:vector [:ref ::iter-clause]]}
   :if {:else [:maybe [:ref ::node]]
        :test [:ref ::node]
        :then [:ref ::node]}
   :if-let {:else [:maybe [:ref ::node]]
            :then [:ref ::node]
            :val [:ref ::node]}
   :if-let* {:bindings [:vector [:ref ::var-val]]
             :else [:maybe [:ref ::node]]
             :then [:ref ::node]}
   :invoke {:args [:vector [:ref ::node]]
            :fn [:ref ::node]}
   :let {:bindings [:vector [:ref ::name-init]]
         :body [:vector [:ref ::node]]}
   :letfn {:body [:vector [:ref ::node]]
           :fns [:vector [:ref ::fn-spec]]}
   :loop {:bindings [:vector [:ref ::name-init]]
          :body [:vector [:ref ::node]]}
   :map {:keys [:vector [:ref ::node]]
         :vals [:vector [:ref ::node]]}
   :or {:exprs [:vector [:ref ::node]]}
   :push {:place [:ref ::node]
          :value [:ref ::node]}
   :recur {:args [:vector [:ref ::node]]}
   :save-excursion {:body [:vector [:ref ::node]]}
   :save-restriction {:body [:vector [:ref ::node]]}
   :set {:items [:vector [:ref ::node]]}
   :set! {:value [:ref ::node]}
   :setf {:pairs [:vector [:ref ::setf-pair]]}
   :setq {:pairs [:vector [:ref ::setq-pair]]}
   :throw {:exception [:ref ::node]}
   :try {:body [:vector [:ref ::node]]
         :catches [:vector [:ref ::catch]]
         ;; :finally is present-but-nil when a try has no finally clause
         :finally [:maybe [:vector [:ref ::node]]]}
   :unless {:body [:vector [:ref ::node]]
            :test [:ref ::node]}
   :unwind-protect {:body [:ref ::node]
                    :cleanup [:vector [:ref ::node]]}
   :vector {:items [:vector [:ref ::node]]}
   :when {:body [:vector [:ref ::node]]
          :test [:ref ::node]}
   :when-let {:body [:vector [:ref ::node]]
              :val [:ref ::node]}
   :when-let* {:bindings [:vector [:ref ::var-val]]
               :body [:vector [:ref ::node]]}
   :while {:body [:vector [:ref ::node]]
           :test [:ref ::node]}
   :with-current-buffer {:body [:vector [:ref ::node]]
                         :buffer [:ref ::node]}
   :with-eval-after-load {:body [:vector [:ref ::node]]
                          :feature [:ref ::node]}
   :with-temp-buffer {:body [:vector [:ref ::node]]}})

;; ============================================================================
;; Analyzer environment + ns-form require spec
;; ============================================================================

(def env-schema
  "The environment map threaded through analysis. Open — analysis stages assoc
   extra keys (e.g. :defs during the pre-scan)."
  [:map {:closed false}
   [:ns :symbol]
   [:locals [:set :symbol]]
   [:aliases [:map-of :symbol :symbol]]
   [:refers [:map-of :symbol :symbol]]
   [:defs [:map-of :symbol [:map [:private? :boolean]]]]
   [:in-tail-position? :boolean]])

(def require-spec-schema
  "A single normalized require clause from an ns form."
  [:map
   [:ns :symbol]
   [:as {:optional true} [:maybe :symbol]]
   [:refer {:optional true} [:maybe [:or [:= :all] [:vector :symbol]]]]])

;; ============================================================================
;; Result algebra
;; ============================================================================

(def error-result-schema
  "Failed compile Result. hive-dsl `try-effect*` always assocs :class (the
   stringified exception class) alongside the tag + message."
  [:map
   [:error :keyword]
   ;; nil for a messageless exception — see errors/error-result-schema.
   [:message [:maybe :string]]
   [:class {:optional true} :string]])

;; ============================================================================
;; Recursive AST node schema
;; ============================================================================

(defn- op-branch
  "Build the ::node multi branch for one :op: an open :map that types only the
   verified nested-node child positions (all optional — required-key
   enforcement lives in clojure-elisp.ast/op->required)."
  [op child-entries]
  (into [:map [:op [:= op]]]
        (map (fn [[k s]] [k {:optional true} s]))
        (sort-by key child-entries)))

(def node-schema
  "Open recursive AST node schema: [:multi {:dispatch :op}] whose modeled
   branches type their nested-node children via [:ref ::node]. Unmodeled ops
   fall through [::m/default :map], preserving open-extension semantics."
  (into [:multi {:dispatch :op}]
        (conj (mapv (fn [[op ce]] [op (op-branch op ce)]) op->child-schema)
              [::m/default :map])))

;; ============================================================================
;; Registry + helpers
;; ============================================================================

(def registry
  "Composite registry: Malli defaults + malli.util + this ns's shared schemas."
  (mr/composite-registry
   (m/default-schemas)
   (mu/schemas)
   (assoc sub-schemas
          ::node         node-schema
          ::env          env-schema
          ::require-spec require-spec-schema
          ::error-result error-result-schema)))

(defn schema
  "Compile `?schema` against the shared registry so its [:ref …] targets resolve."
  [?schema]
  (m/schema ?schema {:registry registry}))

(defn validate
  "Validate `value` against `?schema` compiled in the shared registry."
  [?schema value]
  (m/validate (schema ?schema) value))

(defn explain
  "Explain `value` against `?schema` compiled in the shared registry."
  [?schema value]
  (m/explain (schema ?schema) value))

(defn humanize
  "Humanized explanation of why `value` fails `?schema`, or nil when valid."
  [?schema value]
  (some-> (explain ?schema value) me/humanize))

(defn result-schema
  "Result algebra parameterized by success payload: {:ok payload} | error."
  [ok-schema]
  [:or [:map [:ok ok-schema]] error-result-schema])
