(ns clojure-elisp.errors
  "Compiler error vocabulary: the single home for the compile-error taxonomy.

   Owns the categorized error tags, the hive-dsl ADT sum type (for exhaustive
   pattern matching), and the Malli schemas describing compile Results. Every
   other layer depends on this namespace for the error language."
  (:require [hive-dsl.adt :as adt]
            [malli.core :as m]))

(def compile-error-tags
  "The categorized compile-error tags. Single source of the error vocabulary."
  [:compile/read-error
   :compile/analysis-error
   :compile/emit-error
   :compile/file-error])

(adt/defadt CompileError
  "Categorized compiler errors with structured data."
  [:compile/read-error     {:message string?}]
  [:compile/analysis-error {:message string? :form any?}]
  [:compile/emit-error     {:message string? :op keyword?}]
  [:compile/file-error     {:message string? :path string?}])

;; ============================================================================
;; Result Schemas (Malli)
;; ============================================================================

(def error-tag-schema
  "Malli enum of the compile-error tags."
  (into [:enum] compile-error-tags))

(def error-result-schema
  "Failed compile Result: {:error tag :message string (:class string)}.
   hive-dsl `try-effect*` assocs :class (the stringified exception class)
   alongside the tag and message, so it is modeled here (optional)."
  [:map
   [:error error-tag-schema]
   ;; :message comes from (.getMessage e), which is nil for a messageless
   ;; exception — so it must tolerate nil, or a valid error Result would be
   ;; rejected by the return contract under instrumentation.
   [:message [:maybe :string]]
   [:class {:optional true} :string]])

(defn- result-schema
  "A compile Result parameterized by its success payload. Uses :multi dispatch
   on the presence of :ok vs :error so a malformed value is explained against
   the ONE branch it structurally belongs to (unlike :or, which reports both)."
  [ok-payload-schema]
  [:multi {:dispatch (fn [x] (if (and (map? x) (contains? x :ok)) :ok :error))}
   [:ok    [:map [:ok ok-payload-schema]]]
   [:error error-result-schema]])

(def string-result-schema
  "Result whose success payload is elisp source: {:ok string} | error."
  (result-schema :string))

(def file-artifact-schema
  "Written-file summary payload."
  [:map [:input :string] [:output :string] [:size :int]])

(def file-result-schema
  "Result whose success payload is a written-file summary: {:ok artifact} | error."
  (result-schema file-artifact-schema))

(defn valid-string-result?
  "True when x conforms to string-result-schema."
  [x]
  (m/validate string-result-schema x))

(defn valid-file-result?
  "True when x conforms to file-result-schema."
  [x]
  (m/validate file-result-schema x))
