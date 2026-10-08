(ns clojure-elisp.nrepl-kernel
  "Transport-independent core of the ClojureElisp nREPL service.

   Strata:
     Collect   session registry reads
     Promote   pure compilation and response shaping over values
     Pipeline  op dispatch, composing the above
     Boundary  none here; the transports own I/O

   handle-op returns a vector of partial response maps, or nil when the op is
   not a CLJEL concern. The transport merges its own correlation keys.

   Loads on any host that can load the compiler: the JVM behind
   clojure-elisp.nrepl, Babashka behind clel.nrepl-server."
  (:require [clojure.string :as str]
            [clojure-elisp.compile :as cc]
            [clojure-elisp.errors :as errors]
            [hive-dsl.result :as r]
            [malli.core :as m]))

;; ============================================================================
;; Value Objects
;; ============================================================================

(def compile-mode-schema
  "How a request's code is compiled."
  [:enum :expr :ns :file])

(def eval-request-schema
  "An eval op's compilable payload. :cljel-context is the buffer the form came
   from; :cljel-ns is the older, narrower spelling carrying only its ns form."
  [:map
   [:code :string]
   [:cljel-context {:optional true} [:maybe :string]]
   [:cljel-ns {:optional true} [:maybe :string]]])

(def response-schema
  "One partial nREPL response, before the transport adds correlation keys."
  [:map-of :keyword :any])

(def responses-schema
  [:sequential response-schema])

;; ============================================================================
;; Collect — session registry
;; ============================================================================

(def cljel-sessions
  "Set of nREPL session IDs that have CLJEL compilation active."
  (atom #{}))

(defn cljel-active?
  "True when session-id has CLJEL compilation active."
  [session-id]
  (contains? @cljel-sessions session-id))

(defn activate!
  "Mark session-id as compiling ClojureElisp."
  [session-id]
  (swap! cljel-sessions conj session-id))

(defn deactivate!
  "Return session-id to ordinary Clojure evaluation."
  [session-id]
  (swap! cljel-sessions disj session-id))

;; ============================================================================
;; Promote — compilation
;; ============================================================================

(defn compile-result
  "Compile code in mode, returning a Result.
   :expr compiles standalone, :ns against context, :file as a whole buffer."
  ([code] (compile-result code :expr nil))
  ([code mode] (compile-result code mode nil))
  ([code mode context]
   (case mode
     :file (cc/compile-file-string-result code)
     :ns   (cc/compile-string-in-ns-result context code)
     (cc/compile-string-in-ns-result nil code))))

(defn- fallback-contexts
  "The contexts to try, widest first: the buffer, its leading ns form, nothing."
  [context]
  (into [] (distinct) [context (cc/leading-ns-source context) nil]))

(defn compile-in-context
  "Compile code against context, degrading to narrower contexts on failure.
   Reports the widest context's error when every attempt fails."
  [code context]
  (let [attempts (map #(compile-result code :ns %) (fallback-contexts context))]
    (or (first (filter r/ok? attempts))
        (first attempts))))

;; ============================================================================
;; Promote — response shaping
;; ============================================================================

(defn- done [] {:status ["done"]})

(def default-ns
  "The namespace reported when the source names none."
  "user")

(defn source-ns-name
  "The name of the leading (ns ...) form in source, or default-ns.
   Never throws: unreadable or ns-less source reports default-ns."
  [source]
  (or (when-let [ns-src (cc/leading-ns-source source)]
        (try
          (some-> (cc/extract-ns-name ns-src) str)
          (catch Exception _ nil)))
      default-ns))

(defn result->responses
  "Shape a compile Result into the responses to send.
   Compiled Elisp travels as :cljel-compiled-elisp rather than :value, which
   CIDER's built-in display handler would try to render as a Clojure value.
   ns-name is the namespace the code was compiled in (default-ns when absent).
   With mirror-value? true the Elisp is also sent as :value, for plain nREPL
   clients that read only the standard keys."
  ([result] (result->responses result default-ns false))
  ([result ns-name] (result->responses result ns-name false))
  ([result ns-name mirror-value?]
   (if (r/ok? result)
     [(cond-> {:cljel-compiled-elisp (:ok result) :ns (or ns-name default-ns)}
        mirror-value? (assoc :value (:ok result)))
      (done)]
     [{:err (str "Compilation error: " (:message result))} (done)])))

(defn request-context
  "The compilation context a request carries, or nil."
  [{:keys [cljel-context cljel-ns]}]
  (first (remove str/blank? [cljel-context cljel-ns])))

(defn mirror-value?
  "True when the request opts into the :value mirror with cljel-mirror-value."
  [{:keys [cljel-mirror-value]}]
  (contains? #{true "true" "1" 1} cljel-mirror-value))

;; ============================================================================
;; Pipeline — op semantics
;; ============================================================================

(defn handle-eval
  "Responses for an eval op, compiled against the request's context.
   :ns names the context's namespace, else default-ns."
  [{:keys [code] :as request}]
  (let [context (request-context request)]
    (result->responses
     (if context
       (compile-in-context code context)
       (compile-result code))
     (source-ns-name context)
     (mirror-value? request))))

(defn handle-load-file
  "Responses for a load-file op: the whole file, with its ns context.
   :ns names the file's namespace, else default-ns."
  [{:keys [file] :as request}]
  (result->responses (compile-result file :file)
                     (source-ns-name file)
                     (mirror-value? request)))

(defn handle-op
  "Dispatch one nREPL message.
   Returns the responses to send, or nil when the op is not ours."
  [{:keys [op session] :as msg}]
  (case op
    "cljel-start" (do (activate! session)
                      [{:value "ClojureElisp session started" :status ["done"]}])
    "cljel-stop"  (do (deactivate! session)
                      [{:value "ClojureElisp session stopped" :status ["done"]}])
    "eval"        (when (cljel-active? session) (handle-eval msg))
    "load-file"   (when (cljel-active? session) (handle-load-file msg))
    nil))

;; ============================================================================
;; Compatibility
;; ============================================================================

(defn compile-code
  "Compile code, returning {:status :ok :elisp s} or {:status :error :error s}.
   The pre-Result shape, kept for callers of clojure-elisp.nrepl/compile-code."
  ([code] (compile-code code :expr nil))
  ([code mode] (compile-code code mode nil))
  ([code mode context]
   (let [result (compile-result code mode context)]
     (if (r/ok? result)
       {:status :ok :elisp (:ok result)}
       {:status :error :error (:message result)}))))

;; ============================================================================
;; Function Contracts (Malli)
;; ============================================================================

(m/=> cljel-active?     [:=> [:cat [:maybe :string]] :boolean])
(m/=> compile-result    [:function
                         [:=> [:cat :string] errors/string-result-schema]
                         [:=> [:cat :string compile-mode-schema]
                          errors/string-result-schema]
                         [:=> [:cat :string compile-mode-schema [:maybe :string]]
                          errors/string-result-schema]])
(m/=> compile-in-context
      [:=> [:cat :string [:maybe :string]] errors/string-result-schema])
(m/=> source-ns-name    [:=> [:cat [:maybe :string]] :string])
(m/=> result->responses [:function
                         [:=> [:cat errors/string-result-schema] responses-schema]
                         [:=> [:cat errors/string-result-schema [:maybe :string]]
                          responses-schema]
                         [:=> [:cat errors/string-result-schema [:maybe :string] :boolean]
                          responses-schema]])
(m/=> request-context   [:=> [:cat eval-request-schema] [:maybe :string]])
(m/=> handle-eval       [:=> [:cat eval-request-schema] responses-schema])
(m/=> handle-op         [:=> [:cat [:map [:op {:optional true} [:maybe :string]]]]
                         [:maybe responses-schema]])
