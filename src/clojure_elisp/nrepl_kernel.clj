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

(defn result->responses
  "Shape a compile Result into the responses to send.
   Compiled Elisp travels as :cljel-compiled-elisp rather than :value, which
   CIDER's built-in display handler would try to render as a Clojure value."
  [result]
  (if (r/ok? result)
    [{:cljel-compiled-elisp (:ok result) :ns "user"} (done)]
    [{:err (str "Compilation error: " (:message result))} (done)]))

(defn request-context
  "The compilation context a request carries, or nil."
  [{:keys [cljel-context cljel-ns]}]
  (first (remove str/blank? [cljel-context cljel-ns])))

;; ============================================================================
;; Pipeline — op semantics
;; ============================================================================

(defn handle-eval
  "Responses for an eval op, compiled against the request's context."
  [{:keys [code] :as request}]
  (result->responses
   (if-let [context (request-context request)]
     (compile-in-context code context)
     (compile-result code))))

(defn handle-load-file
  "Responses for a load-file op: the whole file, with its ns context."
  [{:keys [file]}]
  (result->responses (compile-result file :file)))

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
(m/=> result->responses [:=> [:cat errors/string-result-schema] responses-schema])
(m/=> request-context   [:=> [:cat eval-request-schema] [:maybe :string]])
(m/=> handle-eval       [:=> [:cat eval-request-schema] responses-schema])
(m/=> handle-op         [:=> [:cat [:map [:op {:optional true} [:maybe :string]]]]
                         [:maybe responses-schema]])
