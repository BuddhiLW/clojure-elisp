(ns clojure-elisp.nrepl-kernel
  "Transport-independent core of the ClojureElisp nREPL service.

   Owns the session registry, the compile modes, and the op semantics. Knows
   nothing about how a message arrives or how a response is written, so it
   loads on any host that can load the compiler: the JVM (behind
   clojure-elisp.nrepl's middleware) and Babashka (behind
   clojure-elisp.nrepl-server's socket loop).

   handle-op returns a VECTOR of partial response maps, in send order, or nil
   when the op is not ours. The transport merges its own correlation keys
   (:id, :session) into each one."
  (:require [clojure.string :as str]
            [clojure-elisp.core :as core]))

;; ============================================================================
;; Session Tracking
;; ============================================================================

(def cljel-sessions
  "Set of nREPL session IDs that have CLJEL compilation active."
  (atom #{}))

(defn cljel-active?
  "Check if the given session ID has CLJEL compilation active."
  [session-id]
  (contains? @cljel-sessions session-id))

(defn activate!
  "Mark SESSION-ID as compiling ClojureElisp."
  [session-id]
  (swap! cljel-sessions conj session-id))

(defn deactivate!
  "Return SESSION-ID to ordinary Clojure evaluation."
  [session-id]
  (swap! cljel-sessions disj session-id))

;; ============================================================================
;; Compilation
;; ============================================================================

(defn compile-code
  "Compile a ClojureElisp code string to Elisp.

   MODE is one of:
     :expr  single expression, no namespace context (default)
     :ns    forms compiled in the namespace context of NS-SOURCE, the text of
            an (ns ...) form. Definitions get the same namespace prefix :file
            would give them; no file header or (provide ...) is emitted
     :file  whole buffer. (ns ...) aliases/refers apply, definitions get their
            namespace prefix, and a trailing (provide ...) is appended

   Returns {:status :ok :elisp \"...\"} or {:status :error :error \"...\"}."
  ([code] (compile-code code :expr nil))
  ([code mode] (compile-code code mode nil))
  ([code mode ns-source]
   (try
     {:status :ok
      :elisp (case mode
               :file (core/compile-file-string code)
               :ns   (core/compile-string-in-ns ns-source code)
               (core/compile-string code))}
     (catch Exception e
       {:status :error
        :error (.getMessage e)}))))

;; ============================================================================
;; Responses
;; ============================================================================

(defn- compiled-responses
  "Responses carrying compiled Elisp.
   Uses :cljel-compiled-elisp rather than :value so CIDER's built-in display
   handler does not try to render an Elisp source string as a Clojure value."
  [elisp]
  [{:cljel-compiled-elisp elisp :ns "user"}
   {:status ["done"]}])

(defn- error-responses
  [error-msg]
  [{:err (str "Compilation error: " error-msg)}
   {:status ["done"]}])

(defn- result-responses
  [{:keys [status elisp error]}]
  (if (= :ok status)
    (compiled-responses elisp)
    (error-responses error)))

;; ============================================================================
;; Op Semantics
;; ============================================================================

(defn handle-eval
  "Compile an eval op's code and return the responses to send.
   When the client supplies :cljel-ns (the source text of the buffer's
   (ns ...) form), the code is compiled in that namespace context, so an
   interactively evaluated defn installs the SAME Elisp name that compiling
   the whole buffer would install."
  [{:keys [code cljel-ns]}]
  (result-responses
   (if (str/blank? cljel-ns)
     (compile-code code)
     (compile-code code :ns cljel-ns))))

(defn handle-load-file
  "Compile a load-file op's whole file content and return the responses."
  [{:keys [file]}]
  (result-responses (compile-code file :file)))

(defn handle-op
  "Dispatch one nREPL message.
   Returns a vector of partial response maps, or nil when OP is not a CLJEL
   concern and the host should handle it normally."
  [{:keys [op session] :as msg}]
  (case op
    "cljel-start" (do (activate! session)
                      [{:value "ClojureElisp session started"
                        :status ["done"]}])
    "cljel-stop"  (do (deactivate! session)
                      [{:value "ClojureElisp session stopped"
                        :status ["done"]}])
    "eval"        (when (cljel-active? session) (handle-eval msg))
    "load-file"   (when (cljel-active? session) (handle-load-file msg))
    nil))
