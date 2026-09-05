(ns clojure-elisp.nrepl
  "nREPL middleware for ClojureElisp.

   Intercepts eval and load-file ops in CLJEL sessions, compiles
   ClojureElisp forms to Elisp, and returns the compiled Elisp for
   client-side evaluation in Emacs.

   Architecture:
     CIDER sends form via nREPL
       -> middleware compiles ClojureElisp to an Elisp string
       -> returns to CIDER
       -> CIDER extension evals Elisp locally in Emacs
       -> displays result

   Custom ops:
     cljel-start  activate CLJEL compilation for the session
     cljel-stop   deactivate CLJEL compilation

   Intercepted ops (when CLJEL active):
     eval      compiles code to Elisp instead of evaluating as Clojure
     load-file compiles file content to Elisp

   This namespace is the JVM TRANSPORT only. Session state, compile modes and
   op semantics live in clojure-elisp.nrepl-kernel, which the standalone
   Babashka server shares; see clojure-elisp.nrepl-server."
  (:require [clojure-elisp.nrepl-kernel :as kernel]
            [nrepl.middleware :refer [set-descriptor!]]
            [nrepl.misc :refer [response-for]]
            [nrepl.transport :as t]))

;; ============================================================================
;; Re-exports (kernel is the single source; these keep the public surface)
;; ============================================================================

(def cljel-sessions kernel/cljel-sessions)

(defn cljel-active?
  "Check if the given session ID has CLJEL compilation active."
  [session-id]
  (kernel/cljel-active? session-id))

(defn compile-code
  "Compile a ClojureElisp code string to Elisp. See nrepl-kernel/compile-code."
  ([code] (kernel/compile-code code))
  ([code mode] (kernel/compile-code code mode))
  ([code mode ns-source] (kernel/compile-code code mode ns-source)))

;; ============================================================================
;; Transport
;; ============================================================================

(defn- send-responses!
  "Write each kernel response back over the nREPL transport."
  [{:keys [transport] :as msg} responses]
  (doseq [response responses]
    (t/send transport (response-for msg response))))

;; ============================================================================
;; Op Handlers (transport-bound wrappers over the kernel)
;; ============================================================================

(defn handle-eval
  "Handle an eval op for a CLJEL session.
   Compiles the code and sends the compiled Elisp back. When the client
   supplies :cljel-ns, the code compiles in that namespace context."
  [msg]
  (send-responses! msg (kernel/handle-eval msg)))

(defn handle-load-file
  "Handle a load-file op for a CLJEL session.
   Compiles the whole file content in :file mode and sends the Elisp back."
  [msg]
  (send-responses! msg (kernel/handle-load-file msg)))

(defn handle-start
  "Activate CLJEL compilation for the session."
  [{:keys [session] :as msg}]
  (kernel/activate! session)
  (send-responses! msg [{:value "ClojureElisp session started"
                         :status ["done"]}]))

(defn handle-stop
  "Deactivate CLJEL compilation for the session."
  [{:keys [session] :as msg}]
  (kernel/deactivate! session)
  (send-responses! msg [{:value "ClojureElisp session stopped"
                         :status ["done"]}]))

;; ============================================================================
;; Middleware
;; ============================================================================

(defn wrap-cljel
  "nREPL middleware for ClojureElisp compilation.

   Handles custom ops:
   - cljel-start: Activate CLJEL compilation for the session
   - cljel-stop:  Deactivate CLJEL compilation

   When CLJEL is active, intercepts:
   - eval:      Compiles code to Elisp instead of evaluating as Clojure
   - load-file: Compiles file content to Elisp"
  [handler]
  (fn [msg]
    (if-let [responses (kernel/handle-op msg)]
      (send-responses! msg responses)
      (handler msg))))

(set-descriptor! #'wrap-cljel
                 {:requires #{"clone" "session"}
                  :expects #{"eval" "load-file"}
                  :handles {"cljel-start"
                            {:doc "Activate ClojureElisp compilation for this session.
                    Subsequent eval and load-file ops compile to Elisp
                    instead of evaluating as Clojure."
                             :returns {"value" "Confirmation message"
                                       "status" "done"}}
                            "cljel-stop"
                            {:doc "Deactivate ClojureElisp compilation for this session.
                    Eval and load-file ops return to normal Clojure behavior."
                             :returns {"value" "Confirmation message"
                                       "status" "done"}}}})
