(ns clojure-elisp.nrepl
  "nREPL middleware for ClojureElisp.

   Intercepts eval and load-file ops in CLJEL sessions, compiles
   ClojureElisp forms to Elisp, and returns the compiled Elisp for
   client-side evaluation in Emacs.

   Architecture:
     CIDER sends form via nREPL
       → middleware compiles ClojureElisp → Elisp string
       → returns to CIDER
       → CIDER extension evals Elisp locally in Emacs
       → displays result

   Custom ops:
     cljel-start  — activate CLJEL compilation for the session
     cljel-stop   — deactivate CLJEL compilation

   Intercepted ops (when CLJEL active):
     eval      — compiles code to Elisp instead of evaluating as Clojure
     load-file — compiles file content to Elisp"
  (:require [clojure-elisp.core :as core]
            [nrepl.middleware :refer [set-descriptor!]]
            [nrepl.misc :refer [response-for]]
            [nrepl.transport :as t]))

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

;; ============================================================================
;; Compilation
;; ============================================================================

(defn compile-code
  "Compile a ClojureElisp code string to Elisp.
   Returns {:status :ok :elisp \"...\"} or {:status :error :error \"...\"}."
  [code]
  (try
    {:status :ok
     :elisp (core/compile-string code)}
    (catch Exception e
      {:status :error
       :error (.getMessage e)})))

;; ============================================================================
;; Response Helpers
;; ============================================================================

(defn- send-compiled-result
  "Send a successful compilation result via transport."
  [{:keys [transport] :as msg} elisp]
  (t/send transport (response-for msg {:value elisp
                                       :cljel-compiled-elisp elisp
                                       :ns "user"}))
  (t/send transport (response-for msg {:status :done})))

(defn- send-error
  "Send a compilation error via transport."
  [{:keys [transport] :as msg} error-msg]
  (t/send transport (response-for msg {:err (str "Compilation error: " error-msg)}))
  (t/send transport (response-for msg {:status :done})))

;; ============================================================================
;; Op Handlers
;; ============================================================================

(defn handle-eval
  "Handle an eval op for a CLJEL session.
   Compiles the code and sends the compiled Elisp back."
  [{:keys [code] :as msg}]
  (let [result (compile-code code)]
    (if (= :ok (:status result))
      (send-compiled-result msg (:elisp result))
      (send-error msg (:error result)))))

(defn handle-load-file
  "Handle a load-file op for a CLJEL session.
   Compiles the file content and sends the compiled Elisp back."
  [{:keys [file] :as msg}]
  (let [result (compile-code file)]
    (if (= :ok (:status result))
      (send-compiled-result msg (:elisp result))
      (send-error msg (:error result)))))

(defn handle-start
  "Activate CLJEL compilation for the session."
  [{:keys [session transport] :as msg}]
  (swap! cljel-sessions conj session)
  (t/send transport (response-for msg {:value "ClojureElisp session started"
                                       :status :done})))

(defn handle-stop
  "Deactivate CLJEL compilation for the session."
  [{:keys [session transport] :as msg}]
  (swap! cljel-sessions disj session)
  (t/send transport (response-for msg {:value "ClojureElisp session stopped"
                                       :status :done})))

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
  (fn [{:keys [op session] :as msg}]
    (case op
      "cljel-start" (handle-start msg)
      "cljel-stop"  (handle-stop msg)
      "eval"        (if (cljel-active? session)
                      (handle-eval msg)
                      (handler msg))
      "load-file"   (if (cljel-active? session)
                      (handle-load-file msg)
                      (handler msg))
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
