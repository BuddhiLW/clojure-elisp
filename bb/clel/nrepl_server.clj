(ns clel.nrepl-server
  "Standalone ClojureElisp nREPL server, no JVM required.

   Boundary transport over clojure-elisp.nrepl-kernel. Speaks clone, close,
   describe, ls-sessions and the CLJEL ops: enough of the protocol for the
   CIDER client in cider-clojure-elisp.el.

   Sessions start with CLJEL compilation ACTIVE. There is no Clojure evaluator
   here to fall through to.

     bb -m clel.nrepl-server --port 7888"
  (:require [bencode.core :as bencode]
            [clojure-elisp.nrepl-kernel :as kernel])
  (:import [java.io PushbackInputStream BufferedOutputStream EOFException]
           [java.net ServerSocket SocketException]))

(def ^:private default-port 7888)

;; ---------------------------------------------------------------------------
;; Wire codec
;; ---------------------------------------------------------------------------

(defn- ->str
  "Decode a bencode value to a String when it arrived as bytes."
  [x]
  (if (bytes? x) (String. ^bytes x "UTF-8") x))

(defn- decode-msg
  "Turn a raw bencode map into a keyword-keyed message with String values."
  [m]
  (persistent!
   (reduce-kv (fn [acc k v] (assoc! acc (keyword k) (->str v)))
              (transient {})
              (into {} m))))

(defn- encode-response
  "Build the wire map for one response, carrying the request's correlation keys."
  [{:keys [id session]} response]
  (cond-> (persistent!
           (reduce-kv (fn [acc k v] (assoc! acc (name k) v))
                      (transient {})
                      response))
    id      (assoc "id" id)
    session (assoc "session" session)))

(defn- send-responses!
  [out msg responses]
  (doseq [response responses]
    (bencode/write-bencode out (encode-response msg response))
    (.flush ^BufferedOutputStream out)))

;; ---------------------------------------------------------------------------
;; Ops this transport owns
;; ---------------------------------------------------------------------------

(def ^:private describe-response
  {"ops" (zipmap ["clone" "close" "describe" "ls-sessions"
                  "eval" "load-file" "cljel-start" "cljel-stop"]
                 (repeat {}))
   "versions" {"clojure-elisp" {"incremental" 0}}
   "status" ["done"]})

(defn- handle-transport-op
  "Handle the session/introspection ops. Returns responses, or nil."
  [{:keys [op session]} sessions]
  (case op
    "clone"       (let [new-session (str (random-uuid))]
                    (swap! sessions conj new-session)
                    ;; Compilation is on from the first message: this server
                    ;; has no Clojure evaluator to fall back to.
                    (kernel/activate! new-session)
                    [{:new-session new-session :status ["done"]}])
    "close"       (do (swap! sessions disj session)
                      (kernel/deactivate! session)
                      [{:status ["done" "session-closed"]}])
    "describe"    [describe-response]
    "ls-sessions" [{:sessions (vec @sessions) :status ["done"]}]
    nil))

(defn- unsupported
  [op]
  [{:err (str "clel nrepl compiles ClojureElisp only; it has no Clojure "
              "evaluator, so the op " (pr-str op) " is not supported.")}
   {:status ["done" "unknown-op"]}])

(defn handle-message
  "Route one decoded message to its responses."
  [msg sessions]
  (or (handle-transport-op msg sessions)
      (kernel/handle-op msg)
      (unsupported (:op msg))))

;; ---------------------------------------------------------------------------
;; Socket loop
;; ---------------------------------------------------------------------------

(defn- serve-connection!
  [socket sessions]
  (with-open [sock socket
              in  (PushbackInputStream. (.getInputStream sock))
              out (BufferedOutputStream. (.getOutputStream sock))]
    (loop []
      (when-let [raw (try (bencode/read-bencode in)
                          (catch EOFException _ nil)
                          (catch SocketException _ nil))]
        (let [msg (decode-msg raw)]
          (try
            (send-responses! out msg (handle-message msg sessions))
            (catch Exception e
              (send-responses! out msg
                               [{:err (str "clel nrepl error: " (.getMessage e))}
                                {:status ["done"]}]))))
        (recur)))))

(defn start-server!
  "Start the server on PORT and block. Returns only when the socket closes."
  [port]
  (let [server   (ServerSocket. port)
        sessions (atom #{})]
    (println (str "ClojureElisp nREPL server listening on port " port))
    (println "Connect with: M-x cider-connect-clj, then M-x cider-cljel-mode")
    (spit ".nrepl-port" (str port))
    (loop []
      (let [sock (.accept server)]
        (doto (Thread. #(serve-connection! sock sessions))
          (.setDaemon true)
          (.start)))
      (recur))))

(defn- parse-port
  [args]
  (or (some->> args
               (drop-while #(not (contains? #{"--port" "-p"} %)))
               second
               parse-long)
      default-port))

(defn -main
  [& args]
  (start-server! (parse-port args)))
