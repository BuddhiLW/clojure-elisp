(ns clojure-elisp.mcp
  "Minimal MCP (Model Context Protocol) stdio server.
   Exposes the ClojureElisp compiler as MCP tools over JSON-RPC."
  (:require [clojure-elisp.core :as clel]
            [clojure-elisp.analyzer :as ana]
            [clojure-elisp.schema :as schema]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [malli.core :as m]
            [malli.json-schema :as mjs])
  (:import [java.io BufferedReader InputStreamReader OutputStream]))

;; ---------------------------------------------------------------------------
;; JSON helpers (minimal, no external deps)
;; ---------------------------------------------------------------------------

(defn- json-str
  "Escape a string for JSON output."
  [s]
  (-> s
      (str/replace "\\" "\\\\")
      (str/replace "\"" "\\\"")
      (str/replace "\n" "\\n")
      (str/replace "\r" "\\r")
      (str/replace "\t" "\\t")))

(defn- to-json
  "Convert a Clojure value to a JSON string. Supports maps, vectors, strings,
   numbers, booleans, and nil."
  [x]
  (cond
    (nil? x)    "null"
    (string? x) (str "\"" (json-str x) "\"")
    (number? x) (str x)
    (true? x)   "true"
    (false? x)  "false"
    (keyword? x) (str "\"" (json-str (name x)) "\"")
    (map? x)    (str "{"
                     (str/join ","
                               (map (fn [[k v]]
                                      (str (to-json (if (keyword? k) (name k) (str k)))
                                           ":" (to-json v)))
                                    x))
                     "}")
    (sequential? x) (str "[" (str/join "," (map to-json x)) "]")
    :else (str "\"" (json-str (pr-str x)) "\"")))

(defn- parse-json
  "Minimal JSON parser using Clojure's EDN reader with some transforms.
   Handles the subset of JSON that MCP clients send."
  [s]
  (let [s (str/trim s)]
    (-> s
        (str/replace #"\"([^\"\\]|\\.)*\"" #(identity %1))  ; preserve strings
        (read-string))))  ; fallback — works for simple cases

;; Use clojure.edn for safe parsing, then fixup JSON→Clojure differences
(defn- parse-json-value
  "Parse a JSON string into Clojure data. Uses a simple recursive descent approach."
  [^String s]
  ;; We'll use Java's built-in JSON support if available, otherwise a simple approach
  ;; For JDK 11+, we can use a simple tokenizer
  (let [reader (java.io.PushbackReader. (java.io.StringReader. s))]
    (letfn [(skip-ws []
              (loop []
                (let [c (.read reader)]
                  (cond
                    (= c -1) nil
                    (Character/isWhitespace (char c)) (recur)
                    :else (.unread reader c)))))
            (read-string-val []
              (.read reader) ; consume opening "
              (let [sb (StringBuilder.)]
                (loop []
                  (let [c (.read reader)]
                    (cond
                      (= c -1) (str sb)
                      (= (char c) \") (str sb)
                      (= (char c) \\)
                      (let [next-c (.read reader)]
                        (case (char next-c)
                          \n (.append sb \newline)
                          \t (.append sb \tab)
                          \r (.append sb \return)
                          \" (.append sb \")
                          \\ (.append sb \\)
                          \/ (.append sb \/)
                          (.append sb (char next-c)))
                        (recur))
                      :else (do (.append sb (char c)) (recur)))))))
            (read-number []
              (let [sb (StringBuilder.)]
                (loop []
                  (let [c (.read reader)]
                    (if (and (not= c -1)
                             (or (Character/isDigit (char c))
                                 (= (char c) \.)
                                 (= (char c) \-)
                                 (= (char c) \+)
                                 (= (char c) \e)
                                 (= (char c) \E)))
                      (do (.append sb (char c)) (recur))
                      (do (when (not= c -1) (.unread reader c))
                          (let [s (str sb)]
                            (if (str/includes? s ".")
                              (Double/parseDouble s)
                              (Long/parseLong s)))))))))
            (read-value []
              (skip-ws)
              (let [c (.read reader)]
                (cond
                  (= c -1) nil
                  (= (char c) \") (do (.unread reader c) (read-string-val))
                  (= (char c) \{) (read-object)
                  (= (char c) \[) (read-array)
                  (= (char c) \t) (do (.read reader) (.read reader) (.read reader) true) ; true
                  (= (char c) \f) (do (dotimes [_ 4] (.read reader)) false) ; false
                  (= (char c) \n) (do (.read reader) (.read reader) (.read reader) nil) ; null
                  (or (Character/isDigit (char c)) (= (char c) \-))
                  (do (.unread reader c) (read-number))
                  :else nil)))
            (read-object []
              (loop [m {}]
                (skip-ws)
                (let [c (.read reader)]
                  (cond
                    (= (char c) \}) m
                    (= (char c) \,) (recur m)
                    (= (char c) \")
                    (do (.unread reader c)
                        (let [k (read-string-val)]
                          (skip-ws)
                          (.read reader) ; consume :
                          (let [v (read-value)]
                            (recur (assoc m k v)))))
                    :else m))))
            (read-array []
              (loop [arr []]
                (skip-ws)
                (let [c (.read reader)]
                  (cond
                    (= c -1) arr
                    (= (char c) \]) arr
                    (= (char c) \,) (recur arr)
                    :else (do (.unread reader c)
                              (recur (conj arr (read-value))))))))]
      (read-value))))

;; ---------------------------------------------------------------------------
;; MCP tool definitions — Malli :input schemas are the single source of truth
;; ---------------------------------------------------------------------------

(def mcp-content-schema
  "MCP tool-call result: a vector of content blocks. Only text blocks are produced."
  [:vector [:map ["type" :string] ["text" :string]]])

(defn- canonical-property
  "Order a derived JSON-Schema property fragment the way the MCP wire form lists
   it: `type` first, then any remaining keys (e.g. `description`) in place."
  [prop]
  (into (array-map)
        (concat (select-keys prop [:type])
                (dissoc prop :type))))

(defn- malli->input-schema
  "Derive an MCP JSON-Schema `inputSchema` map from a Malli `:input` map schema
   via `malli.json-schema/transform`, canonicalized to the wire key order. The
   Malli schema is thus the single source of truth for a tool's arguments — the
   wire schema is generated, never hand-maintained. A `:closed` `:input` schema
   surfaces as `additionalProperties:false`, so the advertised wire contract
   rejects unknown arguments exactly as the pre-dispatch validation does."
  [input]
  (let [{jtype :type props :properties required :required :as js}
        (mjs/transform (schema/schema input))]
    (cond-> (array-map "type" jtype
                       "properties" (update-vals props canonical-property))
      (seq required)                     (assoc "required" required)
      (contains? js :additionalProperties) (assoc "additionalProperties"
                                                  (:additionalProperties js)))))

(def tool-specs
  "MCP tools defined by their Malli `:input` schema — the single source of truth
   for both the wire `inputSchema` (derived below) and argument validation before
   dispatch. `:input` uses string map keys so it matches the JSON-parsed
   arguments (string-keyed) and the wire property names directly. Each schema is
   `:closed`, so an unknown argument is rejected before dispatch (and advertised
   as `additionalProperties:false` on the wire)."
  [{:name "compile_string"
    :description "Compile a ClojureElisp code string to Emacs Lisp"
    :input [:map {:closed true} ["code" {:description "ClojureElisp source code to compile"} :string]]}
   {:name "compile_file"
    :description "Compile a .cljel file to a .el file"
    :input [:map {:closed true}
            ["input"  {:description "Path to input .cljel file"} :string]
            ["output" {:description "Path to output .el file"} :string]]}
   {:name "analyze"
    :description "Analyze ClojureElisp code and return the AST as EDN"
    :input [:map {:closed true} ["code" {:description "ClojureElisp source code to analyze"} :string]]}])

(def tools
  "MCP tool descriptors advertised over `tools/list`. Each `inputSchema` is
   DERIVED from the tool's Malli `:input` schema, so the wire contract and the
   validation contract cannot drift."
  (mapv (fn [{:keys [name description input]}]
          {"name" name
           "description" description
           "inputSchema" (malli->input-schema input)})
        tool-specs))

(def ^:private tool-input-schema
  "Tool name -> Malli `:input` schema, for validating arguments before dispatch."
  (into {} (map (juxt :name :input)) tool-specs))

(defn- handle-tool-call
  "Execute a tool call and return the result content."
  [tool-name arguments]
  (try
    (case tool-name
      "compile_string"
      (let [code (get arguments "code")
            result (clel/compile-string code)]
        [{"type" "text" "text" result}])

      "compile_file"
      (let [input (get arguments "input")
            output (get arguments "output")
            result (clel/compile-file input output)]
        [{"type" "text" "text" (str "Compiled " (:input result)
                                    " -> " (:output result)
                                    " (" (:size result) " chars)")}])

      "analyze"
      (let [code (get arguments "code")
            forms (read-string (str "[" code "]"))
            asts (mapv ana/analyze forms)]
        [{"type" "text" "text" (pr-str asts)}])

      ;; unknown tool
      (throw (ex-info (str "Unknown tool: " tool-name) {})))
    (catch Exception e
      [{"type" "text" "text" (str "Error: " (.getMessage e))}])))

(m/=> handle-tool-call [:=> [:cat :string [:map-of :string :any]] mcp-content-schema])

;; ---------------------------------------------------------------------------
;; JSON-RPC / MCP protocol
;; ---------------------------------------------------------------------------

(defn- respond [id result]
  {"jsonrpc" "2.0" "id" id "result" result})

(defn- error-response [id code message]
  {"jsonrpc" "2.0" "id" id
   "error" {"code" code "message" message}})

(defn- handle-message
  "Handle a single JSON-RPC message and return the response map (or nil for notifications)."
  [msg]
  (let [method (get msg "method")
        id     (get msg "id")
        params (get msg "params" {})]
    (case method
      "initialize"
      (respond id
               {"protocolVersion" "2024-11-05"
                "capabilities" {"tools" {"listChanged" false}}
                "serverInfo" {"name" "clojure-elisp"
                              "version" (or (some-> (io/resource "clojure-elisp/VERSION")
                                                    slurp
                                                    str/trim)
                                            "dev")}})

      "notifications/initialized"
      nil ; notification, no response

      "tools/list"
      (respond id {"tools" tools})

      "tools/call"
      (let [tool-name (get params "name")
            arguments (get params "arguments" {})
            input     (get tool-input-schema tool-name)
            errors    (when input (schema/humanize input arguments))]
        (if errors
          ;; Reject malformed arguments before touching the compiler.
          (respond id {"content" [{"type" "text"
                                   "text" (str "Invalid arguments: " (pr-str errors))}]
                       "isError" true})
          (respond id {"content" (handle-tool-call tool-name arguments)
                       "isError" false})))

      "ping"
      (respond id {})

      ;; unknown method
      (if id
        (error-response id -32601 (str "Method not found: " method))
        nil))))

;; ---------------------------------------------------------------------------
;; stdio transport
;; ---------------------------------------------------------------------------

(defn- read-message
  "Read a JSON-RPC message using Content-Length framing from the reader.
   Returns the parsed message map, or nil on EOF."
  [^BufferedReader reader]
  (loop []
    (let [line (.readLine reader)]
      (cond
        (nil? line) nil ; EOF

        (str/starts-with? line "Content-Length:")
        (let [length (Long/parseLong (str/trim (subs line 16)))]
          ;; Read remaining headers until blank line
          (loop []
            (let [header (.readLine reader)]
              (when (and header (not (str/blank? header)))
                (recur))))
          ;; Read body
          (let [buf (char-array length)]
            (loop [offset 0]
              (when (< offset length)
                (let [n (.read reader buf offset (- length offset))]
                  (when (pos? n)
                    (recur (+ offset n))))))
            (parse-json-value (String. buf))))

        (str/blank? line) (recur) ; skip blank lines

        :else (recur))))) ; skip unknown headers

(defn- write-message
  "Write a JSON-RPC message with Content-Length framing to stdout."
  [^OutputStream out msg]
  (let [body (.getBytes (to-json msg) "UTF-8")
        header (.getBytes (str "Content-Length: " (alength body) "\r\n\r\n") "UTF-8")]
    (.write out header)
    (.write out body)
    (.flush out)))

(defn start-server
  "Start the MCP stdio server. Reads from stdin, writes to stdout.
   Blocks until stdin is closed."
  []
  (let [reader (BufferedReader. (InputStreamReader. System/in "UTF-8"))
        out    System/out]
    ;; Redirect *out* to stderr so println in tools doesn't corrupt the protocol
    (binding [*out* (io/writer System/err)]
      (loop []
        (when-let [msg (read-message reader)]
          (when-let [response (handle-message msg)]
            (write-message out response))
          (recur))))))
