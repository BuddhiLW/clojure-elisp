(ns clojure-elisp.compile
  "Pure compile pipeline: Clojure forms/source text -> Elisp source text.

   Contains no filesystem or classpath effects — every function here is a
   calculation over strings, forms, and AST nodes. Orchestration that touches
   disk lives in clojure-elisp.project / clojure-elisp.config."
  (:require [clojure.string :as str]
            [clojure-elisp.analyzer :as ana]
            [clojure-elisp.emitter :as emit]
            [clojure-elisp.errors :as errors]
            [hive-dsl.result :as r]
            [malli.core :as m]))

;; ============================================================================
;; Single-Form Compilation
;; ============================================================================

(defn emit
  "Compile a Clojure form to an Elisp string."
  [form]
  (-> form ana/analyze emit/emit))

(defn emit-forms
  "Compile multiple forms to Elisp, joined by newlines."
  [forms]
  (->> forms (map emit) (str/join "\n\n")))

(defn emit-result
  "Compile a Clojure form to Elisp, returning a Result.
   On success: {:ok \"elisp-string\"}
   On error:   {:error :compile/analysis-error :message \"...\" ...}"
  [form]
  (r/try-effect* :compile/analysis-error
                 (-> form ana/analyze emit/emit)))

(defn emit-forms-result
  "Compile multiple forms to Elisp, returning a Result."
  [forms]
  (r/try-effect* :compile/analysis-error
                 (->> forms
                      (map (fn [f] (-> f ana/analyze emit/emit)))
                      (str/join "\n\n"))))

;; ============================================================================
;; Elisp syntax preprocessing
;; ============================================================================

(def ^:private elisp-number-symbols
  "Map of Elisp number-like symbols to reader-safe aliases."
  {"1+" "cljel--1plus"
   "1-" "cljel--1minus"})

(def ^:private elisp-number-symbols-reverse
  "Map of reader-safe aliases back to Elisp symbols."
  (into {} (map (fn [[k v]] [v k])) elisp-number-symbols))

(def ^:private hex-escape-pre-re
  (re-pattern "\\\\x([0-9a-fA-F]{1,2})"))

(def ^:private hex-escape-post-re
  #"CLJEL_HEX_([0-9a-fA-F]{1,2})")

(defn preprocess-elisp-escapes
  "Replace Elisp hex string escapes (\\xNN) with reader-safe placeholders."
  [s]
  (str/replace s hex-escape-pre-re "CLJEL_HEX_$1"))

(defn postprocess-elisp-escapes
  "Restore Elisp hex escapes from placeholders in emitted code."
  [s]
  (str/replace s hex-escape-post-re "\\\\x$1"))

(defn preprocess-elisp-numbers
  "Replace Elisp number-like symbols (1+, 1-) in call position with aliases."
  [s]
  (-> s
      (str/replace #"\(1\+(?=[\s\(\)])" "(cljel--1plus")
      (str/replace #"\(1-(?=[\s\(\)])" "(cljel--1minus")))

(defn postprocess-elisp-numbers
  "Restore Elisp number-like symbols from reader-safe aliases in emitted code."
  [s]
  (reduce-kv (fn [s alias original]
               (str/replace s alias original))
             s
             elisp-number-symbols-reverse))

(defn- count-preceding-backslashes
  "Count consecutive backslashes preceding position i in string s."
  [^String s ^long i]
  (loop [j (dec i) n 0]
    (if (and (>= j 0) (= (.charAt s j) \\))
      (recur (dec j) (inc n))
      n)))

(defn- token-start?
  "True if position i in s is at a token boundary."
  [^String s ^long i]
  (or (zero? i)
      (let [prev (.charAt s (dec i))]
        (or (Character/isWhitespace prev)
            (= prev \() (= prev \[) (= prev \{)
            (= prev \,) (= prev \')))))

(defn- scan-elisp-source
  "Walk source text with string/comment awareness, calling handlers.
   Handlers are {:on-code f, :on-string f}; each f takes (s, i) and returns
   {:emit \"text\" :skip N} to replace chars, or nil to pass through."
  [^String s {:keys [on-code on-string]}]
  (let [sb  (StringBuilder.)
        len (count s)]
    (loop [i 0
           in-string? false]
      (if (>= i len)
        (.toString sb)
        (let [ch (.charAt s i)]
          (cond
            (= ch \")
            (do (.append sb ch)
                (recur (inc i)
                       (if (even? (count-preceding-backslashes s i))
                         (not in-string?) in-string?)))

            (and (not in-string?) (= ch \;))
            (let [eol (let [nl (.indexOf s (int \newline) i)]
                        (if (neg? nl) len nl))]
              (.append sb (.substring s i eol))
              (recur eol in-string?))

            in-string?
            (if-let [{:keys [emit skip]} (when on-string (on-string s i))]
              (do (.append sb ^String emit)
                  (recur (+ i (long skip)) in-string?))
              (do (.append sb ch)
                  (recur (inc i) in-string?)))

            :else
            (if-let [{:keys [emit skip]} (when on-code (on-code s i))]
              (do (.append sb ^String emit)
                  (recur (+ i (long skip)) in-string?))
              (do (.append sb ch)
                  (recur (inc i) in-string?)))))))))

(def ^:private elisp-named-char-table
  "Named Elisp character escapes to their integer codepoints."
  {"\\s" 32, "\\t" 9, "\\n" 10, "\\r" 13, "\\e" 27, "\\a" 7,
   "\\b" 8, "\\f" 12, "\\d" 127, "\\\\" 92})

(defn- hex-digit?
  [c]
  (or (<= (int \0) (int c) (int \9))
      (<= (int \a) (int c) (int \f))
      (<= (int \A) (int c) (int \F))))

(defn- octal-digit?
  [c]
  (<= (int \0) (int c) (int \7)))

(defn- collect-digits
  "Collect up to max-n consecutive digits from s at pos passing pred?."
  [^String s ^long pos ^long max-n pred?]
  (let [len (count s)
        end (loop [j pos]
              (if (and (< j (min len (+ pos max-n)))
                       (pred? (.charAt s j)))
                (recur (inc j))
                j))]
    (.substring s pos end)))

(defn- translate-char-literal
  "Recognize an Elisp char literal at position i in s.
   Returns {:emit \"<int>\" :skip N} or nil. Handles ?\\s ?\\033 ?\\x1b ?a."
  [^String s ^long i]
  (let [len (count s)]
    (when (and (= (.charAt s i) \?)
               (token-start? s i)
               (< (inc i) len))
      (let [next-ch (.charAt s (inc i))]
        (cond
          (and (= next-ch \\) (< (+ i 2) len))
          (let [esc-ch (.charAt s (+ i 2))]
            (cond
              (and (= esc-ch \x) (< (+ i 3) len))
              (let [hex-str (collect-digits s (+ i 3) 2 hex-digit?)]
                (when (pos? (count hex-str))
                  {:emit (str (Integer/parseInt hex-str 16))
                   :skip (+ 3 (count hex-str))}))

              (octal-digit? esc-ch)
              (let [oct-str (collect-digits s (+ i 2) 3 octal-digit?)]
                {:emit (str (Integer/parseInt oct-str 8))
                 :skip (+ 2 (count oct-str))})

              :else
              (when-let [code (get elisp-named-char-table (str \\ esc-ch))]
                {:emit (str code) :skip 3})))

          (and (not (Character/isWhitespace next-ch))
               (not= next-ch \\))
          {:emit (str (int next-ch)) :skip 2}

          :else nil)))))

(defn preprocess-elisp-char-literals
  "Replace Elisp char literals with integer values. String-aware."
  [s]
  (scan-elisp-source s {:on-code translate-char-literal}))

(defn- translate-string-escape
  "Recognize an Elisp-specific string escape at position i.
   Returns {:emit \"\\uXXXX\" :skip N} or nil. Handles \\e \\a \\0NNN."
  [^String s ^long i]
  (let [len (count s)]
    (when (and (= (.charAt s i) \\) (< (inc i) len))
      (let [next-ch (.charAt s (inc i))]
        (cond
          (= next-ch \e)
          {:emit "\\u001b" :skip 2}

          (= next-ch \a)
          {:emit "\\u0007" :skip 2}

          (and (<= (int \0) (int next-ch) (int \3))
               (< (+ i 2) len)
               (octal-digit? (.charAt s (+ i 2))))
          (let [digits (collect-digits s (inc i) 3 octal-digit?)
                code   (Integer/parseInt digits 8)]
            {:emit (format "\\u%04x" code)
             :skip (+ 1 (count digits))})

          (= next-ch \\)
          {:emit "\\\\" :skip 2}

          :else
          {:emit (str \\ next-ch) :skip 2})))))

(defn preprocess-elisp-string-escapes
  "Replace Elisp string escapes with Clojure-compatible \\uXXXX. String-aware."
  [s]
  (scan-elisp-source s {:on-string translate-string-escape}))

(defn preprocess-elisp-syntax
  "Combined preprocessing: char literals + numbers + string escapes + hex."
  [s]
  (-> s
      preprocess-elisp-char-literals
      preprocess-elisp-numbers
      preprocess-elisp-string-escapes
      preprocess-elisp-escapes))

(defn postprocess-elisp-syntax
  "Combined postprocessing: numbers + string escapes."
  [s]
  (-> s postprocess-elisp-numbers postprocess-elisp-escapes))

;; ============================================================================
;; Reader
;; ============================================================================

(defn- number-format-cause?
  "True when e (or its cause chain) originates from a NumberFormatException."
  [^Throwable e]
  (loop [^Throwable ex e]
    (cond
      (nil? ex) false
      (instance? NumberFormatException ex) true
      :else (recur (.getCause ex)))))

(defn read-all-forms
  "Read all forms from a string, preserving source line/column metadata.
   Source should be preprocessed with preprocess-elisp-syntax first."
  [s]
  (let [rdr (clojure.lang.LineNumberingPushbackReader.
             (java.io.StringReader. s))]
    (loop [forms []]
      (let [form (try
                   (read rdr false ::eof)
                   (catch Exception e
                     (if (number-format-cause? e)
                       (throw (ex-info (str "Unhandled Elisp number symbol: "
                                            (if-let [cause (.getCause e)]
                                              (.getMessage cause)
                                              (.getMessage e))
                                            " (line " (.getLineNumber rdr) ")"
                                            " — add to elisp-number-symbols map")
                                       {:line (.getLineNumber rdr)}
                                       e))
                       (throw (ex-info (str "Reader error at line " (.getLineNumber rdr)
                                            ": " (.getMessage e)
                                            "\nHint: if you see \"Unsupported escape character\","
                                            " backslash-newline (\\<newline>) in strings is Elisp-only;"
                                            " use a plain string or \\n instead.")
                                       {:line (.getLineNumber rdr)}
                                       e)))))]
        (if (= ::eof form)
          forms
          (recur (conj forms form)))))))

;; ============================================================================
;; String Compilation
;; ============================================================================

(defn compile-file-string
  "Compile a string of Clojure code as a file (with namespace context).
   (ns ...) establishes aliases/refers for subsequent forms; appends
   (provide ...) when ns is present."
  [s]
  (let [preprocessed (preprocess-elisp-syntax s)
        forms        (read-all-forms preprocessed)
        ast-nodes    (ana/analyze-file-forms forms)
        raw-elisp    (emit/emit-file ast-nodes)]
    (postprocess-elisp-syntax raw-elisp)))

(defn compile-string
  "Compile a string of Clojure code to Elisp.
   For namespace-aware compilation, use compile-file-string instead."
  [s]
  (let [preprocessed (preprocess-elisp-syntax s)
        forms        (read-string (str "[" preprocessed "]"))
        raw-elisp    (emit-forms forms)]
    (postprocess-elisp-syntax raw-elisp)))

(defn compile-string-in-ns
  "Compile a string of Clojure code in the namespace context of ns-source.
   ns-source is the text of an (ns ...) form, or nil/blank for no context.

   Emits only the compiled forms: no file header, no (provide ...). Definitions
   carry the same namespace prefix compile-file-string would give them, so a
   form compiled here and the same form compiled as part of its file emit the
   same Elisp name."
  [ns-source s]
  (let [ns-forms  (if (str/blank? ns-source)
                    []
                    (read-all-forms (preprocess-elisp-syntax ns-source)))
        forms     (read-all-forms (preprocess-elisp-syntax s))
        ast-nodes (ana/analyze-file-forms (into (vec ns-forms) forms))
        body      (drop (count ns-forms) ast-nodes)]
    (postprocess-elisp-syntax (str/join "\n\n" (map emit/emit body)))))

(defn compile-file-string-result
  "Compile a string of Clojure code as a file, returning a Result.
   Staged so the reader boundary tags failures :compile/read-error (rather than
   mis-attributing them to :compile/analysis-error): preprocessing + reading are
   one stage, analysis + emit + postprocessing the next."
  [s]
  (r/let-ok [forms (r/try-effect* :compile/read-error
                                  (read-all-forms (preprocess-elisp-syntax s)))]
    (r/try-effect* :compile/analysis-error
                   (postprocess-elisp-syntax
                    (emit/emit-file (ana/analyze-file-forms forms))))))

;; ============================================================================
;; Namespace & Dependency Graph
;; ============================================================================

(defn extract-ns-name
  "Extract the namespace name from a source string by reading its ns form."
  [source]
  (let [forms (read-all-forms (preprocess-elisp-syntax source))]
    (when (and (seq forms)
               (seq? (first forms))
               (= 'ns (first (first forms))))
      (second (first forms)))))

(defn ns-derived-output-name
  "Derive an output .el filename from the ns form in source, or nil."
  [source]
  (when-let [ns-sym (extract-ns-name source)]
    (str (emit/mangle-name ns-sym) ".el")))

(defn extract-ns-deps
  "Extract dependency namespace names from a source string."
  [source]
  (let [forms (read-all-forms (preprocess-elisp-syntax source))]
    (when (and (seq forms)
               (seq? (first forms))
               (= 'ns (first (first forms))))
      (let [ns-ast (ana/analyze (first forms))]
        (mapv :ns (:requires ns-ast))))))

(defn topological-sort
  "Topologically sort a dependency graph using Kahn's algorithm.
   graph is {node -> #{dependency-nodes}}. Returns nodes in dependency
   order (dependencies first). Throws on circular dependency."
  [graph]
  (let [all-nodes (set (keys graph))
        in-degree (reduce-kv (fn [m node deps]
                               (assoc m node (count deps)))
                             {}
                             graph)]
    (loop [queue            (into clojure.lang.PersistentQueue/EMPTY
                                  (filter #(zero? (get in-degree %)) all-nodes))
           result           []
           remaining-degree in-degree]
      (if (empty? queue)
        (if (= (count result) (count all-nodes))
          result
          (throw (ex-info "Circular dependency detected"
                          {:unresolved (remove (set result) all-nodes)})))
        (let [node        (peek queue)
              queue       (pop queue)
              dependents  (for [[n deps] graph
                                :when    (contains? deps node)]
                            n)
              new-degree  (reduce (fn [d dep]
                                    (update d dep dec))
                                  remaining-degree
                                  dependents)
              newly-ready (filter #(zero? (get new-degree %)) dependents)]
          (recur (into queue newly-ready)
                 (conj result node)
                 new-degree))))))

(defn build-dependency-graph
  "Build a dependency graph from a map of {path -> source-string}.
   Returns {ns-sym -> #{dep-ns-syms}}. External deps (not in the source set)
   are filtered out to avoid false circular-dependency detection."
  [path->source]
  (let [raw (into {}
                  (for [[_ source] path->source
                        :let  [ns-name (extract-ns-name source)
                               deps    (extract-ns-deps source)]
                        :when ns-name]
                    [ns-name (set (or deps []))]))
        local-nses (set (keys raw))]
    (into {} (map (fn [[ns-name deps]]
                    [ns-name (set (filter local-nses deps))])
                  raw))))

;; ============================================================================
;; Function Contracts (Malli)
;; ============================================================================
;;
;; Contracts live HERE (the real pipeline boundary), not only on the core.clj
;; re-export wrappers — project.clj calls these fns directly. Enforced by the
;; test suite's instrumentation fixture; see clojure-elisp.core/instrument!.

(def dependency-graph-schema
  "A namespace dependency graph: {ns-sym -> #{dep-ns-syms}}."
  [:map-of :symbol [:set :symbol]])

(m/=> emit                       [:=> [:cat :any] :string])
(m/=> emit-forms                 [:=> [:cat [:sequential :any]] :string])
(m/=> compile-string             [:=> [:cat :string] :string])
(m/=> compile-string-in-ns       [:=> [:cat [:maybe :string] :string] :string])
(m/=> compile-file-string        [:=> [:cat :string] :string])
(m/=> emit-result                [:=> [:cat :any] errors/string-result-schema])
(m/=> emit-forms-result          [:=> [:cat [:sequential :any]] errors/string-result-schema])
(m/=> compile-file-string-result [:=> [:cat :string] errors/string-result-schema])
(m/=> read-all-forms             [:=> [:cat :string] [:sequential :any]])
(m/=> extract-ns-name            [:=> [:cat :string] [:maybe :symbol]])
(m/=> extract-ns-deps            [:=> [:cat :string] [:maybe [:sequential :symbol]]])
(m/=> ns-derived-output-name     [:=> [:cat :string] [:maybe :string]])
(m/=> build-dependency-graph     [:=> [:cat [:map-of :any :string]] dependency-graph-schema])
(m/=> topological-sort           [:=> [:cat dependency-graph-schema] [:vector :symbol]])
