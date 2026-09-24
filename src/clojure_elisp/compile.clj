(ns clojure-elisp.compile
  "Pure compile pipeline: Clojure forms/source text -> Elisp source text.

   Contains no filesystem or classpath effects — every function here is a
   calculation over strings, forms, and AST nodes. Orchestration that touches
   disk lives in clojure-elisp.project / clojure-elisp.config."
  (:require [clojure.string :as str]
            [clojure-elisp.analyzer :as ana]
            [clojure-elisp.emitter :as emit]
            [clojure-elisp.errors :as errors]
            [clojure-elisp.names :as names]
            [clojure-elisp.reader :as reader]
            [hive-dsl.result :as r]
            [malli.core :as m]))

;; Every public entry point below is one compilation: it runs inside
;; names/with-fresh-names, so generated names are numbered from 1 and the
;; output is a pure function of the input on every host.

;; ============================================================================
;; Single-Form Compilation
;; ============================================================================

(defn emit
  "Compile a Clojure form to an Elisp string."
  [form]
  (names/with-fresh-names
    (-> form ana/analyze emit/emit)))

(defn emit-forms
  "Compile multiple forms to Elisp, joined by newlines."
  [forms]
  (names/with-fresh-names
    (->> forms (map emit) (str/join "\n\n"))))

(defn emit-result
  "Compile a Clojure form to Elisp, returning a Result.
   On success: {:ok \"elisp-string\"}
   On error:   {:error :compile/analysis-error :message \"...\" ...}"
  [form]
  (names/with-fresh-names
    (r/try-effect* :compile/analysis-error
                   (-> form ana/analyze emit/emit))))

(defn emit-forms-result
  "Compile multiple forms to Elisp, returning a Result."
  [forms]
  (names/with-fresh-names
    (r/try-effect* :compile/analysis-error
                   (->> forms
                        (map (fn [f] (-> f ana/analyze emit/emit)))
                        (str/join "\n\n")))))

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

;; The scanners below index source text as a char vector (cs), never as a
;; string: string indexing is O(n) on ClojureWasm, which made a char-by-char
;; pass over the 64 KB runtime quadratic there.

(defn- char-at
  "The char at position i of cs, or nil past either end."
  [cs i]
  (when (< -1 i (count cs))
    (nth cs i)))

(defn- chars->str
  "The text of cs between positions start (inclusive) and end (exclusive)."
  [cs start end]
  (apply str (subvec cs start end)))

(defn- count-preceding-backslashes
  "Count consecutive backslashes preceding position i in cs."
  [cs i]
  (loop [j (dec i) n 0]
    (if (= (char-at cs j) \\)
      (recur (dec j) (inc n))
      n)))

(defn- token-start?
  "True if position i in cs is at a token boundary."
  [cs i]
  (or (zero? i)
      (let [prev (char-at cs (dec i))]
        (or (reader/whitespace-char? prev)
            (= prev \() (= prev \[) (= prev \{)
            (= prev \,) (= prev \')))))

(defn- scan-elisp-source
  "Walk source text with string/comment awareness, calling handlers.
   Handlers are {:on-code f, :on-string f}; each f takes (cs, i), cs being the
   source as a char vector, and returns {:emit \"text\" :skip N} to replace
   chars, or nil to pass through."
  [s {:keys [on-code on-string]}]
  (let [cs  (vec s)
        len (count cs)]
    (loop [i          0
           in-string? false
           out        (transient [])]
      (if (>= i len)
        (apply str (persistent! out))
        (let [ch (nth cs i)]
          (cond
            (= ch \")
            (recur (inc i)
                   (if (even? (count-preceding-backslashes cs i))
                     (not in-string?) in-string?)
                   (conj! out ch))

            (and (not in-string?) (= ch \;))
            (let [eol (loop [j i]
                        (if (or (>= j len) (= \newline (nth cs j))) j (recur (inc j))))]
              (recur eol in-string? (conj! out (chars->str cs i eol))))

            :else
            (let [handler (if in-string? on-string on-code)]
              (if-let [{:keys [emit skip]} (when handler (handler cs i))]
                (recur (+ i skip) in-string? (conj! out emit))
                (recur (inc i) in-string? (conj! out ch))))))))))

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
  "Collect up to max-n consecutive digits from cs at pos passing pred?."
  [cs pos max-n pred?]
  (let [len (count cs)
        end (loop [j pos]
              (if (and (< j (min len (+ pos max-n)))
                       (pred? (nth cs j)))
                (recur (inc j))
                j))]
    (chars->str cs pos end)))

(defn- parse-digits
  "Parse a string of hex or octal digits in the given radix."
  [digits radix]
  (reduce (fn [acc c]
            (let [d (cond
                      (<= (int \0) (int c) (int \9)) (- (int c) (int \0))
                      (<= (int \a) (int c) (int \f)) (+ 10 (- (int c) (int \a)))
                      :else                          (+ 10 (- (int c) (int \A))))]
              (+ (* acc radix) d)))
          0
          digits))

(defn- unicode-escape
  "Clojure \\uXXXX escape for a code point below 0x10000, lowercase hex."
  [code]
  (let [hex (reduce (fn [acc shift]
                      (str acc (nth "0123456789abcdef"
                                    (mod (quot code shift) 16))))
                    ""
                    [4096 256 16 1])]
    (str "\\u" hex)))

(defn- translate-char-literal
  "Recognize an Elisp char literal at position i in cs.
   Returns {:emit \"<int>\" :skip N} or nil. Handles ?\\s ?\\033 ?\\x1b ?a."
  [cs i]
  (let [len (count cs)]
    (when (and (= (nth cs i) \?)
               (token-start? cs i)
               (< (inc i) len))
      (let [next-ch (nth cs (inc i))]
        (cond
          (and (= next-ch \\) (< (+ i 2) len))
          (let [esc-ch (nth cs (+ i 2))]
            (cond
              (and (= esc-ch \x) (< (+ i 3) len))
              (let [hex-str (collect-digits cs (+ i 3) 2 hex-digit?)]
                (when (pos? (count hex-str))
                  {:emit (str (parse-digits hex-str 16))
                   :skip (+ 3 (count hex-str))}))

              (octal-digit? esc-ch)
              (let [oct-str (collect-digits cs (+ i 2) 3 octal-digit?)]
                {:emit (str (parse-digits oct-str 8))
                 :skip (+ 2 (count oct-str))})

              :else
              (when-let [code (get elisp-named-char-table (str \\ esc-ch))]
                {:emit (str code) :skip 3})))

          (and (not (reader/whitespace-char? next-ch))
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
  [cs i]
  (let [len (count cs)]
    (when (and (= (nth cs i) \\) (< (inc i) len))
      (let [next-ch (nth cs (inc i))]
        (cond
          (= next-ch \e)
          {:emit "\\u001b" :skip 2}

          (= next-ch \a)
          {:emit "\\u0007" :skip 2}

          (and (<= (int \0) (int next-ch) (int \3))
               (< (+ i 2) len)
               (octal-digit? (nth cs (+ i 2))))
          (let [digits (collect-digits cs (inc i) 3 octal-digit?)]
            {:emit (unicode-escape (parse-digits digits 8))
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

(defn- reader-failure
  "Wrap a clojure-elisp.reader error in the compiler's reader-error message."
  [e]
  (let [{:keys [line type]} (ex-data e)]
    (if (= ::reader/invalid-number type)
      (ex-info (str "Unhandled Elisp number symbol: " (ex-message e)
                    " (line " line ")"
                    " — add to elisp-number-symbols map")
               {:line line}
               e)
      (ex-info (str "Reader error at line " line
                    ": " (ex-message e)
                    "\nHint: if you see \"Unsupported escape character\","
                    " backslash-newline (\\<newline>) in strings is Elisp-only;"
                    " use a plain string or \\n instead.")
               {:line line}
               e))))

(defn read-all-forms
  "Read all forms from a string, preserving source line/column metadata.
   Source should be preprocessed with preprocess-elisp-syntax first.

   Uses clojure-elisp.reader, not the host reader, so every host (JVM,
   Babashka, ClojureWasm) reads the same forms with the same metadata."
  [s]
  (try
    (reader/read-forms s)
    (catch Exception e
      (throw (reader-failure e)))))

;; ============================================================================
;; String Compilation
;; ============================================================================

(defn compile-file-string
  "Compile a string of Clojure code as a file (with namespace context).
   (ns ...) establishes aliases/refers for subsequent forms; appends
   (provide ...) when ns is present."
  [s]
  (names/with-fresh-names
    (let [preprocessed (preprocess-elisp-syntax s)
          forms        (read-all-forms preprocessed)
          ast-nodes    (ana/analyze-file-forms forms)
          raw-elisp    (emit/emit-file ast-nodes)]
      (postprocess-elisp-syntax raw-elisp))))

(defn compile-string
  "Compile a string of Clojure code to Elisp.
   For namespace-aware compilation, use compile-file-string instead."
  [s]
  (names/with-fresh-names
    (let [preprocessed (preprocess-elisp-syntax s)
          forms        (read-all-forms preprocessed)
          raw-elisp    (emit-forms forms)]
      (postprocess-elisp-syntax raw-elisp))))

(defn compile-string-in-ns
  "Compile s in the context of context-source: the buffer's (ns ...) form at
   minimum, or the whole buffer to also resolve calls to sibling definitions.
   nil or blank means no context.

   Emits only the forms in s: no file header, no (provide ...), nothing from
   context-source. Definitions carry the namespace prefix compile-file-string
   gives them."
  [context-source s]
  (names/with-fresh-names
    (let [context-forms (if (str/blank? context-source)
                          []
                          (read-all-forms (preprocess-elisp-syntax context-source)))
          forms         (read-all-forms (preprocess-elisp-syntax s))
          ast-nodes     (ana/analyze-file-forms (into (vec context-forms) forms))
          body          (drop (count context-forms) ast-nodes)]
      (postprocess-elisp-syntax (str/join "\n\n" (map emit/emit body))))))

(defn compile-string-in-ns-result
  "Compile s against context-source, returning a Result.
   Staged so the reader boundary tags failures :compile/read-error."
  [context-source s]
  (names/with-fresh-names
    (r/let-ok [context-forms (r/try-effect*
                              :compile/read-error
                              (if (str/blank? context-source)
                                []
                                (read-all-forms (preprocess-elisp-syntax context-source))))
               forms         (r/try-effect*
                              :compile/read-error
                              (read-all-forms (preprocess-elisp-syntax s)))]
      (r/try-effect* :compile/analysis-error
                     (-> (ana/analyze-file-forms (into (vec context-forms) forms))
                         (->> (drop (count context-forms)) (map emit/emit)
                              (str/join "\n\n"))
                         postprocess-elisp-syntax)))))

(defn leading-ns-source
  "Return the source text of the leading (ns ...) form in source, or nil.
   Returns nil rather than throwing when source cannot be read."
  [source]
  (when-not (str/blank? source)
    (try
      (let [form (reader/read-first (preprocess-elisp-syntax source))]
        (when (and (seq? form) (= 'ns (first form)))
          (pr-str form)))
      (catch Exception _ nil))))

(defn compile-file-string-result
  "Compile a string of Clojure code as a file, returning a Result.
   Staged so the reader boundary tags failures :compile/read-error (rather than
   mis-attributing them to :compile/analysis-error): preprocessing + reading are
   one stage, analysis + emit + postprocessing the next."
  [s]
  (names/with-fresh-names
    (r/let-ok [forms (r/try-effect* :compile/read-error
                                    (read-all-forms (preprocess-elisp-syntax s)))]
      (r/try-effect* :compile/analysis-error
                     (postprocess-elisp-syntax
                      (emit/emit-file (ana/analyze-file-forms forms)))))))

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
   order (dependencies first). Throws on circular dependency.

   Ties are broken by name, so the order does not depend on the host's hash
   order: the JVM, Babashka and ClojureWasm compile a project in the same
   sequence."
  [graph]
  (let [by-name   (fn [nodes] (sort-by str nodes))
        all-nodes (by-name (keys graph))
        in-degree (reduce-kv (fn [m node deps]
                               (assoc m node (count deps)))
                             {}
                             graph)]
    (loop [queue            (vec (filter #(zero? (get in-degree %)) all-nodes))
           result           []
           remaining-degree in-degree]
      (if (empty? queue)
        (if (= (count result) (count all-nodes))
          result
          (throw (ex-info "Circular dependency detected"
                          {:unresolved (remove (set result) all-nodes)})))
        (let [node        (first queue)
              dependents  (by-name (for [[n deps] graph
                                         :when    (contains? deps node)]
                                     n))
              new-degree  (reduce (fn [d dep]
                                    (update d dep dec))
                                  remaining-degree
                                  dependents)
              newly-ready (filter #(zero? (get new-degree %)) dependents)]
          (recur (into (subvec queue 1) newly-ready)
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
(m/=> compile-string-in-ns-result
      [:=> [:cat [:maybe :string] :string] errors/string-result-schema])
(m/=> leading-ns-source          [:=> [:cat [:maybe :string]] [:maybe :string]])
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
