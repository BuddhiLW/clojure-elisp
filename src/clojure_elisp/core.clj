(ns clojure-elisp.core
  "ClojureElisp - A Clojure dialect that compiles to Emacs Lisp.

   Similar to ClojureScript for JavaScript, ClojureElisp brings
   Clojure's syntax and semantics to Emacs Lisp.

   Usage:
     (require '[clojure-elisp.core :as clel])

     ;; Compile a form to elisp string
     (clel/emit '(defn greet [name] (str \"Hello, \" name)))

     ;; Compile a file
     (clel/compile-file \"src/my_package.cljel\" \"out/my-package.el\")

     ;; Compile a namespace
     (clel/compile-ns 'my.package)

   Result-returning variants:
     (clel/emit-result '(+ 1 2))           ;; => {:ok \"(+ 1 2)\"}
     (clel/compile-file-result in out)      ;; => {:ok {...}} or {:error ...}"
  (:require [clojure-elisp.analyzer :as ana]
            [clojure-elisp.emitter :as emit]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [hive-dsl.result :as r]))

;; ============================================================================
;; Single-Form Compilation
;; ============================================================================

(defn emit
  "Compile a Clojure form to an Elisp string."
  [form]
  (-> form
      ana/analyze
      emit/emit))

(defn emit-forms
  "Compile multiple forms to Elisp, joined by newlines."
  [forms]
  (->> forms
       (map emit)
       (str/join "\n\n")))

;; ============================================================================
;; Result-Returning API Variants
;; ============================================================================

(defn emit-result
  "Compile a Clojure form to Elisp, returning a Result.
   On success: {:ok \"elisp-string\"}
   On error:   {:error :compile/analysis-error :message \"...\" ...}"
  [form]
  (r/try-effect* :compile/analysis-error
                 (-> form ana/analyze emit/emit)))

(defn emit-forms-result
  "Compile multiple forms to Elisp, returning a Result.
   On success: {:ok \"elisp-string\"}
   On error:   {:error :compile/analysis-error :message \"...\" ...}"
  [forms]
  (r/try-effect* :compile/analysis-error
                 (->> forms
                      (map (fn [f] (-> f ana/analyze emit/emit)))
                      (str/join "\n\n"))))

;; ============================================================================
;; File-Aware Compilation (with namespace context)
;; ============================================================================

;; ---------------------------------------------------------------------------
;; Elisp syntax preprocessing
;; ---------------------------------------------------------------------------
;; The Clojure reader rejects certain valid Elisp syntax:
;;   1. Number-like symbols: 1+ and 1- (increment/decrement functions)
;;   2. Hex string escapes: \xNN (Elisp) vs \uNNNN (Clojure)
;;   3. Elisp-specific escapes: \e (escape), \a (bell)
;; We preprocess before reading and postprocess after emitting.

(def ^:private elisp-number-symbols
  "Map of Elisp number-like symbols to reader-safe aliases."
  {"1+" "cljel--1plus"
   "1-" "cljel--1minus"})

(def ^:private elisp-number-symbols-reverse
  "Map of reader-safe aliases back to Elisp symbols."
  (into {} (map (fn [[k v]] [v k])) elisp-number-symbols))

(def ^:private hex-escape-pre-re
  "Regex matching Elisp \\xNN hex escapes in source text."
  (re-pattern "\\\\x([0-9a-fA-F]{1,2})"))

(def ^:private hex-escape-post-re
  "Regex matching CLJEL_HEX_ placeholders in emitted text."
  #"CLJEL_HEX_([0-9a-fA-F]{1,2})")

(defn- preprocess-elisp-escapes
  "Replace Elisp hex string escapes (\\xNN) with reader-safe placeholders.
   The Clojure reader doesn't support \\x escapes in strings."
  [s]
  (str/replace s hex-escape-pre-re "CLJEL_HEX_$1"))

(defn- postprocess-elisp-escapes
  "Restore Elisp hex escapes from placeholders in emitted code."
  [s]
  (str/replace s hex-escape-post-re "\\\\x$1"))

(defn- preprocess-elisp-numbers
  "Replace Elisp number-like symbols (1+, 1-) with reader-safe aliases.
   Only matches in call position: (1+ ...) or (1- ...)"
  [s]
  (-> s
      (str/replace #"\(1\+(?=[\s\(\)])" "(cljel--1plus")
      (str/replace #"\(1-(?=[\s\(\)])" "(cljel--1minus")))

(defn- postprocess-elisp-numbers
  "Restore Elisp number-like symbols from reader-safe aliases in emitted code."
  [s]
  (reduce-kv (fn [s alias original]
               (str/replace s alias original))
             s
             elisp-number-symbols-reverse))

;; ---------------------------------------------------------------------------
;; String-aware source scanner (Infrastructure layer)
;; ---------------------------------------------------------------------------
;; Generic scanner that walks source text character-by-character, tracking
;; whether we're inside a string literal or comment. Delegates to handler
;; functions for context-specific transformations.
;;
;; DDD: Scanning infrastructure is separated from translation domain logic.
;; SRP: Scanner tracks context; handlers translate escapes.
;; OCP: New handlers can be added without modifying the scanner.
;; FP:  Handlers are pure functions returning {:emit "..." :skip N} or nil.

(defn- count-preceding-backslashes
  "Count consecutive backslashes preceding position i in string s."
  [^String s ^long i]
  (loop [j (dec i) n 0]
    (if (and (>= j 0) (= (.charAt s j) \\))
      (recur (dec j) (inc n))
      n)))

(defn- token-start?
  "True if position i in string s is at a token boundary
   (preceded by whitespace, open delimiter, or start-of-string)."
  [^String s ^long i]
  (or (zero? i)
      (let [prev (.charAt s (dec i))]
        (or (Character/isWhitespace prev)
            (= prev \() (= prev \[) (= prev \{)
            (= prev \,) (= prev \')))))

(defn- scan-elisp-source
  "Walk source text with string/comment awareness, calling handlers.
   Handlers are {:on-code f, :on-string f} where each f takes (s, i) and
   returns {:emit \"text\" :skip N} to replace chars, or nil to pass through.
   :on-code is called for chars outside strings/comments.
   :on-string is called for chars inside strings."
  [^String s {:keys [on-code on-string]}]
  (let [sb  (StringBuilder.)
        len (count s)]
    (loop [i 0
           in-string? false]
      (if (>= i len)
        (.toString sb)
        (let [ch (.charAt s i)]
          (cond
            ;; Toggle string state on unescaped double-quote
            (= ch \")
            (do (.append sb ch)
                (recur (inc i)
                       (if (even? (count-preceding-backslashes s i))
                         (not in-string?) in-string?)))

            ;; Comment line — copy to end-of-line, skip handlers
            (and (not in-string?) (= ch \;))
            (let [eol (let [nl (.indexOf s (int \newline) i)]
                        (if (neg? nl) len nl))]
              (.append sb (.substring s i eol))
              (recur eol in-string?))

            ;; Inside string — try on-string handler
            in-string?
            (if-let [{:keys [emit skip]} (when on-string (on-string s i))]
              (do (.append sb ^String emit)
                  (recur (+ i (long skip)) in-string?))
              (do (.append sb ch)
                  (recur (inc i) in-string?)))

            ;; Outside string — try on-code handler
            :else
            (if-let [{:keys [emit skip]} (when on-code (on-code s i))]
              (do (.append sb ^String emit)
                  (recur (+ i (long skip)) in-string?))
              (do (.append sb ch)
                  (recur (inc i) in-string?)))))))))

;; ---------------------------------------------------------------------------
;; Char literal translation (Domain layer — SRP: one concern)
;; ---------------------------------------------------------------------------
;; Pure function: given source + position, recognizes Elisp char literals
;; and returns their integer replacement or nil.

(def ^:private elisp-named-char-table
  "Named Elisp character escapes to their integer codepoints."
  {"\\s" 32, "\\t" 9, "\\n" 10, "\\r" 13, "\\e" 27, "\\a" 7,
   "\\b" 8, "\\f" 12, "\\d" 127, "\\\\" 92})

(defn- hex-digit?
  "True if char c is a hexadecimal digit."
  [c]
  (or (<= (int \0) (int c) (int \9))
      (<= (int \a) (int c) (int \f))
      (<= (int \A) (int c) (int \F))))

(defn- octal-digit?
  "True if char c is an octal digit."
  [c]
  (<= (int \0) (int c) (int \7)))

(defn- collect-digits
  "Collect up to max-n consecutive digits from s starting at pos,
   using pred? to test each char. Returns the substring."
  [^String s ^long pos ^long max-n pred?]
  (let [len (count s)
        end (loop [j pos]
              (if (and (< j (min len (+ pos max-n)))
                       (pred? (.charAt s j)))
                (recur (inc j))
                j))]
    (.substring s pos end)))

(defn- translate-char-literal
  "Recognize an Elisp char literal at position i in source s.
   Returns {:emit \"<int>\" :skip N} or nil if not a char literal.
   Handles: ?\\s ?\\033 ?\\x1b ?a etc."
  [^String s ^long i]
  (let [len (count s)]
    (when (and (= (.charAt s i) \?)
               (token-start? s i)
               (< (inc i) len))
      (let [next-ch (.charAt s (inc i))]
        (cond
          ;; ?\<escape> — escaped char literal
          (and (= next-ch \\) (< (+ i 2) len))
          (let [esc-ch (.charAt s (+ i 2))]
            (cond
              ;; ?\xNN — hex
              (and (= esc-ch \x) (< (+ i 3) len))
              (let [hex-str (collect-digits s (+ i 3) 2 hex-digit?)]
                (when (pos? (count hex-str))
                  {:emit (str (Integer/parseInt hex-str 16))
                   :skip (+ 3 (count hex-str))}))

              ;; ?\NNN — octal
              (octal-digit? esc-ch)
              (let [oct-str (collect-digits s (+ i 2) 3 octal-digit?)]
                {:emit (str (Integer/parseInt oct-str 8))
                 :skip (+ 2 (count oct-str))})

              ;; ?\s ?\e ?\n etc. — named escape
              :else
              (when-let [code (get elisp-named-char-table (str \\ esc-ch))]
                {:emit (str code) :skip 3})))

          ;; ?<printable> — plain char literal
          (and (not (Character/isWhitespace next-ch))
               (not= next-ch \\))
          {:emit (str (int next-ch)) :skip 2}

          :else nil)))))

(defn- preprocess-elisp-char-literals
  "Replace Elisp char literals with integer values. String-aware."
  [s]
  (scan-elisp-source s {:on-code translate-char-literal}))

;; ---------------------------------------------------------------------------
;; String escape translation (Domain layer — SRP: one concern)
;; ---------------------------------------------------------------------------
;; Pure function: given source + position inside a string, recognizes
;; Elisp-specific escapes and returns Clojure-compatible replacements.

(defn- translate-string-escape
  "Recognize an Elisp-specific string escape at position i.
   Returns {:emit \"\\uXXXX\" :skip N} or nil if not an Elisp escape.
   Handles: \\e → \\u001b, \\a → \\u0007, \\0NNN → \\uXXXX."
  [^String s ^long i]
  (let [len (count s)]
    (when (and (= (.charAt s i) \\) (< (inc i) len))
      (let [next-ch (.charAt s (inc i))]
        (cond
          ;; \e → ESC
          (= next-ch \e)
          {:emit "\\u001b" :skip 2}

          ;; \a → BEL
          (= next-ch \a)
          {:emit "\\u0007" :skip 2}

          ;; \0NNN → \uXXXX (octal)
          (and (<= (int \0) (int next-ch) (int \3))
               (< (+ i 2) len)
               (octal-digit? (.charAt s (+ i 2))))
          (let [digits (collect-digits s (inc i) 3 octal-digit?)
                code   (Integer/parseInt digits 8)]
            {:emit (format "\\u%04x" code)
             :skip (+ 1 (count digits))})

          ;; \\ — pass through both chars (prevent next \ from being misread)
          (= next-ch \\)
          {:emit "\\\\" :skip 2}

          ;; Other escape — pass through both chars
          :else
          {:emit (str \\ next-ch) :skip 2})))))

(defn- preprocess-elisp-string-escapes
  "Replace Elisp string escapes with Clojure-compatible \\uXXXX. String-aware."
  [s]
  (scan-elisp-source s {:on-string translate-string-escape}))

(defn- preprocess-elisp-syntax
  "Combined preprocessing: char literals + numbers + string escapes + hex.
   Pipeline: each stage handles one concern (SRP), composed via -> (FP)."
  [s]
  (-> s
      preprocess-elisp-char-literals
      preprocess-elisp-numbers
      preprocess-elisp-string-escapes
      preprocess-elisp-escapes))

(defn- postprocess-elisp-syntax
  "Combined postprocessing: numbers + string escapes."
  [s]
  (-> s postprocess-elisp-numbers postprocess-elisp-escapes))

;; ---------------------------------------------------------------------------
;; Reader
;; ---------------------------------------------------------------------------

(defn- number-format-cause?
  "Check if an exception (or its cause chain) originates from a NumberFormatException."
  [^Throwable e]
  (loop [^Throwable ex e]
    (cond
      (nil? ex) false
      (instance? NumberFormatException ex) true
      :else (recur (.getCause ex)))))

(defn- read-all-forms
  "Read all forms from a string, preserving source line/column metadata.
   Uses LineNumberingPushbackReader so the Clojure reader attaches
   :line and :column metadata to forms.
   NOTE: Source should be preprocessed with preprocess-elisp-numbers first."
  [s]
  (let [rdr (clojure.lang.LineNumberingPushbackReader.
             (java.io.StringReader. s))]
    (loop [forms []]
      (let [form (try
                   ;; read with EOF sentinel: returns ::eof at end-of-stream
                   ;; without throwing, so we only catch real syntax errors.
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

(defn compile-file-string
  "Compile a string of Clojure code as a file (with namespace context).
   Uses analyze-file-forms so (ns ...) establishes aliases/refers
   for subsequent forms. Appends (provide ...) when ns is present.
   Handles Elisp number-symbols (1+, 1-) via pre/post-processing."
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

;; ============================================================================
;; File Compilation
;; ============================================================================

(defn compile-file
  "Compile a .cljel file to a .el file.
   Uses file-level analysis for namespace context."
  [input-path output-path]
  (let [source (slurp input-path)
        elisp  (compile-file-string source)]
    (spit output-path elisp)
    {:input input-path
     :output output-path
     :size (count elisp)}))

(defn compile-file-string-result
  "Compile a string of Clojure code as a file, returning a Result.
   On success: {:ok \"elisp-string\"}
   On error:   {:error :compile/analysis-error :message \"...\" ...}"
  [s]
  (r/try-effect* :compile/analysis-error
                 (compile-file-string s)))

(defn compile-file-result
  "Compile a .cljel file to a .el file, returning a Result.
   On success: {:ok {:input path :output path :size n}}
   On error:   {:error :compile/file-error :message \"...\" ...}"
  [input-path output-path]
  (r/let-ok [source (r/try-effect* :compile/file-error (slurp input-path))
             elisp  (r/try-effect* :compile/analysis-error (compile-file-string source))]
            (r/try-effect* :compile/file-error
                           (spit output-path elisp)
                           {:input input-path :output output-path :size (count elisp)})))

(defn compile-ns
  "Compile a namespace to Elisp.
   Looks for the source file in the classpath."
  [ns-sym]
  (let [path     (-> (str ns-sym)
                     (str/replace "." "/")
                     (str/replace "-" "_")
                     (str ".cljel"))
        resource (io/resource path)]
    (when resource
      (compile-file-string (slurp resource)))))

;; ============================================================================
;; Dependency Graph & Topological Sort
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
  "Derive an output .el filename from the ns form in source.
   Returns nil if source has no ns form.
   E.g., source with (ns hive-mcp-projectile) → \"hive-mcp-projectile.el\""
  [source]
  (when-let [ns-sym (extract-ns-name source)]
    (str (emit/mangle-name ns-sym) ".el")))

(defn- extract-ns-deps
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
   graph is a map of {node -> #{dependency-nodes}}.
   Returns a vector of nodes in dependency order (dependencies first).
   Throws on circular dependency."
  [graph]
  (let [all-nodes (set (keys graph))
        ;; Build reverse adjacency: who depends on whom
        ;; in-degree = number of deps each node has
        in-degree (reduce-kv (fn [m node deps]
                               (assoc m node (count deps)))
                             {}
                             graph)]
    (loop [queue            (into clojure.lang.PersistentQueue/EMPTY
                                  (filter #(zero? (get in-degree %)) all-nodes))
           result           []
           remaining-degree in-degree]
      (if (empty? queue)
        ;; Check if all nodes are processed
        (if (= (count result) (count all-nodes))
          result
          (throw (ex-info "Circular dependency detected"
                          {:unresolved (remove (set result) all-nodes)})))
        (let [node        (peek queue)
              queue       (pop queue)
              ;; Find nodes that depend on this one and decrement their in-degree
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

(defn- discover-cljel-files
  "Discover all .cljel files under source paths."
  [source-paths]
  (->> source-paths
       (mapcat #(file-seq (io/file %)))
       (filter #(.endsWith (.getName %) ".cljel"))
       (map #(.getAbsolutePath %))))

(defn- build-dependency-graph
  "Build a dependency graph from a collection of source files.
   Returns {ns-sym -> #{dep-ns-syms}}.
   External deps (not in the file set) are filtered out to avoid
   false circular dependency detection."
  [file-paths]
  (let [raw (into {}
                  (for [path  file-paths
                        :let  [source (slurp path)
                               ns-name (extract-ns-name source)
                               deps (extract-ns-deps source)]
                        :when ns-name]
                    [ns-name (set (or deps []))]))
        local-nses (set (keys raw))]
    ;; Only keep deps that are in the local file set
    (into {} (map (fn [[ns-name deps]]
                    [ns-name (set (filter local-nses deps))])
                  raw))))

(defn compile-project
  "Compile all .cljel files under source-paths in dependency order.
   source-paths: vector of directories containing .cljel files.
   output-dir: directory for .el output files.
   Returns a vector of compilation results."
  [source-paths output-dir]
  (let [files    (discover-cljel-files source-paths)
        ;; Map ns-name -> file-path
        ns->file (into {}
                       (for [path  files
                             :let  [source (slurp path)
                                    ns-name (extract-ns-name source)]
                             :when ns-name]
                         [ns-name path]))
        graph    (build-dependency-graph files)
        order    (topological-sort graph)]
    ;; Ensure output directory exists
    (.mkdirs (io/file output-dir))
    ;; Compile in dependency order
    (mapv (fn [ns-sym]
            (when-let [input-path (get ns->file ns-sym)]
              (let [output-name (str (emit/mangle-name ns-sym) ".el")
                    output-path (str output-dir "/" output-name)]
                (compile-file input-path output-path))))
          order)))

;; ============================================================================
;; Self-Hosted Runtime Compilation
;; ============================================================================

(defn- read-version
  "Read the project version from the VERSION file (classpath or filesystem)."
  []
  (let [resource (io/resource "clojure-elisp/VERSION")]
    (if resource
      (str/trim (slurp resource))
      (str/trim (slurp "VERSION")))))

(defn- runtime-header
  "MELPA-compatible header for the compiled runtime .el file."
  []
  (str ";;; clojure-elisp-runtime.el --- Runtime library for ClojureElisp -*- lexical-binding: t; -*-\n"
       "\n"
       ";; Copyright (C) 2025 Pedro G. Branquinho\n"
       ";; Author: Pedro G. Branquinho <pedrogbranquinho@gmail.com>\n"
       ";; Maintainer: Pedro G. Branquinho <pedrogbranquinho@gmail.com>\n"
       ";; URL: https://github.com/BuddhiLW/clojure-elisp\n"
       ";; Version: " (read-version) "\n"
       ";; Package-Requires: ((emacs \"28.1\"))\n"
       ";; Keywords: languages, lisp, clojure\n"
       ";; SPDX-License-Identifier: MIT\n"
       "\n"
       ";;; Commentary:\n"
       ";;\n"
       ";; Runtime support library for ClojureElisp compiled code.\n"
       ";; Provides Clojure-like functions that don't have direct Elisp equivalents.\n"
       ";; Auto-generated from runtime.cljel — do not edit by hand.\n"
       "\n"
       ";;; Code:\n"
       "\n"
       "(require 'cl-lib)\n"
       "(require 'seq)\n"))

(defn compile-runtime
  "Compile the self-hosted runtime .cljel to the .el runtime library.
   The runtime .cljel has no (ns ...) form because runtime functions
   must not be namespace-prefixed.  This function adds the MELPA header,
   cl-lib/seq requires, and the (provide ...) footer."
  [input-path output-path]
  (let [source (slurp input-path)
        code   (compile-file-string source)
        elisp  (str (runtime-header) "\n" code
                    "\n\n(provide 'clojure-elisp-runtime)\n"
                    ";;; clojure-elisp-runtime.el ends here\n")]
    (spit output-path elisp)
    {:input input-path :output output-path :size (count elisp)}))

(comment
  ;; Quick test
  (emit '(defn foo [x] (+ x 1)))
  (emit '(let [a 1 b 2] (+ a b)))
  (emit '(if (> x 0) "positive" "non-positive"))

  ;; Compile runtime
  (compile-runtime "resources/clojure-elisp/runtime.cljel"
                   "resources/clojure-elisp/clojure-elisp-runtime.el"))
