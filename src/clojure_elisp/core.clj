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

(defn- read-all-forms
  "Read all forms from a string, preserving source line/column metadata.
   Uses LineNumberingPushbackReader so the Clojure reader attaches
   :line and :column metadata to forms."
  [s]
  (let [rdr (clojure.lang.LineNumberingPushbackReader.
             (java.io.StringReader. s))]
    (loop [forms []]
      (let [form (try (read rdr) (catch Exception _ ::eof))]
        (if (= ::eof form)
          forms
          (recur (conj forms form)))))))

(defn compile-file-string
  "Compile a string of Clojure code as a file (with namespace context).
   Uses analyze-file-forms so (ns ...) establishes aliases/refers
   for subsequent forms. Appends (provide ...) when ns is present."
  [s]
  (let [forms     (read-all-forms s)
        ast-nodes (ana/analyze-file-forms forms)]
    (emit/emit-file ast-nodes)))

(defn compile-string
  "Compile a string of Clojure code to Elisp.
   For namespace-aware compilation, use compile-file-string instead."
  [s]
  (let [forms (read-string (str "[" s "]"))]
    (emit-forms forms)))

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

(defn- extract-ns-name
  "Extract the namespace name from a source string by reading its ns form."
  [source]
  (let [forms (read-all-forms source)]
    (when (and (seq forms)
               (seq? (first forms))
               (= 'ns (first (first forms))))
      (second (first forms)))))

(defn- extract-ns-deps
  "Extract dependency namespace names from a source string."
  [source]
  (let [forms (read-all-forms source)]
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
   Returns {ns-sym -> #{dep-ns-syms}}."
  [file-paths]
  (into {}
        (for [path  file-paths
              :let  [source (slurp path)
                     ns-name (extract-ns-name source)
                     deps (extract-ns-deps source)]
              :when ns-name]
          [ns-name (set (or deps []))])))

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
