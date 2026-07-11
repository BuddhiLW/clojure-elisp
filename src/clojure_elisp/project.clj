(ns clojure-elisp.project
  "Project-level compilation orchestration.

   Threads a filesystem port (clojure-elisp.fs/IFilesystem) through every
   disk/classpath effect and delegates all pure work to clojure-elisp.compile.
   Public arities default the port to fs/default-fs."
  (:require [clojure.edn :as edn]
            [clojure.string :as str]
            [clojure-elisp.analyzer :as ana]
            [clojure-elisp.emitter :as emit]
            [clojure-elisp.compile :as cc]
            [clojure-elisp.errors :as errors]
            [clojure-elisp.fs :as fs]
            [hive-dsl.result :as r]
            [malli.core :as m]))

;; ============================================================================
;; Data schemas
;; ============================================================================

(def symbol-table-schema
  "Project-wide symbol table: {ns-sym -> #{def-sym ...}}."
  [:map-of :symbol [:set :symbol]])

(def artifact-schema
  "A freshly-compiled file summary."
  [:map [:input :string] [:output :string] [:size :int]])

(def cached-artifact-schema
  "A skipped (up-to-date) file summary."
  [:map [:input :string] [:output :string] [:cached [:= true]]])

(def compile-result-schema
  "One per-file outcome from compile-project: freshly compiled or cached."
  [:multi {:dispatch (fn [x] (if (and (map? x) (contains? x :cached)) :cached :compiled))}
   [:cached   cached-artifact-schema]
   [:compiled artifact-schema]])

(def manifest-schema
  "The incremental-compilation cache manifest persisted at
   <output-dir>/.clel-cache/manifest.edn."
  [:map
   [:version :int]
   [:files [:map-of :symbol
            [:map
             [:source-path :string]
             [:source-mtime :int]
             [:output-path :string]
             [:output-mtime :int]
             [:deps [:set :symbol]]]]]])

;; ============================================================================
;; File Compilation
;; ============================================================================

(defn compile-file
  "Compile a .cljel file to a .el file. Returns {:input :output :size}."
  ([input-path output-path]
   (compile-file fs/default-fs input-path output-path))
  ([fs input-path output-path]
   (let [source (fs/read-file fs input-path)
         elisp  (cc/compile-file-string source)]
     (fs/write-file! fs output-path elisp)
     {:input input-path :output output-path :size (count elisp)})))

(defn compile-file-result
  "Compile a .cljel file to a .el file, returning a Result.
   On success: {:ok {:input path :output path :size n}}
   On error:   {:error :compile/file-error :message \"...\" ...}"
  ([input-path output-path]
   (compile-file-result fs/default-fs input-path output-path))
  ([fs input-path output-path]
   ;; compile-file-string-result is staged, so a reader failure keeps its
   ;; :compile/read-error tag rather than being flattened to :analysis-error.
   (r/let-ok [source (r/try-effect* :compile/file-error (fs/read-file fs input-path))
              elisp  (cc/compile-file-string-result source)]
             (r/try-effect* :compile/file-error
                            (fs/write-file! fs output-path elisp)
                            {:input input-path :output output-path :size (count elisp)}))))

(defn compile-ns
  "Compile a namespace to Elisp. Looks for the source file in the classpath."
  ([ns-sym]
   (compile-ns fs/default-fs ns-sym))
  ([fs ns-sym]
   (let [path   (-> (str ns-sym)
                    (str/replace "." "/")
                    (str/replace "-" "_")
                    (str ".cljel"))
         source (fs/read-resource fs path)]
     (when source
       (cc/compile-file-string source)))))

;; ============================================================================
;; Project Symbol Table & Discovery
;; ============================================================================

(defn- discover-cljel-files
  "Discover all .cljel files under source paths."
  [fs source-paths]
  (->> source-paths
       (mapcat #(fs/list-files fs %))
       (filter #(str/ends-with? % ".cljel"))))

(defn build-project-symbol-table
  "Build a project-wide symbol table by scanning all .cljel files.
   Returns {ns-sym -> #{def-sym ...}}."
  ([file-paths]
   (build-project-symbol-table fs/default-fs file-paths))
  ([fs file-paths]
   (into {}
         (for [path  file-paths
               :let  [source  (fs/read-file fs path)
                      forms   (cc/read-all-forms (cc/preprocess-elisp-syntax source))
                      ns-name (cc/extract-ns-name source)]
               :when ns-name]
           [ns-name (ana/scan-exports (rest forms))]))))

;; ============================================================================
;; Incremental Compilation (mtime tracking)
;; ============================================================================

(defn- manifest-path [output-dir]
  (str output-dir "/.clel-cache/manifest.edn"))

(defn- read-manifest [fs output-dir]
  (let [path  (manifest-path output-dir)
        empty {:version 1 :files {}}]
    (if (fs/file-exists? fs path)
      ;; A corrupt/stale-shaped manifest must not crash the build — treat it as
      ;; a cold cache and rebuild from scratch.
      (let [parsed (edn/read-string (fs/read-file fs path))]
        (if (m/validate manifest-schema parsed) parsed empty))
      empty)))

(defn- write-manifest [fs output-dir manifest]
  (fs/make-dirs! fs (str output-dir "/.clel-cache"))
  (fs/write-file! fs (manifest-path output-dir) (pr-str manifest)))

(defn- file-mtime [fs path]
  (fs/file-mtime fs path))

(defn- needs-recompile?
  "True if source changed, output missing, not in manifest, or a dep is stale."
  [fs ns-sym ns->file output-dir manifest stale-set]
  (let [input-path  (get ns->file ns-sym)
        output-name (str (emit/mangle-name ns-sym) ".el")
        output-path (str output-dir "/" output-name)
        entry       (get-in manifest [:files ns-sym])]
    (or
      (nil? entry)
      (not= (file-mtime fs input-path) (:source-mtime entry))
      (not (fs/file-exists? fs output-path))
      (some stale-set (:deps entry)))))

(defn compile-project
  "Compile all .cljel files under source-paths in dependency order.
   Pass 1 builds a project-wide symbol table; pass 2 compiles each stale file
   with cross-file symbol checking. Returns a vector of compilation results."
  ([source-paths output-dir]
   (compile-project fs/default-fs source-paths output-dir))
  ([fs source-paths output-dir]
   (let [files        (discover-cljel-files fs source-paths)
         exports      (build-project-symbol-table fs files)
         path->source (into {} (for [path files] [path (fs/read-file fs path)]))
         ns->file     (into {}
                            (for [[path source] path->source
                                  :let  [ns-name (cc/extract-ns-name source)]
                                  :when ns-name]
                              [ns-name path]))
         graph        (cc/build-dependency-graph path->source)
         order        (cc/topological-sort graph)
         manifest     (read-manifest fs output-dir)
         stale-set    (loop [remaining order
                             stale     #{}]
                        (if (empty? remaining)
                          stale
                          (let [ns-sym (first remaining)]
                            (if (needs-recompile? fs ns-sym ns->file output-dir manifest stale)
                              (recur (rest remaining) (conj stale ns-sym))
                              (recur (rest remaining) stale)))))]
     (fs/make-dirs! fs output-dir)
     (let [results
           (binding [ana/*project-exports* exports]
             (mapv (fn [ns-sym]
                     (ana/clear-macros!)
                     (when-let [input-path (get ns->file ns-sym)]
                       (let [output-name (str (emit/mangle-name ns-sym) ".el")
                             output-path (str output-dir "/" output-name)]
                         (if (contains? stale-set ns-sym)
                           (compile-file fs input-path output-path)
                           {:input input-path :output output-path :cached true}))))
                   order))
           new-manifest {:version 1
                         :files (into {}
                                      (for [ns-sym order
                                            :let [input-path (get ns->file ns-sym)
                                                  output-name (str (emit/mangle-name ns-sym) ".el")
                                                  output-path (str output-dir "/" output-name)]
                                            :when input-path]
                                        [ns-sym {:source-path input-path
                                                 :source-mtime (file-mtime fs input-path)
                                                 :output-path output-path
                                                 :output-mtime (file-mtime fs output-path)
                                                 :deps (get graph ns-sym #{})}]))}]
       (write-manifest fs output-dir new-manifest)
       results))))

;; ============================================================================
;; Self-Hosted Runtime Compilation
;; ============================================================================

(defn- read-version
  "Read the project version from the VERSION file (classpath or filesystem)."
  [fs]
  (if-let [r (fs/read-resource fs "clojure-elisp/VERSION")]
    (str/trim r)
    (str/trim (fs/read-file fs "VERSION"))))

(defn- runtime-header
  "MELPA-compatible header for the compiled runtime .el file."
  [fs]
  (str ";;; clojure-elisp-runtime.el --- Runtime library for ClojureElisp -*- lexical-binding: t; -*-\n"
       "\n"
       ";; Copyright (C) 2025 Pedro G. Branquinho\n"
       ";; Author: Pedro G. Branquinho <pedrogbranquinho@gmail.com>\n"
       ";; Maintainer: Pedro G. Branquinho <pedrogbranquinho@gmail.com>\n"
       ";; URL: https://github.com/BuddhiLW/clojure-elisp\n"
       ";; Version: " (read-version fs) "\n"
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
   Adds the MELPA header, cl-lib/seq requires, and the (provide ...) footer."
  ([input-path output-path]
   (compile-runtime fs/default-fs input-path output-path))
  ([fs input-path output-path]
   (let [source (fs/read-file fs input-path)
         code   (cc/compile-file-string source)
         elisp  (str (runtime-header fs) "\n" code
                     "\n\n(provide 'clojure-elisp-runtime)\n"
                     ";;; clojure-elisp-runtime.el ends here\n")]
     (fs/write-file! fs output-path elisp)
     {:input input-path :output output-path :size (count elisp)})))

;; ============================================================================
;; Function Contracts (Malli)
;; ============================================================================

(m/=> compile-file
      [:function
       [:=> [:cat :string :string] artifact-schema]
       [:=> [:cat fs/Fs :string :string] artifact-schema]])

(m/=> compile-file-result
      [:function
       [:=> [:cat :string :string] errors/file-result-schema]
       [:=> [:cat fs/Fs :string :string] errors/file-result-schema]])

(m/=> compile-ns
      [:function
       [:=> [:cat :symbol] [:maybe :string]]
       [:=> [:cat fs/Fs :symbol] [:maybe :string]]])

(m/=> build-project-symbol-table
      [:function
       [:=> [:cat [:sequential :string]] symbol-table-schema]
       [:=> [:cat fs/Fs [:sequential :string]] symbol-table-schema]])

(m/=> compile-project
      [:function
       [:=> [:cat [:sequential :string] :string] [:vector [:maybe compile-result-schema]]]
       [:=> [:cat fs/Fs [:sequential :string] :string] [:vector [:maybe compile-result-schema]]]])

(m/=> compile-runtime
      [:function
       [:=> [:cat :string :string] artifact-schema]
       [:=> [:cat fs/Fs :string :string] artifact-schema]])
