(ns clojure-elisp.core
  "ClojureElisp — a Clojure dialect that compiles to Emacs Lisp.

   Public façade over the stratified compiler layers:
     clojure-elisp.compile  — pure pipeline (forms/source -> elisp)
     clojure-elisp.project  — file/project orchestration over the fs port
     clojure-elisp.config   — clel.edn descriptor
     clojure-elisp.errors   — error vocabulary (ADT + Result schemas)
     clojure-elisp.fs       — filesystem Boundary port

   Usage:
     (require '[clojure-elisp.core :as clel])
     (clel/emit '(defn greet [name] (str \"Hello, \" name)))
     (clel/compile-file \"src/my_package.cljel\" \"out/my-package.el\")
     (clel/compile-ns 'my.package)

   Result-returning variants return {:ok ...} or {:error tag :message ...}."
  (:require [clojure-elisp.compile :as cc]
            [clojure-elisp.project :as project]
            [clojure-elisp.config :as config]
            [clojure-elisp.fs :as fs]
            [clojure-elisp.errors :as errors]
            [malli.core :as m]
            [malli.instrument :as mi]))

;; ============================================================================
;; Public API — instrumentable pipeline entry points
;; ============================================================================

(defn emit
  "Compile a Clojure form to an Elisp string."
  [form]
  (cc/emit form))

(defn emit-forms
  "Compile multiple forms to Elisp, joined by newlines."
  [forms]
  (cc/emit-forms forms))

(defn emit-result
  "Compile a Clojure form to Elisp, returning a Result."
  [form]
  (cc/emit-result form))

(defn emit-forms-result
  "Compile multiple forms to Elisp, returning a Result."
  [forms]
  (cc/emit-forms-result forms))

(defn compile-string
  "Compile a string of Clojure code to Elisp."
  [s]
  (cc/compile-string s))

(defn compile-string-in-ns
  "Compile a string of Clojure code in the context of context-source.
   context-source is the buffer's (ns ...) form at minimum, or the whole
   buffer to also make sibling definitions visible to call sites.
   Emits the forms only: no file header, no (provide ...)."
  [context-source s]
  (cc/compile-string-in-ns context-source s))

(defn leading-ns-source
  "Return the source text of the leading (ns ...) form in source, or nil."
  [source]
  (cc/leading-ns-source source))

(defn compile-file-string
  "Compile a string of Clojure code as a file (with namespace context)."
  [s]
  (cc/compile-file-string s))

(defn compile-file-string-result
  "Compile a string of Clojure code as a file, returning a Result."
  [s]
  (cc/compile-file-string-result s))

(defn compile-file-result
  "Compile a .cljel file to a .el file, returning a Result."
  [input-path output-path]
  (project/compile-file-result input-path output-path))

;; ============================================================================
;; Public API — re-exports
;; ============================================================================
;;
;; Two flavours, split by whether the underlying var carries an m/=> contract:
;;
;;   * Contract-bearing pipeline fns (compile/project) are re-exported as thin
;;     defn WRAPPERS that call through the delegate var. Because the body
;;     resolves the delegate at call time, a `clel/…` call dispatches to the
;;     delegate's CURRENT root — i.e. the instrumented wrapper once instrument!
;;     has run. The boundary contract is thus enforced whether a caller reaches
;;     the fn via clel/… or via the compile/project var directly.
;;
;;   * Contract-free helpers/predicates stay as plain `def` aliases: there is no
;;     contract to enforce, so a wrapper would add only indirection.

;; Contract-bearing pipeline re-exports (instrumented via their delegate var).
(defn compile-file
  "Compile a .cljel file to a .el file. Returns {:input :output :size}."
  ([input-path output-path]    (project/compile-file input-path output-path))
  ([fs input-path output-path] (project/compile-file fs input-path output-path)))

(defn compile-ns
  "Compile a namespace to Elisp, resolving its source on the classpath."
  ([ns-sym]    (project/compile-ns ns-sym))
  ([fs ns-sym] (project/compile-ns fs ns-sym)))

(defn build-project-symbol-table
  "Scan .cljel files into a project-wide {ns-sym -> #{def-sym …}} table."
  ([file-paths]    (project/build-project-symbol-table file-paths))
  ([fs file-paths] (project/build-project-symbol-table fs file-paths)))

(defn compile-project
  "Compile all .cljel files under source-paths in dependency order."
  ([source-paths output-dir]    (project/compile-project source-paths output-dir))
  ([fs source-paths output-dir] (project/compile-project fs source-paths output-dir)))

(defn bundle-runtime!
  "Write clojure-elisp-runtime.el from the classpath into output-dir.
   Returns {:runtime-output path} or nil when the resource is absent."
  ([output-dir] (bundle-runtime! fs/default-fs output-dir))
  ([fs output-dir] (config/bundle-runtime fs output-dir)))

(defn compile-runtime
  "Compile the self-hosted runtime .cljel to its .el library."
  ([input-path output-path]    (project/compile-runtime input-path output-path))
  ([fs input-path output-path] (project/compile-runtime fs input-path output-path)))

(defn extract-ns-name
  "Extract the namespace symbol declared by a source string, or nil."
  [source]
  (cc/extract-ns-name source))

(defn ns-derived-output-name
  "Derive the mangled .el output filename for a source string's namespace."
  [source]
  (cc/ns-derived-output-name source))

(defn topological-sort
  "Topologically sort a {ns -> #{dep-ns …}} dependency graph."
  [graph]
  (cc/topological-sort graph))

;; Contract-free re-exports (plain aliases — nothing to instrument).
(def valid-string-result?        errors/valid-string-result?)
(def valid-file-result?          errors/valid-file-result?)
(def read-project-config         config/read-project-config)
(def compile-project-from-config config/compile-project-from-config)

;; Preprocessing internals reached by tests via #'clojure-elisp.core/…
(def ^:private preprocess-elisp-numbers        cc/preprocess-elisp-numbers)
(def ^:private postprocess-elisp-numbers       cc/postprocess-elisp-numbers)
(def ^:private preprocess-elisp-char-literals  cc/preprocess-elisp-char-literals)
(def ^:private preprocess-elisp-string-escapes cc/preprocess-elisp-string-escapes)

;; ============================================================================
;; Function Contracts (Malli)
;; ============================================================================

(m/=> emit                       [:=> [:cat :any] :string])
(m/=> emit-forms                 [:=> [:cat [:sequential :any]] :string])
(m/=> compile-string             [:=> [:cat :string] :string])
(m/=> compile-file-string        [:=> [:cat :string] :string])
(m/=> emit-result                [:=> [:cat :any] errors/string-result-schema])
(m/=> emit-forms-result          [:=> [:cat [:sequential :any]] errors/string-result-schema])
(m/=> compile-file-string-result [:=> [:cat :string] errors/string-result-schema])
(m/=> compile-file-result        [:=> [:cat :string :string] errors/file-result-schema])

(def ^:private instrumented-nses
  "Boundary namespaces whose m/=> contracts we enforce together. compile.clj and
   project.clj carry the real pipeline contracts, so instrumenting core alone
   would leave them unenforced. emitter.clj carries the emit-surface contracts
   (emit/emit-file/name helpers); its emit-node MultiFn is uncontracted, so
   instrument! leaves it untouched.

   The contract-bearing pipeline re-exports above (compile-file, compile-ns,
   compile-project, extract-ns-name, …) are defn WRAPPERS that call through the
   compile/project delegate var, so a clel/… call dispatches to the delegate's
   instrumented root once instrument! has run — full coverage of core's public
   surface. Only the four contract-free aliases (valid-*-result?,
   read-project-config, compile-project-from-config) capture a fn value at load
   time, and they carry no contract to enforce anyway."
  ['clojure-elisp.core
   'clojure-elisp.compile
   'clojure-elisp.project
   'clojure-elisp.analyzer
   'clojure-elisp.emitter
   'clojure-elisp.nrepl-kernel])

(defn instrument!
  "Enable Malli instrumentation of the boundary fn contracts (core + compile +
   project). Call unstrument! to disable. Intended for dev/test."
  []
  (mi/instrument! {:filters [(apply mi/-filter-ns instrumented-nses)]}))

(defn unstrument!
  "Disable Malli instrumentation of the boundary fn contracts."
  []
  (mi/unstrument! {:filters [(apply mi/-filter-ns instrumented-nses)]}))

(comment
  (emit '(defn foo [x] (+ x 1)))
  (emit '(let [a 1 b 2] (+ a b)))
  (compile-runtime "resources/clojure-elisp/runtime.cljel"
                   "resources/clojure-elisp/clojure-elisp-runtime.el"))
