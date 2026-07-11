(ns clojure-elisp.config
  "Project descriptor (clel.edn) reading and config-driven compilation.

   Filesystem reads/writes go through the fs port; java.io.File is used only
   for pure path arithmetic (resolving relative source/output paths)."
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure-elisp.project :as project]
            [clojure-elisp.fs :as fs]
            [malli.core :as m]
            [malli.error :as me]))

(def ^:private default-project-config
  "Default values for clel.edn project config."
  {:source-paths ["src"]
   :output-dir   "out"
   :runtime      :require})

(def project-config-schema
  "Schema for a resolved clel.edn descriptor (after defaults are merged).
   Open — projects may carry extra keys the compiler ignores."
  [:map {:closed false}
   [:source-paths [:vector :string]]
   [:output-dir :string]
   [:runtime [:enum :bundled :require]]])

(defn read-project-config
  "Read a clel.edn project config file. Returns a map with :source-paths,
   :output-dir, :runtime; missing keys filled with defaults. Validates the
   merged descriptor against `project-config-schema`, throwing an ex-info with
   a humanized explanation on any shape violation."
  ([config-path]
   (read-project-config fs/default-fs config-path))
  ([fs config-path]
   (let [raw    (edn/read-string (fs/read-file fs config-path))
         _      (when-not (map? raw)
                  (throw (ex-info "clel.edn must contain a map"
                                  {:path config-path :value raw})))
         config (merge default-project-config raw)]
     (when-not (m/validate project-config-schema config)
       (let [errors (me/humanize (m/explain project-config-schema config))]
         (throw (ex-info (str "Invalid clel.edn: " (pr-str errors))
                         {:path config-path :errors errors :config config}))))
     config)))

(defn- bundle-runtime
  "Copy the runtime .el resource into the output directory."
  [fs output-dir]
  (when-let [content (fs/read-resource fs "clojure-elisp/clojure-elisp-runtime.el")]
    (fs/make-dirs! fs output-dir)
    (let [dest (str output-dir "/clojure-elisp-runtime.el")]
      (fs/write-file! fs dest content)
      {:runtime-output dest})))

(defn compile-project-from-config
  "Compile a project using a clel.edn config file. With no arguments, reads
   clel.edn from the current directory. Resolves source-paths and output-dir
   relative to the config file's directory."
  ([] (compile-project-from-config "clel.edn"))
  ([config-path]
   (compile-project-from-config fs/default-fs config-path))
  ([fs config-path]
   (let [config-file  (io/file config-path)
         base-dir     (.getParentFile (.getAbsoluteFile config-file))
         config       (read-project-config fs config-path)
         source-paths (mapv #(.getAbsolutePath (io/file base-dir %))
                            (:source-paths config))
         output-dir   (.getAbsolutePath (io/file base-dir (:output-dir config)))
         results      (project/compile-project fs source-paths output-dir)]
     (when (= :bundled (:runtime config))
       (bundle-runtime fs output-dir))
     results)))
