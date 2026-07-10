(ns build
  (:require [clojure.tools.build.api :as b]
            [clojure.string :as str]))

(def lib 'io.github.BuddhiLW/clojure-elisp)
(def version (str/trim (slurp "VERSION")))
(def class-dir "target/classes")
(def uber-file (format "target/clel-%s.jar" version))
(def basis (delay (b/create-basis {:project "deps.edn"})))

(def version-resource "resources/clojure-elisp/VERSION")

(defn clean [_]
  (b/delete {:path "target"}))

(defn sync-version
  "Regenerate the classpath VERSION resource from the canonical top-level VERSION."
  [_]
  (spit version-resource (slurp "VERSION"))
  (println (str "Synced " version-resource " -> " version)))

(defn uber [_]
  (clean nil)
  (sync-version nil)
  (b/copy-dir {:src-dirs ["src" "resources"]
               :target-dir class-dir})
  (b/compile-clj {:basis @basis
                  :src-dirs ["src"]
                  :class-dir class-dir
                  :ns-compile ['clojure-elisp.cli]})
  (b/uber {:class-dir class-dir
           :uber-file uber-file
           :basis @basis
           :main 'clojure-elisp.cli})
  (println (str "Built " uber-file)))
