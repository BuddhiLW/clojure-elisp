(ns build
  (:require [clojure.tools.build.api :as b]
            [clojure.string :as str]))

(def lib 'io.github.BuddhiLW/clojure-elisp)
(def version (str/trim (slurp "VERSION")))
(def class-dir "target/classes")
(def uber-file (format "target/clel-%s.jar" version))
(def basis (delay (b/create-basis {:project "deps.edn"})))

(defn clean [_]
  (b/delete {:path "target"}))

(defn uber [_]
  (clean nil)
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
