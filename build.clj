(ns build
  (:require [clojure.tools.build.api :as b]
            [clojure.string :as str]))

(def lib 'io.github.buddhilw/clojure-elisp)
(def version (str/trim (slurp "VERSION")))
(def class-dir "target/classes")
(def uber-file (format "target/clel-%s.jar" version))
(def jar-file (format "target/%s-%s.jar" (name lib) version))
(def basis (delay (b/create-basis {:project "deps.edn"})))

(def version-resource "resources/clojure-elisp/VERSION")

(def pom-data
  [[:description "A Clojure-to-Emacs-Lisp compiler: analyzes Clojure-style source into an AST and emits Emacs Lisp, with a self-hosted runtime library."]
   [:url "https://github.com/BuddhiLW/clojure-elisp"]
   [:licenses
    [:license
     [:name "MIT"]
     [:url "https://opensource.org/license/mit"]]]
   [:scm
    [:url "https://github.com/BuddhiLW/clojure-elisp"]
    [:connection "scm:git:git://github.com/BuddhiLW/clojure-elisp.git"]
    [:developerConnection "scm:git:ssh://git@github.com/BuddhiLW/clojure-elisp.git"]
    [:tag (str "v" version)]]
   [:developers
    [:developer
     [:name "Pedro G. Branquinho"]]]])

(defn clean [_]
  (b/delete {:path "target"}))

(defn sync-version
  "Regenerate the classpath VERSION resource from the canonical top-level VERSION."
  [_]
  (spit version-resource (slurp "VERSION"))
  (println (str "Synced " version-resource " -> " version)))

(defn jar
  "Build the library thin jar + pom for Clojars/Maven consumption."
  [_]
  (clean nil)
  (sync-version nil)
  (b/write-pom {:class-dir class-dir
                :lib lib
                :version version
                :basis @basis
                :src-dirs ["src"]
                :pom-data pom-data})
  (b/copy-dir {:src-dirs ["src" "resources"]
               :target-dir class-dir})
  (b/jar {:class-dir class-dir
          :jar-file jar-file})
  (println (str "Built " jar-file)))

(defn deploy
  "Deploy the library jar to Clojars. Requires CLOJARS_USERNAME + CLOJARS_PASSWORD
   (a Clojars deploy token) in the environment."
  [_]
  (jar nil)
  ((requiring-resolve 'deps-deploy.deps-deploy/deploy)
   {:installer :remote
    :artifact  jar-file
    :pom-file  (b/pom-path {:lib lib :class-dir class-dir})})
  (println (str "Deployed " lib " " version " to Clojars")))

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
