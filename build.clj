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
(def readme "README.md")

(defn coord-patterns
  "Regexes matching the README install coordinates as (prefix)(version)(suffix).
   Derived from `lib`, so a group/artifact rename can't silently orphan them."
  []
  (let [l (java.util.regex.Pattern/quote (str lib))]
    [;; deps.edn:   io.github.buddhilw/clojure-elisp {:mvn/version "X"}
     (re-pattern (str "(" l " \\{:mvn/version \")([^\"]+)(\")"))
     ;; Leiningen:  [io.github.buddhilw/clojure-elisp "X"]
     (re-pattern (str "(\\[" l " \")([^\"]+)(\")"))]))

(defn readme-versions
  "Every version currently pinned in the README's install coordinates."
  [source]
  (into [] (mapcat #(map second (re-seq % source))) (coord-patterns)))

(defn- sync-readme
  "Rewrite the README install coordinates to `v`. Returns the new source."
  [source v]
  (reduce (fn [s re] (str/replace s re (str "$1" v "$3")))
          source
          (coord-patterns)))

(defn- tag-exists?
  "True when the git tag vV already exists in this checkout.

   Uses `tag --list`, whose output is the matched tag or nothing. `rev-parse`
   is unusable here: on an unknown revision it echoes the argument back on
   stdout, which git-process returns, so every tag would look present."
  [v]
  (not (str/blank? (b/git-process {:git-args ["tag" "--list" (str "v" v)]}))))

(defn- next-free-version
  "First A.B.X — X ascending from V's patch — whose vA.B.X tag is unused.
   Returns V itself when its own tag is free."
  [v]
  (let [[maj min pat] (str/split v #"\.")
        start (some-> pat parse-long)]
    (when-not (and maj min start)
      (throw (ex-info "VERSION is not A.B.C; cannot auto-bump the patch"
                      {:version v})))
    (loop [p start]
      (let [candidate (str maj "." min "." p)]
        (if (tag-exists? candidate)
          (recur (inc p))
          candidate)))))

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

(defn- sync-version!
  "Propagate the on-disk VERSION (reported as `v`) to everything that restates
   it: the classpath VERSION resource, which stamps the runtime .el MELPA
   header, and the README install coordinates."
  [v]
  (spit version-resource (slurp "VERSION"))
  (println (str "Synced " version-resource " -> " v))
  (let [before (slurp readme)
        after  (sync-readme before v)]
    (when (not= before after)
      (spit readme after))
    (println (str "Synced " readme " install coords -> " v
                  (when (= before after) " (already current)")))))

(defn sync-version
  "Propagate the canonical top-level VERSION to everything that restates it."
  [_]
  (sync-version! version))

(defn bump-version
  "Resolve the version to release and propagate it across every tracked file.

   A hand-edited VERSION wins when its tag is free; otherwise the patch
   advances to the first untagged A.B.X. VERSION, the classpath resource and
   the README coordinates are written together, so the consistency guard is
   never left failing. Writes version/tag to GITHUB_OUTPUT when it is set.
   Returns the resolved version."
  [_]
  (let [current  (str/trim (slurp "VERSION"))
        resolved (next-free-version current)]
    (when (not= current resolved)
      (spit "VERSION" (str resolved "\n"))
      (println (str "Bumped VERSION " current " -> " resolved)))
    (sync-version! resolved)
    (when-let [out (System/getenv "GITHUB_OUTPUT")]
      (spit out (str "version=" resolved "\ntag=v" resolved "\n") :append true))
    (println (str "Release version: " resolved))
    resolved))

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
