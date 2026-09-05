(ns clojure-elisp.version-consistency-test
  "Guards kanban 20260710145203-7196a846: the two tracked VERSION files must not
   drift. /VERSION drives the jar name + git tag (build.clj); the classpath
   resource resources/clojure-elisp/VERSION stamps the compiled runtime .el
   MELPA header (core/read-version). A release that bumps only /VERSION leaves
   the .el header lagging. `clojure -T:build sync-version` (also run inside
   `uber`) regenerates the resource from /VERSION; this test fails CI if they
   ever diverge."
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]))

(deftest version-files-stay-in-sync
  (is (= (slurp "VERSION") (slurp "resources/clojure-elisp/VERSION"))
      "/VERSION and resources/clojure-elisp/VERSION diverged — run `clojure -T:build sync-version`"))

(deftest runtime-el-header-names-the-current-version
  (testing "the shipped .el header carries the version it was stamped from"
    (let [header  (re-find #"(?m)^;; Version: (.+)$"
                           (slurp "resources/clojure-elisp/clojure-elisp-runtime.el"))]
      (is (some? header)
          "clojure-elisp-runtime.el lost its MELPA `;; Version:` header")
      (is (= (str/trim (slurp "VERSION")) (str/trim (second header)))
          (str "The runtime .el header lags /VERSION. sync-version writes the "
               "VERSION resource but does NOT restamp the .el, which is only "
               "written by a regen: `(clel/compile-runtime "
               "\"resources/clojure-elisp/runtime.cljel\" "
               "\"resources/clojure-elisp/clojure-elisp-runtime.el\")`. "
               "Bumping VERSION after a regen leaves the shipped runtime "
               "naming the previous release.")))))

(def ^:private readme-coord-patterns
  [;; deps.edn:   io.github.buddhilw/clojure-elisp {:mvn/version "X"}
   #"io\.github\.buddhilw/clojure-elisp \{:mvn/version \"([^\"]+)\""
   ;; Leiningen:  [io.github.buddhilw/clojure-elisp "X"]
   #"\[io\.github\.buddhilw/clojure-elisp \"([^\"]+)\""])

(deftest readme-install-coords-carry-no-concrete-version
  (let [readme (slurp "README.md")
        pinned (into [] (mapcat #(map second (re-seq % readme)))
                     readme-coord-patterns)]

    (testing "the coordinates are still findable — a README rewrite that changes
              their shape must fail loudly rather than pass vacuously"
      (is (= 3 (count pinned))
          (str "expected 3 install coordinates in README.md (deps.edn + Leiningen + bb.edn), found "
               (count pinned)
               ". If the install snippets moved or changed shape, update this test — "
               "otherwise it silently stops covering them.")))

    (testing "no coordinate names a concrete version — the release job cannot
              write to a protected main, so a hardcoded version would go stale
              the moment the next release ships. The Clojars badge is the
              single source of truth for the current version."
      (doseq [v pinned]
        (is (not (re-matches #"\d+\.\d+\.\d+.*" v))
            (str "README.md pins concrete version " (pr-str v)
                 " — use a placeholder and let the Clojars badge carry the version."))))))
