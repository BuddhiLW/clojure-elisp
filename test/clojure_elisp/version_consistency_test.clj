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

(def ^:private el-packages
  "Every .el file carrying a MELPA `;; Version:` header. Mirrors build/el-packages;
   `el-package-list-covers-every-el-file` fails if a new .el escapes both."
  ["resources/clojure-elisp/clojure-elisp-runtime.el"
   "resources/clojure-elisp/clojure-elisp-mode.el"
   "resources/clojure-elisp/cider-clojure-elisp.el"])

(deftest el-headers-name-the-current-version
  (testing "every shipped .el header carries /VERSION"
    (doseq [path el-packages]
      (let [header (re-find #"(?m)^;; Version: (.+)$" (slurp path))]
        (is (some? header)
            (str path " lost its MELPA `;; Version:` header — the sync writes "
                 "by regex, so a header that moves stops being written and "
                 "nothing else notices."))
        (is (= (str/trim (slurp "VERSION")) (str/trim (second header)))
            (str path " lags /VERSION — run `clojure -T:build sync-version`. "
                 "MELPA Stable reads this header, so drift ships the wrong "
                 "version. clojure-elisp-mode.el and cider-clojure-elisp.el sat "
                 "two releases behind before anything looked at them."))))))

(deftest el-package-list-covers-every-el-file
  (testing "no .el ships without being in the version sync — a new package that
            nobody adds to the list would silently keep whatever header it was
            born with"
    (let [on-disk (->> (file-seq (java.io.File. "resources/clojure-elisp"))
                       (filter #(.isFile %))
                       (map #(.getPath %))
                       (filter #(str/ends-with? % ".el"))
                       set)]
      (is (= on-disk (set el-packages))
          (str "resources/clojure-elisp .el files and the synced list diverged. "
               "Add the file to build/el-packages AND to el-packages here, or "
               "say why it carries no version.")))))

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
