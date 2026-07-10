(ns clojure-elisp.version-consistency-test
  "Guards kanban 20260710145203-7196a846: the two tracked VERSION files must not
   drift. /VERSION drives the jar name + git tag (build.clj); the classpath
   resource resources/clojure-elisp/VERSION stamps the compiled runtime .el
   MELPA header (core/read-version). A release that bumps only /VERSION leaves
   the .el header lagging. `clojure -T:build sync-version` (also run inside
   `uber`) regenerates the resource from /VERSION; this test fails CI if they
   ever diverge."
  (:require [clojure.test :refer [deftest is]]))

(deftest version-files-stay-in-sync
  (is (= (slurp "VERSION") (slurp "resources/clojure-elisp/VERSION"))
      "/VERSION and resources/clojure-elisp/VERSION diverged — run `clojure -T:build sync-version`"))
