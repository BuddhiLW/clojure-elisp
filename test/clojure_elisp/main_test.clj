(ns clojure-elisp.main-test
  "The portable CLI (clojure-elisp.main): the same commands on every host.
   `run` returns the exit code instead of exiting, so it is tested directly."
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure-elisp.core :as clel]
            [clojure-elisp.main :as main]))

(defn- temp-dir []
  (doto (io/file (System/getProperty "java.io.tmpdir") (str "clel-main-" (System/nanoTime)))
    (.mkdirs)))

(defn- run
  "Run the CLI, returning {:code :out :err}."
  [& args]
  (let [err  (java.io.StringWriter.)
        code (atom nil)
        out  (with-out-str
               (binding [*err* err]
                 (reset! code (main/run args))))]
    {:code @code :out out :err (str err)}))

(deftest compile-one-file
  (let [dir    (temp-dir)
        input  (io/file dir "greet.cljel")
        output (io/file dir "out" "greet.el")
        source "(ns my.greet)\n(defn hi [n] (str \"hi \" n))"]
    (spit input source)
    (let [{:keys [code out]} (run "compile" (str input) "-o" (str output))]
      (is (zero? code))
      (is (str/starts-with? out (str "Compiled " input " -> " output)))
      (is (= (clel/compile-file-string source) (slurp output))))
    (testing "without -o, the output is named after the ns, next to the source"
      (is (zero? (:code (run "compile" (str input)))))
      (is (.isFile (io/file dir "my-greet.el"))))))

(deftest compile-a-directory
  (let [dir (temp-dir)
        src (doto (io/file dir "src" "app") (.mkdirs))
        out (io/file dir "out")]
    (spit (io/file src "util.cljel") "(ns app.util)\n(defn twice [x] (* 2 x))")
    (spit (io/file src "core.cljel") "(ns app.core (:require [app.util :as u]))\n(defn f [x] (u/twice x))")
    (let [{:keys [code out]} (run "compile" (str (io/file dir "src")) "-o" (str out))]
      (is (zero? code))
      (is (= 2 (count (re-seq #"Compiled " out)))))
    (is (.isFile (io/file out "app-util.el")))
    (is (.isFile (io/file out "app-core.el")))))

(deftest usage-errors
  (is (= 1 (:code (run "compile" "/no/such/file.cljel"))))
  (is (= 2 (:code (run "compile" "x.cljel" "-o"))))
  (is (str/includes? (:err (run "frobnicate")) "Unknown command: frobnicate"))
  (is (= 1 (:code (run))))
  (is (zero? (:code (run "--help")))))

(deftest reports-compile-errors-without-a-stack-trace
  (let [dir   (temp-dir)
        input (io/file dir "bad.cljel")]
    (spit input "(defn f [x]\n  (inc x)")
    (let [{:keys [code err]} (run "compile" (str input) "-o" (str (io/file dir "bad.el")))]
      (is (= 1 code))
      (is (str/includes? err "EOF while reading")))))

(deftest version-comes-from-the-resource
  (let [{:keys [code out]} (run "version")]
    (is (zero? code))
    (is (= (str "clojure-elisp " (str/trim (slurp (io/resource "clojure-elisp/VERSION"))) "\n")
           out))))
