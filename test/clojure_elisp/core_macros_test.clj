(ns clojure-elisp.core-macros-test
  "clojure.core macros that name temporaries expand in the compiler, not the
   host, so their output is the same in every process and on every host."
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.string :as str]
            [clojure-elisp.core :as clel]))

(def ^:private source
  "(defn f [x xs]
     (list (cond-> x a inc)
           (cond->> x a (map inc))
           (some-> x :a :b)
           (some->> x (map inc))
           (as-> x $ (inc $))
           (doto x (put 1) run)
           (if-not x 1 2)
           (when-some [y (g x)] y)
           (if-some [y (g x)] y 0)
           (when-first [y xs] y)
           (condp = x 1 :a :b)))")

(deftest expansions-do-not-depend-on-the-process
  (let [before (clel/compile-string source)]
    (dotimes [_ 50] (gensym))
    (is (= before (clel/compile-string source))
        "host gensyms in between do not renumber anything")))

(deftest expansions-follow-clojure-core
  (let [out (clel/compile-string source)]
    (testing "temporaries are numbered per compilation"
      (is (str/includes? out "(G__1 x)"))
      (is (str/includes? out "temp__"))
      (is (not (re-find #"G__\d{3,}" out))))
    (testing "if-not calls not, as clojure.core/if-not does"
      (is (str/includes? out "(if (not x) 1 2)")))
    (testing "doto threads the object through every call"
      (is (re-find #"\(put (G__\d+) 1\)\s+\(run \1\)" out)))))

(deftest qualified-core-heads-use-the-compilers-analyzers
  (testing "a syntax-quoted when-let/cond (clojure.core/...) is analyzed by the
            compiler, not expanded by the host"
    (let [out (clel/compile-string-in-ns
               "(ns m)"
               "(defmacro w [a & b] `(when-let [x# ~a] (cond ~@b)))
                (defn u [] (w 1 true 2))")]
      (is (str/includes? out "(when-let ((x__"))
      (is (not (str/includes? out "temp__"))))))
