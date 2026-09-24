(ns clojure-elisp.melpa-clean-output-test
  "Emitted definitions that MELPA's tools (byte-compile, checkdoc,
   package-lint, melpazoid) accept without a diagnostic.

   These pin the emitted STRINGS. What the strings do once Emacs reads them
   (the byte-compiler's verdict, the docstring and arglist `help' sees, the
   autoloads loaddefs generates) is asserted in
   test/elisp/clojure-elisp-melpa-test.el."
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [clojure-elisp.core :as clel]))

(deftest when-let-and-if-let-emit-the-starred-macros
  (testing "unstarred when-let / if-let are obsolete since Emacs 31.1 and
            the byte-compiler warns on each use"
    (let [el (clel/emit '(when-let [x (f)] (g x)))]
      (is (str/starts-with? el "(when-let* ((x (f)))"))
      (is (not (str/includes? el "(when-let ("))))
    (let [el (clel/emit '(if-let [x (f)] (g x) :none))]
      (is (str/starts-with? el "(if-let* ((x (f)))"))
      (is (str/includes? el ":none")))
    (is (str/starts-with? (clel/emit '(if-let [x (f)] x)) "(if-let* ((x (f)))"))))

(deftest clojure-namespaces-are-not-required-but-their-aliases-resolve
  (let [el (clel/compile-file-string
            "(ns my.app
               (:require [clojure.string :as str]
                         [clojure.set :as set]
                         clojure.walk
                         [my.dep :as d]
                         [my.dep :refer [helper]]))
             (defn f [xs] (str/join \",\" (set/union xs xs)))
             (defn g [s] (clojure.string/upper-case (d/h (helper s))))")]
    (testing "no Emacs feature named clojure-* exists, so requiring one fails
              with \"Cannot open load file\""
      (is (not (re-find #"\(require 'clojure-(?!elisp-runtime)" el))))
    (testing "calls through the alias still compile to the runtime"
      (is (str/includes? el "(clel-str-join \",\" (clel-set-union xs xs))"))
      (is (str/includes? el "(clel-str-upper ")))
    (testing "a project namespace is still required, once, though named twice"
      (is (= 1 (count (re-seq #"\(require 'my-dep\)" el)))))))
