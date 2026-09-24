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
