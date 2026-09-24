(ns clojure-elisp.docstring-newlines-test
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [clojure-elisp.core :as clel]))

(deftest docstrings-keep-real-line-breaks
  (let [out (clel/compile-string-in-ns
             "(ns demo)"
             "(defn f \"Return X.\nSecond line names Y.\" [x y] (+ x y))
              (defcustom demo-level 1 \"Level.\nMore about it.\" :type 'integer)
              (defvar demo-state nil \"State.\nLine two.\")")]
    (testing "continuation lines are real newlines, so checkdoc sees each word"
      (is (str/includes? out "\"Return X.\nSecond line names Y.\""))
      (is (str/includes? out "\"Level.\nMore about it.\""))
      (is (str/includes? out "\"State.\nLine two.\""))
      (is (not (str/includes? out "\\nSecond"))))))

(deftest docstrings-escape-quotes-and-backslashes
  (let [out (clel/compile-string-in-ns
             "(ns demo)"
             "(defn g \"Call `f' with \\\"x\\\" and \\\\(y).\" [] nil)")]
    (is (str/includes? out "\"Call `f' with \\\"x\\\" and \\\\(y).\""))))

(deftest ordinary-strings-are-unchanged
  (let [out (clel/compile-string-in-ns "(ns demo)" "(defn h [] (message \"a\nb\"))")]
    (is (str/includes? out "(message \"a\\nb\")"))))
