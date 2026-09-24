(ns clojure-elisp.layout-test
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [clojure-elisp.core :as clel]
            [clojure-elisp.emitter :as emit]
            [clojure-elisp.layout :as layout]))

(defn- tokens
  "The token sequence of Elisp text s, without the whitespace between."
  [s]
  (mapv (juxt :t :s) (#'layout/tokenize s)))

(deftest a-form-that-fits-stays-on-one-line
  (is (= "(when x (f x))" (layout/layout-code "(when x\n    (f x))")))
  (is (= "(defvar v 1)" (layout/layout-code "(defvar v 1)"))))

(deftest defining-forms-put-their-body-under-the-first-line
  (is (= "(defun f (x)\n  \"Return X.\"\n  x)" (layout/layout-code "(defun f (x) \"Return X.\" x)")))
  (is (= "(defvar v 1\n  \"V.\")" (layout/layout-code "(defvar v 1 \"V.\")"))))

(deftest a-long-call-aligns-its-arguments-under-the-first
  (is (= (str "(message \"%s %s %s\"\n"
              "         (some-function-name argument-one)\n"
              "         (some-function-name argument-one)\n"
              "         (some-function-name argument-one))")
         (layout/layout-code
          (str "(message \"%s %s %s\" "
               (str/join " " (repeat 3 "(some-function-name argument-one)")) ")")))))

(deftest if-indents-its-then-branch-by-four-and-else-by-two
  (is (= (str "(if (and some-long-condition another-long-condition)\n"
              "    (then-branch-function argument-one argument-two)\n"
              "  (else-branch-function argument))")
         (layout/layout-code
          (str "(if (and some-long-condition another-long-condition) "
               "(then-branch-function argument-one argument-two) "
               "(else-branch-function argument))")))))

(deftest a-keyword-shares-a-line-with-its-value
  (is (= "(defcustom tod-x 1\n  \"X.\"\n  :type 'integer\n  :group 'tod)"
         (layout/layout-code "(defcustom tod-x 1 \"X.\" :type 'integer :group 'tod)"))))

(deftest strings-and-comments-are-kept
  (let [src "(defun f (p__1)\n  \"First.\nSecond.\"\n  ;; checkdoc-params: (p__1)\n  p__1)"]
    (is (= src (layout/layout-code src))))
  (testing "a comment that ends a list sends the closing paren to a new line"
    (is (= "(progn\n  (f)\n  ;; done\n )" (layout/layout-code "(progn (f) ;; done\n)"))))
  (testing "character literals and reader prefixes are single tokens"
    (let [src "(list ?\\( ?a #'car `(,x ,@xs) [1 2])"]
      (is (= src (layout/layout-code src)))))
  (testing "comments and blank lines between top-level forms stay"
    (let [src ";;; x.el --- X  -*- lexical-binding: t; -*-\n\n;;;###autoload\n(defvar x 1)\n"]
      (is (= src (layout/layout-code src))))))

(deftest unbalanced-text-is-returned-as-it-is
  (is (= "(a (b" (layout/layout-code "(a (b")))
  (is (= "a) (b" (layout/layout-code "a) (b"))))

(def ^:private corpus-sources
  ["resources/clojure-elisp/runtime.cljel"
   "test/parity/kitchen_sink.cljel"
   "test/parity/kitchen_sink_forms.cljel"
   "test/elisp/sources/semantics.cljel"])

(defn- raw-emitted
  "Source file compiled as the emitter writes it, before layout."
  [path]
  (binding [emit/*layout* false]
    (clel/compile-file-string (slurp path))))

(deftest layout-keeps-every-token-and-shortens-lines
  (doseq [path corpus-sources]
    (let [raw  (raw-emitted path)
          out  (layout/layout-code raw)
          long (fn [s] (count (filter #(> (count %) layout/width) (str/split-lines s))))]
      (testing path
        (is (= (tokens raw) (tokens out)) "the same forms, token for token")
        (is (= out (layout/layout-code out)) "laying out twice changes nothing")
        (is (< (long out) (max 1 (long raw))) "fewer lines past the width")))))
