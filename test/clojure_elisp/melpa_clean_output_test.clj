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

(deftest autoload-metadata-emits-the-cookie-on-the-line-before
  (let [cookie-before? (fn [el head] (str/starts-with? el (str ";;;###autoload\n(" head)))]
    (testing "^:autoload on a defn name"
      (is (cookie-before? (clel/emit '(defn ^:autoload now "Show." [] (interactive) 1))
                          "defun now ()")))
    (testing ":autoload in a defn attr-map, after the docstring"
      (is (cookie-before? (clel/emit '(defn now "Show." {:autoload true} [] (interactive) 1))
                          "defun now ()")))
    (testing "multi-arity defn"
      (is (cookie-before? (clel/emit '(defn ^:autoload f "F." ([] 1) ([x] x))) "defun f ")))
    (testing "define-minor-mode: package-lint errors on an un-autoloaded global mode"
      (is (cookie-before? (clel/emit '(define-minor-mode ^:autoload tod-mode "Toggle." :global true))
                          "define-minor-mode tod-mode")))
    (testing "defcustom"
      (is (cookie-before? (clel/emit '(defcustom ^:autoload tod-x 1 "X." :type 'integer))
                          "defcustom tod-x 1"))))
  (testing "no metadata, no cookie; an attr-map without :autoload is just skipped"
    (is (= "(defun f (x)\n  \"F.\"\n  x)" (clel/emit '(defn f "F." {:added "1.0"} [x] x))))
    (is (not (str/includes? (clel/emit '(define-minor-mode m "M.")) "autoload"))))
  (testing "in a file, the cookie sits on its own line directly above the defun"
    (let [el (clel/compile-file-string "(ns my.pkg)\n(defn ^:autoload go \"Go.\" [] (interactive) 1)")]
      (is (str/includes? el "\n;;;###autoload\n(defun my-pkg-go ()")))))

(deftest variadic-defn-emits-its-real-arglist
  (let [el (clel/emit '(defn f "Return A and the list MORE." [a & more] (cons a more)))]
    (testing "the Clojure parameters, so help, eldoc and checkdoc see A and MORE"
      (is (str/starts-with? el "(defun f (a &rest more)\n")))
    (testing "the docstring is the first body form, not buried inside a let"
      (is (str/starts-with? el "(defun f (a &rest more)\n  \"Return A and the list MORE.\"\n")))
    (is (not (str/includes? el "clel--args")))))

(deftest multi-arity-defn-documents-its-signature
  (let [el (clel/emit '(defn span "Span from START to END." ([start] (- 24 start)) ([start end] (- end start))))]
    (testing "dispatch still needs (&rest clel--args)"
      (is (str/starts-with? el "(defun span (&rest clel--args)\n")))
    (testing "the docstring ends with the (fn ...) usage line help and eldoc
              read, written \\( since it opens a line"
      (is (str/includes? el "  \"Span from START to END.\n\n\\(fn START &optional END)\"\n")))
    (testing "checkdoc is told clel--args is not an argument to document"
      (is (str/includes? el "\n  ;; checkdoc-params: (clel--args)\n"))))
  (testing "optional and rest positions are named after the longest arity"
    (is (str/includes? (clel/emit '(defn v "V." ([] 0) ([x] x) ([x y & more] more)))
                       "\n\\(fn &optional X Y &rest MORE)\"")))
  (testing "a docstring that already carries a usage line keeps it"
    (let [el (clel/emit '(defn w "W.\n\n(fn THING)" ([a] a) ([a b] b)))]
      (is (= 1 (count (re-seq #"\(fn " el))))))
  (testing "no docstring: nothing to annotate, output unchanged"
    (is (not (str/includes? (clel/emit '(defn n ([a] a) ([a b] b))) "checkdoc")))))

(deftest docstrings-keep-their-lines
  (testing "checkdoc reads a docstring by line: its first line must be a
            sentence, which an escaped \\n hides"
    (is (= "(defun f ()\n  \"First line.\nSecond line.\"\n  1)"
           (clel/emit '(defn f "First line.\nSecond line." [] 1)))))
  (testing "Clojure indents continuation lines under the quote; Emacs shows
            them as written and checkdoc wants them flush left. The shared
            indentation goes, relative indentation stays."
    (is (= "(defun f ()\n  \"First.\nSecond.\n  Example.\n\nLast.\"\n  1)"
           (clel/emit '(defn f "First.\n   Second.\n     Example.\n\n   Last." [] 1)))))
  (testing "quotes and backslashes are escaped; a paren opening a line is \\("
    (is (= "(defvar x 1 \"Say \\\"hi\\\" \\\\ now.\n\\(not code)\")"
           (clel/emit '(def x "Say \"hi\" \\ now.\n(not code)" 1)))))
  (testing "Elisp style, a string opening a body that goes on is the docstring"
    (is (= "(defun v (&rest items)\n  \"Make a list of ITEMS.\"\n  items)"
           (clel/emit '(defn v [& items] "Make a list of ITEMS." items))))
    (is (= "(defun k ()\n  \"just a value\")" (clel/emit '(defn k [] "just a value"))))))

(deftest generated-parameters-are-exempt-from-checkdoc
  (testing "a destructured parameter without :as is named by the compiler"
    (let [el (clel/emit '(defn d "Describe the phase of a state." [{:keys [phase]}] phase))]
      (is (str/includes? el "(defun d (p__1)\n  \"Describe the phase of a state.\"\n  ;; checkdoc-params: (p__1)\n"))))
  (testing "one with :as is the user's name and must be documented"
    (is (not (str/includes? (clel/emit '(defn d "Use STATE." [{:keys [a] :as state}] a))
                            "checkdoc-params")))))

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
      (is (not (re-find #"\(require 'clojure-" el))))
    (testing "calls through the alias still compile to the runtime"
      (is (str/includes? el "(clel-str-join \",\" (clel-set-union xs xs))"))
      (is (str/includes? el "(clel-str-upper ")))
    (testing "a project namespace is still required, once, though named twice"
      (is (= 1 (count (re-seq #"\(require 'my-dep\)" el)))))))

(deftest clojure-core-names-resolve-or-are-refused
  (testing "syntax-quote writes clojure.core/vector for [...]: it resolves as
            the bare name does, to Elisp's `vector'"
    (is (= "(clel-apply #'vector xs)"
           (clel/emit '(clojure.core/apply clojure.core/vector xs))))
    (is (str/includes? (clel/compile-file-string "(defmacro pair [a b] `[~a ~b])")
                       "(clel-apply #'vector ")))
  (testing "an unmapped clojure.core name is refused while analyzing: the
            emitter used to write clojure-core-NAME, which nothing defines"
    (let [refusal (fn [src]
                    (try (clel/compile-file-string src) nil
                         (catch clojure.lang.ExceptionInfo e e)))
          e       (refusal "(defn f [x]\n  (clojure.core/frobnicate x))")]
      (is (= 'clojure.core/frobnicate (:symbol (ex-data e))))
      (is (= 2 (:line (ex-data e))) "the error points at the call")
      (testing "including one that a macro expanded on the JVM writes"
        (is (= 'clojure.core/push-thread-bindings
               (:symbol (ex-data (refusal "(defn p [x] (with-out-str x))")))))))))
