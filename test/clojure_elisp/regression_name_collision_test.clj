(ns clojure-elisp.regression-name-collision-test
  "Emacs has one global namespace for functions and one for variables. Two
   definitions that mangle to the same Emacs name would silently replace
   each other, and two namespaces that mangle to the same file name would
   overwrite each other's .el: both are compile errors."
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.string :as str]
            [clojure-elisp.compile :as cc]
            [clojure-elisp.core :as clel]
            [clojure-elisp.fs-stub-test :refer [stub-fs]]
            [clojure-elisp.project :as project]))

(defn- collision-message
  "The message compile-project throws for a project of {path source}, or nil."
  [path->source]
  (try
    (project/compile-project (stub-fs (atom path->source)) ["/virt/src"] "/virt/out")
    nil
    (catch clojure.lang.ExceptionInfo e
      (ex-message e))))

(deftest namespaces-writing-one-file
  (let [msg (collision-message {"/virt/src/a/b_c.cljel" "(ns a.b-c)\n(defn f [] 1)"
                                "/virt/src/a_b/c.cljel" "(ns a-b.c)\n(defn g [] 2)"})]
    (is (some? msg))
    (is (str/includes? msg "a-b-c.el"))
    (is (str/includes? msg "a.b-c"))
    (is (str/includes? msg "a-b.c"))))

(deftest definitions-mangling-to-one-function
  (testing "across namespaces: tod/moment-at and tod.moment/at"
    (let [msg (collision-message {"/virt/src/tod.cljel"        "(ns tod)\n(defn moment-at [] 1)"
                                  "/virt/src/tod/moment.cljel" "(ns tod.moment)\n(defn at [] 2)"})]
      (is (some? msg))
      (is (str/includes? msg "tod-moment-at"))
      (is (str/includes? msg "tod/moment-at"))
      (is (str/includes? msg "tod.moment/at"))))
  (testing "in one file: foo? and foo-p"
    (is (thrown-with-msg? clojure.lang.ExceptionInfo #"app-foo-p"
                          (clel/compile-file-string "(ns app)\n(defn foo? [] 1)\n(defn foo-p [] 2)"))))
  (testing "a public -helper and a private helper are both app--helper"
    (is (thrown-with-msg? clojure.lang.ExceptionInfo #"app--helper"
                          (clel/compile-file-string "(ns app)\n(defn -helper [] 1)\n(defn- helper [] 2)")))))

(deftest a-variable-and-a-defcustom
  (is (thrown-with-msg? clojure.lang.ExceptionInfo #"app-rules"
                        (clel/compile-file-string
                         "(ns app)\n(def rules 1)\n(defcustom app-rules nil \"Rules.\" :type 'sexp)"))))

(deftest what-does-not-collide
  (testing "a function and a variable may share an Emacs name"
    (is (empty? (cc/name-collisions
                 {"a.cljel" "(ns app)\n(defn x [] 1)\n(defcustom app-x nil \"X.\" :type 'sexp)"}))))
  (testing "a definition repeated under the same name is a redefinition"
    (is (empty? (cc/name-collisions {"a.cljel" "(ns app)\n(defonce x 1)\n(def x 2)"}))))
  (testing "macros: their calls expand at compile time, file by file"
    (is (empty? (cc/name-collisions {"a.cljel" "(ns a)\n(defmacro dbg [x] x)"
                                     "b.cljel" "(ns b)\n(defmacro dbg [x] x)"}))))
  (testing "a clean project compiles"
    (is (nil? (collision-message {"/virt/src/tod.cljel"        "(ns tod)\n(defn current-moment [] 1)"
                                  "/virt/src/tod/moment.cljel" "(ns tod.moment)\n(defn at [] 2)"})))))
