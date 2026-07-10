(ns clojure-elisp.regression-empty-list-test
  "Regression [CLJEL-EMPTY-LIST]: the empty-list literal () must emit as `nil`
   (Elisp's empty list), never `(nil)` — a one-element list calling the fn nil,
   which breaks empty arglists like (ert-deftest name () ...) on Emacs 31."
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-elisp.core :as clel]
            [clojure-elisp.analyzer :as ana]
            [clojure-elisp.emitter :as emit]
            [clojure.string :as str]))

(defn- emit-form [form] (-> form ana/analyze emit/emit))

(deftest empty-list-emits-nil
  (testing "bare () is the empty-list literal, not (nil)"
    (is (= "nil" (emit-form ()))))
  (testing "quoted empty list '() also emits nil"
    (is (= "nil" (emit-form '()))))
  (testing "let-bound () binds nil, not (nil)"
    (is (str/includes? (emit-form '(let [x ()] x)) "(x nil)"))
    (is (not (str/includes? (emit-form '(let [x ()] x)) "(nil)")))))

(deftest empty-arglist-passthrough-emits-nil
  (testing "(ert-deftest name () ...) empty arglist emits nil — Emacs 31 rejects (nil)"
    (let [out (clel/compile-file-string
               "(ns t)\n(ert-deftest foo () \"d\" (should t))")]
      (is (str/includes? out "(ert-deftest foo nil "))
      (is (not (str/includes? out "(ert-deftest foo (nil)"))))))

(deftest non-empty-invoke-unaffected
  (testing "non-empty invocations and explicit (list) are unchanged"
    (is (= "(+ 1 2)" (emit-form '(+ 1 2))))
    (is (= "(list)" (emit-form '(list))))))
