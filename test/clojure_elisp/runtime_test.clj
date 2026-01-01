(ns clojure-elisp.runtime-test
  "Tests for runtime Elisp functions via compilation.
   Verifies that Clojure core functions compile to the correct
   clel-* runtime function calls."
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-elisp.core :as clel]
            [clojure.string :as str]))

;;; into -> clel-into tests

(deftest emit-into-test
  (testing "into compiles to clel-into"
    (is (str/includes? (clel/emit '(into [] coll)) "clel-into"))
    (is (str/includes? (clel/emit '(into '() items)) "clel-into"))
    (is (str/includes? (clel/emit '(into {} pairs)) "clel-into")))

  (testing "into with various targets"
    ;; Vector target (emits as list in elisp)
    (let [code (clel/emit '(into [1 2] '(3 4)))]
      (is (str/includes? code "clel-into")))

    ;; List target  
    (let [code (clel/emit '(into '(1) [2 3]))]
      (is (str/includes? code "clel-into")))

    ;; Map target (emits as alist in elisp)
    (let [code (clel/emit '(into {:a 1} pairs))]
      (is (str/includes? code "clel-into"))))

  (testing "into nested in expression"
    (let [code (clel/emit '(let [result (into [] items)]
                             (count result)))]
      (is (str/includes? code "clel-into"))
      (is (str/includes? code "let"))))

  (testing "into preserves argument order"
    ;; First arg is target, second is source
    (let [code (clel/emit '(into target source))]
      (is (re-find #"clel-into\s+target\s+source" code)))))

;;; dissoc -> clel-dissoc tests

(deftest emit-dissoc-test
  (testing "dissoc compiles to clel-dissoc"
    (is (str/includes? (clel/emit '(dissoc {:a 1 :b 2} :a)) "clel-dissoc"))
    (is (str/includes? (clel/emit '(dissoc m :key)) "clel-dissoc")))

  (testing "dissoc with single key"
    (let [code (clel/emit '(dissoc {:a 1 :b 2} :a))]
      (is (str/includes? code "clel-dissoc"))
      (is (str/includes? code ":a"))))

  (testing "dissoc with nonexistent key (should still compile)"
    (let [code (clel/emit '(dissoc {:a 1} :nonexistent))]
      (is (str/includes? code "clel-dissoc"))
      (is (str/includes? code ":nonexistent"))))

  (testing "dissoc nested in expression"
    (let [code (clel/emit '(let [result (dissoc m :key)]
                             (get result :other)))]
      (is (str/includes? code "clel-dissoc"))
      (is (str/includes? code "let"))))

  (testing "dissoc preserves argument order"
    (let [code (clel/emit '(dissoc my-map my-key))]
      (is (re-find #"clel-dissoc\s+my-map\s+my-key" code)))))

