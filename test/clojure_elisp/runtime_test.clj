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

;;; Collection predicates -> clel-*-p tests

(deftest collection-predicates-test
  (testing "coll? compiles to clel-coll-p"
    (is (str/includes? (clel/emit '(coll? x)) "clel-coll-p"))
    (is (str/includes? (clel/emit '(coll? [1 2 3])) "clel-coll-p"))
    (is (str/includes? (clel/emit '(coll? {:a 1})) "clel-coll-p")))

  (testing "sequential? compiles to clel-sequential-p"
    (is (str/includes? (clel/emit '(sequential? x)) "clel-sequential-p"))
    (is (str/includes? (clel/emit '(sequential? [1 2])) "clel-sequential-p"))
    (is (str/includes? (clel/emit '(sequential? '(1 2))) "clel-sequential-p")))

  (testing "associative? compiles to clel-associative-p"
    (is (str/includes? (clel/emit '(associative? x)) "clel-associative-p"))
    (is (str/includes? (clel/emit '(associative? {:a 1})) "clel-associative-p"))
    (is (str/includes? (clel/emit '(associative? '((:a . 1)))) "clel-associative-p")))

  (testing "collection predicates in conditional"
    (let [code (clel/emit '(if (coll? x) (count x) 0))]
      (is (str/includes? code "clel-coll-p"))
      (is (str/includes? code "if"))))

  (testing "collection predicates preserve argument"
    (let [code (clel/emit '(coll? my-data))]
      (is (re-find #"clel-coll-p\s+my-data" code)))))

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

;;; Numeric predicates tests

(deftest numeric-predicates-test
  (testing "zero?"
    (is (= "(zerop 0)" (clel/emit '(zero? 0))))
    (is (= "(zerop x)" (clel/emit '(zero? x)))))

  (testing "pos?"
    (is (= "(cl-plusp 1)" (clel/emit '(pos? 1))))
    (is (= "(cl-plusp n)" (clel/emit '(pos? n)))))

  (testing "neg?"
    (is (= "(cl-minusp -1)" (clel/emit '(neg? -1))))
    (is (= "(cl-minusp x)" (clel/emit '(neg? x)))))

  (testing "even?"
    (is (= "(cl-evenp 4)" (clel/emit '(even? 4))))
    (is (= "(cl-evenp num)" (clel/emit '(even? num)))))

  (testing "odd?"
    (is (= "(cl-oddp 3)" (clel/emit '(odd? 3))))
    (is (= "(cl-oddp n)" (clel/emit '(odd? n)))))

  (testing "predicates in expressions"
    (let [code (clel/emit '(if (zero? x) "zero" "not zero"))]
      (is (str/includes? code "zerop"))
      (is (str/includes? code "if")))
    (let [code (clel/emit '(when (even? n) (println "even")))]
      (is (str/includes? code "cl-evenp")))))

;;; Boolean/nil predicates tests

(deftest boolean-predicates-test
  (testing "some? compiles to clel-some-p"
    (is (str/includes? (clel/emit '(some? x)) "clel-some-p"))
    (is (str/includes? (clel/emit '(some? nil)) "clel-some-p"))
    (is (str/includes? (clel/emit '(some? "hello")) "clel-some-p")))

  (testing "true? compiles to clel-true-p"
    (is (str/includes? (clel/emit '(true? x)) "clel-true-p"))
    (is (str/includes? (clel/emit '(true? true)) "clel-true-p")))

  (testing "false? compiles to clel-false-p"
    (is (str/includes? (clel/emit '(false? x)) "clel-false-p"))
    (is (str/includes? (clel/emit '(false? nil)) "clel-false-p"))
    (is (str/includes? (clel/emit '(false? false)) "clel-false-p")))

  (testing "boolean predicates in conditional"
    (let [code (clel/emit '(if (some? result) result :default))]
      (is (str/includes? code "clel-some-p"))
      (is (str/includes? code "if"))))

  (testing "boolean predicates preserve argument"
    (let [code (clel/emit '(some? my-value))]
      (is (re-find #"clel-some-p\s+my-value" code)))))

