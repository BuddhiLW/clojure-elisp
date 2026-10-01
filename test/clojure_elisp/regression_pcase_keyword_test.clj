(ns clojure-elisp.regression-pcase-keyword-test
  "Keywords keep their identity through the emitter: pcase patterns and
   keyword constants, namespaced or not."
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [clojure.string :as str]
            [clojure-elisp.core :as clel]))

(deftest pcase-keyword-pattern-is-a-keyword
  (testing "a bare keyword arm stays a keyword, not a quoted symbol"
    (let [out (clel/emit '(pcase k (:a "A") (_ nil)))]
      (is (str/includes? out "(:a \"A\")"))
      (is (not (str/includes? out "'a")))))
  (testing "a namespaced keyword arm keeps its namespace"
    (let [out (clel/emit '(pcase k (:ns/b "B") (_ nil)))]
      (is (str/includes? out "(:ns/b \"B\")"))))
  (testing "keywords inside list patterns are untouched"
    (is (str/includes? (clel/emit '(pcase k ((or :a :ns/b) 1) (_ nil)))
                       "((or :a :ns/b) 1)")))
  (testing "symbol patterns still quote"
    (is (str/includes? (clel/emit '(pcase s (a 1) (_ nil))) "('a 1)"))))

(deftest keyword-constant-keeps-namespace
  (is (= ":ns/b" (clel/emit :ns/b)))
  (is (= ":a" (clel/emit :a)))
  (is (= "(foo :ns/b)" (clel/emit '(foo :ns/b)))))

(deftest case-on-namespaced-keyword-agrees
  (testing "the scrutinee constant and the case pattern spell the keyword alike"
    (let [out (clel/emit '(case (identity :ns/b) :ns/b 1 2))]
      (is (str/includes? out "(identity :ns/b)"))
      (is (str/includes? out "(:ns/b 1)")))))

(def ^:private gen-name
  (gen/fmap (fn [[c cs]] (apply str c cs))
            (gen/tuple gen/char-alpha (gen/vector gen/char-alphanumeric 0 6))))

(def ^:private gen-kw
  (gen/one-of [(gen/fmap keyword gen-name)
               (gen/fmap (fn [[n s]] (keyword n s)) (gen/tuple gen-name gen-name))]))

(defspec keyword-constant-roundtrips 100
  (prop/for-all [kw gen-kw]
    (= (str kw) (clel/emit kw))))

(defspec pcase-keyword-arm-emits-the-keyword 100
  (prop/for-all [kw gen-kw]
    (str/includes? (clel/emit (list 'pcase 'k (list kw 1) '(_ nil)))
                   (str "(" kw " 1)"))))
