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

;;; Atom watch functions -> clel-add-watch / clel-remove-watch tests

(deftest atom-add-watch-test
  (testing "add-watch compiles to clel-add-watch"
    (is (str/includes? (clel/emit '(add-watch a :key f)) "clel-add-watch"))
    (is (str/includes? (clel/emit '(add-watch my-atom :watcher handler-fn)) "clel-add-watch")))

  (testing "add-watch with inline fn"
    (let [code (clel/emit '(add-watch a :key (fn [k r o n] (println o n))))]
      (is (str/includes? code "clel-add-watch"))
      (is (str/includes? code "lambda"))))

  (testing "add-watch with keyword key"
    (let [code (clel/emit '(add-watch my-atom :my-watcher callback))]
      (is (str/includes? code "clel-add-watch"))
      (is (str/includes? code ":my-watcher"))))

  (testing "add-watch preserves argument order"
    (let [code (clel/emit '(add-watch my-atom my-key my-fn))]
      (is (re-find #"clel-add-watch\s+my-atom\s+my-key\s+my-fn" code))))

  (testing "add-watch in let binding"
    (let [code (clel/emit '(let [result (add-watch a :k f)]
                             result))]
      (is (str/includes? code "clel-add-watch"))
      (is (str/includes? code "let")))))

(deftest atom-remove-watch-test
  (testing "remove-watch compiles to clel-remove-watch"
    (is (str/includes? (clel/emit '(remove-watch a :key)) "clel-remove-watch"))
    (is (str/includes? (clel/emit '(remove-watch my-atom :watcher)) "clel-remove-watch")))

  (testing "remove-watch with keyword key"
    (let [code (clel/emit '(remove-watch my-atom :my-watcher))]
      (is (str/includes? code "clel-remove-watch"))
      (is (str/includes? code ":my-watcher"))))

  (testing "remove-watch preserves argument order"
    (let [code (clel/emit '(remove-watch my-atom my-key))]
      (is (re-find #"clel-remove-watch\s+my-atom\s+my-key" code))))

  (testing "remove-watch in expression context"
    (let [code (clel/emit '(do
                             (remove-watch a :k1)
                             (remove-watch a :k2)))]
      (is (str/includes? code "clel-remove-watch"))
      (is (str/includes? code "progn")))))

(deftest atom-watch-with-atom-operations-test
  (testing "watch with atom creation"
    (let [code (clel/emit '(let [a (atom 0)]
                             (add-watch a :logger
                                        (fn [k r o n]
                                          (println "changed from" o "to" n)))
                             a))]
      (is (str/includes? code "clel-atom"))
      (is (str/includes? code "clel-add-watch"))))

  (testing "watch with reset!"
    (let [code (clel/emit '(do
                             (add-watch a :w (fn [k r o n] nil))
                             (reset! a 42)))]
      (is (str/includes? code "clel-add-watch"))
      (is (str/includes? code "clel-reset!"))))

  (testing "watch with swap!"
    (let [code (clel/emit '(do
                             (add-watch a :w callback)
                             (swap! a inc)))]
      (is (str/includes? code "clel-add-watch"))
      (is (str/includes? code "clel-swap!"))))

  (testing "multiple watchers on same atom"
    (let [code (clel/emit '(do
                             (add-watch a :w1 f1)
                             (add-watch a :w2 f2)
                             (add-watch a :w3 f3)))]
      (is (= 3 (count (re-seq #"clel-add-watch" code))))))

  (testing "add and remove watch sequence"
    (let [code (clel/emit '(do
                             (add-watch a :temp temp-fn)
                             (reset! a 1)
                             (remove-watch a :temp)))]
      (is (str/includes? code "clel-add-watch"))
      (is (str/includes? code "clel-remove-watch"))
      (is (str/includes? code "clel-reset!")))))

(deftest atom-watch-edge-cases-test
  (testing "add-watch returns the atom (for chaining)"
    ;; Typical pattern: (-> a (add-watch :k f) (reset! 0))
    (let [code (clel/emit '(reset! (add-watch a :k f) 0))]
      (is (str/includes? code "clel-add-watch"))
      (is (str/includes? code "clel-reset!"))))

  (testing "same key twice replaces watcher (valid code pattern)"
    (let [code (clel/emit '(do
                             (add-watch a :k old-fn)
                             (add-watch a :k new-fn)))]
      (is (= 2 (count (re-seq #"clel-add-watch" code))))))

  (testing "remove non-existent key (should still emit valid code)"
    (let [code (clel/emit '(remove-watch a :nonexistent))]
      (is (str/includes? code "clel-remove-watch"))
      (is (str/includes? code ":nonexistent"))))

  (testing "watcher fn receives 4 args pattern"
    ;; Verify lambda with 4 params is emitted correctly
    (let [code (clel/emit '(add-watch a :k
                                      (fn [key ref old-val new-val]
                                        (when (not= old-val new-val)
                                          (println "changed")))))]
      (is (str/includes? code "lambda"))
      (is (str/includes? code "key"))
      (is (str/includes? code "ref"))
      (is (str/includes? code "old-val"))
      (is (str/includes? code "new-val"))))

  (testing "empty atom with watchers"
    (let [code (clel/emit '(let [a (atom nil)]
                             (add-watch a :k f)
                             @a))]
      (is (str/includes? code "clel-atom"))
      (is (str/includes? code "clel-add-watch"))
      (is (str/includes? code "clel-deref")))))

;;; Sequence abstraction tests (clel-029)

;; --- Lazy-seq-aware first/rest/next ---

(deftest seq-first-compilation-test
  (testing "first compiles to clel-first"
    (is (str/includes? (clel/emit '(first xs)) "clel-first"))
    (is (str/includes? (clel/emit '(first [1 2 3])) "clel-first")))

  (testing "first in let binding"
    (let [code (clel/emit '(let [x (first coll)] x))]
      (is (str/includes? code "clel-first"))))

  (testing "first preserves argument"
    (is (re-find #"clel-first\s+my-seq" (clel/emit '(first my-seq))))))

(deftest seq-rest-compilation-test
  (testing "rest compiles to clel-rest"
    (is (str/includes? (clel/emit '(rest xs)) "clel-rest"))
    (is (str/includes? (clel/emit '(rest [1 2 3])) "clel-rest")))

  (testing "rest preserves argument"
    (is (re-find #"clel-rest\s+my-seq" (clel/emit '(rest my-seq))))))

(deftest seq-next-compilation-test
  (testing "next compiles to clel-next"
    (is (str/includes? (clel/emit '(next xs)) "clel-next")))

  (testing "next preserves argument"
    (is (re-find #"clel-next\s+items" (clel/emit '(next items))))))

;; --- Lazy sequence functions ---

(deftest seq-map-compilation-test
  (testing "map compiles to clel-map"
    (is (str/includes? (clel/emit '(map inc xs)) "clel-map"))
    (is (str/includes? (clel/emit '(map f coll)) "clel-map")))

  (testing "map with inline fn"
    (let [code (clel/emit '(map (fn [x] (+ x 1)) xs))]
      (is (str/includes? code "clel-map"))
      (is (str/includes? code "lambda"))))

  (testing "map preserves argument order"
    (is (re-find #"clel-map\s+f\s+coll" (clel/emit '(map f coll))))))

(deftest seq-filter-compilation-test
  (testing "filter compiles to clel-filter"
    (is (str/includes? (clel/emit '(filter even? xs)) "clel-filter"))
    (is (str/includes? (clel/emit '(filter pred coll)) "clel-filter")))

  (testing "filter preserves argument order"
    (is (re-find #"clel-filter\s+pred\s+items" (clel/emit '(filter pred items))))))

(deftest seq-take-compilation-test
  (testing "take compiles to clel-take"
    (is (str/includes? (clel/emit '(take 5 xs)) "clel-take")))

  (testing "take preserves argument order"
    (is (re-find #"clel-take\s+n\s+coll" (clel/emit '(take n coll))))))

(deftest seq-drop-compilation-test
  (testing "drop compiles to clel-drop"
    (is (str/includes? (clel/emit '(drop 3 xs)) "clel-drop")))

  (testing "drop preserves argument order"
    (is (re-find #"clel-drop\s+n\s+coll" (clel/emit '(drop n coll))))))

(deftest seq-take-while-compilation-test
  (testing "take-while compiles to clel-take-while"
    (is (str/includes? (clel/emit '(take-while pos? xs)) "clel-take-while")))

  (testing "take-while preserves argument order"
    (is (re-find #"clel-take-while\s+pred\s+coll" (clel/emit '(take-while pred coll))))))

(deftest seq-drop-while-compilation-test
  (testing "drop-while compiles to clel-drop-while"
    (is (str/includes? (clel/emit '(drop-while neg? xs)) "clel-drop-while")))

  (testing "drop-while preserves argument order"
    (is (re-find #"clel-drop-while\s+pred\s+items" (clel/emit '(drop-while pred items))))))

(deftest seq-concat-compilation-test
  (testing "concat compiles to clel-concat"
    (is (str/includes? (clel/emit '(concat xs ys)) "clel-concat")))

  (testing "concat with multiple colls"
    (is (str/includes? (clel/emit '(concat a b c)) "clel-concat"))))

(deftest seq-mapcat-compilation-test
  (testing "mapcat compiles to clel-mapcat"
    (is (str/includes? (clel/emit '(mapcat f xs)) "clel-mapcat")))

  (testing "mapcat preserves argument order"
    (is (re-find #"clel-mapcat\s+f\s+coll" (clel/emit '(mapcat f coll))))))

(deftest seq-interleave-compilation-test
  (testing "interleave compiles to clel-interleave"
    (is (str/includes? (clel/emit '(interleave xs ys)) "clel-interleave")))

  (testing "interleave preserves args"
    (is (re-find #"clel-interleave\s+a\s+b" (clel/emit '(interleave a b))))))

(deftest seq-partition-compilation-test
  (testing "partition compiles to clel-partition"
    (is (str/includes? (clel/emit '(partition 3 xs)) "clel-partition")))

  (testing "partition preserves argument order"
    (is (re-find #"clel-partition\s+n\s+coll" (clel/emit '(partition n coll))))))

;; --- Eager sequence functions ---

(deftest seq-reduce-compilation-test
  (testing "reduce compiles to clel-reduce"
    (is (str/includes? (clel/emit '(reduce + xs)) "clel-reduce"))
    (is (str/includes? (clel/emit '(reduce + 0 xs)) "clel-reduce")))

  (testing "reduce preserves argument order (no init)"
    (is (re-find #"clel-reduce\s+\+\s+coll" (clel/emit '(reduce + coll)))))

  (testing "reduce preserves argument order (with init)"
    (is (re-find #"clel-reduce\s+\+\s+0\s+coll" (clel/emit '(reduce + 0 coll))))))

(deftest seq-sort-compilation-test
  (testing "sort compiles to clel-sort"
    (is (str/includes? (clel/emit '(sort < xs)) "clel-sort")))

  (testing "sort preserves argument order"
    (is (re-find #"clel-sort\s+cmp\s+coll" (clel/emit '(sort cmp coll))))))

(deftest seq-sort-by-compilation-test
  (testing "sort-by compiles to clel-sort-by"
    (is (str/includes? (clel/emit '(sort-by :name users)) "clel-sort-by")))

  (testing "sort-by preserves argument order"
    (is (re-find #"clel-sort-by\s+keyfn\s+coll" (clel/emit '(sort-by keyfn coll))))))

(deftest seq-group-by-compilation-test
  (testing "group-by compiles to clel-group-by"
    (is (str/includes? (clel/emit '(group-by :type items)) "clel-group-by")))

  (testing "group-by preserves argument order"
    (is (re-find #"clel-group-by\s+f\s+coll" (clel/emit '(group-by f coll))))))

(deftest seq-frequencies-compilation-test
  (testing "frequencies compiles to clel-frequencies"
    (is (str/includes? (clel/emit '(frequencies xs)) "clel-frequencies")))

  (testing "frequencies preserves argument"
    (is (re-find #"clel-frequencies\s+items" (clel/emit '(frequencies items))))))

;; --- Sequence predicates ---

(deftest seq-every-p-compilation-test
  (testing "every? compiles to clel-every-p"
    (is (str/includes? (clel/emit '(every? pos? xs)) "clel-every-p")))

  (testing "every? preserves argument order"
    (is (re-find #"clel-every-p\s+pred\s+coll" (clel/emit '(every? pred coll))))))

(deftest seq-some-compilation-test
  (testing "some compiles to clel-some"
    (is (str/includes? (clel/emit '(some pos? xs)) "clel-some")))

  (testing "some preserves argument order"
    (is (re-find #"clel-some\s+pred\s+coll" (clel/emit '(some pred coll))))))

(deftest seq-not-every-p-compilation-test
  (testing "not-every? compiles to clel-not-every-p"
    (is (str/includes? (clel/emit '(not-every? even? xs)) "clel-not-every-p")))

  (testing "not-every? preserves argument order"
    (is (re-find #"clel-not-every-p\s+pred\s+coll" (clel/emit '(not-every? pred coll))))))

(deftest seq-not-any-p-compilation-test
  (testing "not-any? compiles to clel-not-any-p"
    (is (str/includes? (clel/emit '(not-any? neg? xs)) "clel-not-any-p")))

  (testing "not-any? preserves argument order"
    (is (re-find #"clel-not-any-p\s+pred\s+coll" (clel/emit '(not-any? pred coll))))))

(deftest seq-empty-p-compilation-test
  (testing "empty? compiles to clel-empty-p"
    (is (str/includes? (clel/emit '(empty? xs)) "clel-empty-p")))

  (testing "empty? in conditional"
    (let [code (clel/emit '(if (empty? coll) :none (first coll)))]
      (is (str/includes? code "clel-empty-p"))
      (is (str/includes? code "if")))))

;; --- Integration: composing seq functions ---

(deftest seq-composition-compilation-test
  (testing "map + filter composition"
    (let [code (clel/emit '(filter even? (map inc xs)))]
      (is (str/includes? code "clel-filter"))
      (is (str/includes? code "clel-map"))))

  (testing "reduce over map"
    (let [code (clel/emit '(reduce + 0 (map inc xs)))]
      (is (str/includes? code "clel-reduce"))
      (is (str/includes? code "clel-map"))))

  (testing "take from filtered"
    (let [code (clel/emit '(take 5 (filter pos? xs)))]
      (is (str/includes? code "clel-take"))
      (is (str/includes? code "clel-filter"))))

  (testing "into with map"
    (let [code (clel/emit '(into [] (map inc xs)))]
      (is (str/includes? code "clel-into"))
      (is (str/includes? code "clel-map")))))

;;; Lazy sequence runtime function tests (clel-018)

(deftest lazy-seq-compilation-test
  (testing "lazy-seq compiles to clel-lazy-seq-create with lambda"
    (let [code (clel/emit '(lazy-seq (cons 1 nil)))]
      (is (str/includes? code "clel-lazy-seq-create"))
      (is (str/includes? code "lambda"))))

  (testing "lazy-seq with function call body"
    (let [code (clel/emit '(lazy-seq (map inc xs)))]
      (is (str/includes? code "clel-lazy-seq-create"))
      (is (str/includes? code "lambda"))))

  (testing "lazy-seq in defn"
    (let [code (clel/emit '(defn lazy-range [n]
                             (lazy-seq (cons n (lazy-range (inc n))))))]
      (is (str/includes? code "defun"))
      (is (str/includes? code "clel-lazy-seq-create"))
      (is (str/includes? code "lambda")))))

(deftest realized-p-compilation-test
  (testing "realized? compiles to clel-realized-p"
    (is (str/includes? (clel/emit '(realized? x)) "clel-realized-p")))

  (testing "realized? in conditional"
    (let [code (clel/emit '(if (realized? lseq) (first lseq) :pending))]
      (is (str/includes? code "clel-realized-p"))
      (is (str/includes? code "if")))))

(deftest doall-compilation-test
  (testing "doall compiles to clel-doall"
    (is (str/includes? (clel/emit '(doall xs)) "clel-doall")))

  (testing "doall in let binding"
    (let [code (clel/emit '(let [result (doall (lazy-seq (cons 1 nil)))]
                             result))]
      (is (str/includes? code "clel-doall"))
      (is (str/includes? code "clel-lazy-seq-create")))))

(deftest dorun-compilation-test
  (testing "dorun compiles to clel-dorun"
    (is (str/includes? (clel/emit '(dorun xs)) "clel-dorun")))

  (testing "dorun returns nil semantically"
    (let [code (clel/emit '(dorun (lazy-seq (cons 1 nil))))]
      (is (str/includes? code "clel-dorun"))
      (is (str/includes? code "clel-lazy-seq-create")))))

;;; Protocol + Record end-to-end compilation tests (clel-025)

(deftest protocol-record-compilation-test
  (testing "defprotocol + defrecord end-to-end"
    (let [code (clel/emit-forms
                '[(defprotocol IGreeter
                    (greet [this name]))
                  (defrecord Person [first-name last-name]
                    IGreeter
                    (greet [this name] (str "Hello " name)))])]
      (is (str/includes? code "cl-defgeneric"))
      (is (str/includes? code "cl-defstruct"))
      (is (str/includes? code "cl-defmethod"))
      (is (str/includes? code "->Person"))
      (is (str/includes? code "map->Person"))))

  (testing "defprotocol with multiple methods end-to-end"
    (let [code (clel/emit-forms
                '[(defprotocol IShape
                    (area [this])
                    (perimeter [this]))
                  (defrecord Circle [radius]
                    IShape
                    (area [this] (* 3.14 radius radius))
                    (perimeter [this] (* 2 3.14 radius)))])]
      (is (= 2 (count (re-seq #"cl-defgeneric" code))))
      (is (= 2 (count (re-seq #"cl-defmethod" code))))))

  (testing "defrecord without protocol"
    (let [code (clel/emit '(defrecord Point [x y]))]
      (is (str/includes? code "cl-defstruct"))
      (is (str/includes? code "->Point"))
      (is (str/includes? code "map->Point"))
      (is (not (str/includes? code "cl-defmethod"))))))

(deftest deftype-compilation-test
  (testing "deftype end-to-end"
    (let [code (clel/emit '(deftype Counter [^:mutable count]))]
      (is (str/includes? code "cl-defstruct"))
      (is (str/includes? code "->Counter"))
      (is (not (str/includes? code "map->Counter")))))

  (testing "deftype with set! in method"
    (let [code (clel/emit-forms
                '[(defprotocol ICounter
                    (increment [this])
                    (get-count [this]))
                  (deftype Counter [^:mutable count]
                    ICounter
                    (increment [this] (set! count (inc count)))
                    (get-count [this] count))])]
      (is (str/includes? code "cl-defgeneric"))
      (is (str/includes? code "cl-defstruct"))
      (is (str/includes? code "cl-defmethod"))
      (is (str/includes? code "setf")))))

;; ============================================================================
;; extend-type and extend-protocol (clel-025)
;; ============================================================================

(deftest extend-type-compilation-test
  (testing "extend-type compiles to cl-defmethod with type specializer"
    (let [code (clel/emit-forms
                '[(defprotocol IShow
                    (show [this]))
                  (extend-type String
                    IShow
                    (show [this] this))])]
      (is (str/includes? code "cl-defgeneric show"))
      (is (str/includes? code "cl-defmethod show ((this string))"))))

  (testing "extend-type with custom record type"
    (let [code (clel/emit-forms
                '[(defprotocol IShow
                    (show [this]))
                  (defrecord Person [name])
                  (extend-type Person
                    IShow
                    (show [this] name))])]
      (is (str/includes? code "cl-defmethod show ((this Person))")))))

(deftest extend-protocol-compilation-test
  (testing "extend-protocol compiles to multiple cl-defmethods"
    (let [code (clel/emit-forms
                '[(defprotocol IGreeter
                    (greet [this]))
                  (extend-protocol IGreeter
                    String
                    (greet [this] this)
                    Number
                    (greet [this] this))])]
      (is (str/includes? code "cl-defgeneric greet"))
      (is (str/includes? code "cl-defmethod greet ((this string))"))
      (is (str/includes? code "cl-defmethod greet ((this number))")))))

;; ============================================================================
;; satisfies? (clel-025)
;; ============================================================================

(deftest satisfies?-compilation-test
  (testing "satisfies? compiles to runtime check"
    (let [code (clel/emit '(satisfies? IGreeter x))]
      (is (str/includes? code "clel-satisfies-p"))
      (is (str/includes? code "'IGreeter")))))

;; ============================================================================
;; reify (clel-025)
;; ============================================================================

(deftest reify-compilation-test
  (testing "reify compiles to struct and methods"
    (let [code (clel/emit '(reify IGreeter
                             (greet [this] "hello")))]
      (is (str/includes? code "cl-defstruct"))
      (is (str/includes? code "clel--reify-"))
      (is (str/includes? code "cl-defmethod greet"))
      (is (str/includes? code "--create")))))

;; ============================================================================
;; Sequence Generators (clel-037)
;; ============================================================================

(deftest range-compilation-test
  (testing "range compiles to clel-range"
    (is (str/includes? (clel/emit '(range)) "clel-range"))
    (is (str/includes? (clel/emit '(range 10)) "clel-range"))
    (is (str/includes? (clel/emit '(range 1 10)) "clel-range"))
    (is (str/includes? (clel/emit '(range 0 10 2)) "clel-range")))

  (testing "range with single arg (end)"
    (let [code (clel/emit '(range 5))]
      (is (str/includes? code "clel-range"))
      (is (str/includes? code "5"))))

  (testing "range with start and end"
    (let [code (clel/emit '(range 1 10))]
      (is (str/includes? code "clel-range"))
      (is (re-find #"clel-range\s+1\s+10" code))))

  (testing "range with start, end, and step"
    (let [code (clel/emit '(range 0 20 5))]
      (is (str/includes? code "clel-range"))
      (is (re-find #"clel-range\s+0\s+20\s+5" code))))

  (testing "range in expression context"
    (let [code (clel/emit '(let [nums (range 10)] (reduce + nums)))]
      (is (str/includes? code "clel-range"))
      (is (str/includes? code "clel-reduce"))))

  (testing "range with map"
    (let [code (clel/emit '(map inc (range 5)))]
      (is (str/includes? code "clel-map"))
      (is (str/includes? code "clel-range")))))

(deftest repeat-compilation-test
  (testing "repeat compiles to clel-repeat"
    (is (str/includes? (clel/emit '(repeat 5 :x)) "clel-repeat")))

  (testing "repeat preserves argument order"
    (let [code (clel/emit '(repeat 3 "hello"))]
      (is (str/includes? code "clel-repeat"))
      (is (re-find #"clel-repeat\s+3\s+\"hello\"" code))))

  (testing "repeat with variable args"
    (let [code (clel/emit '(repeat n item))]
      (is (re-find #"clel-repeat\s+n\s+item" code))))

  (testing "repeat in let binding"
    (let [code (clel/emit '(let [xs (repeat 5 0)] (count xs)))]
      (is (str/includes? code "clel-repeat"))
      (is (str/includes? code "length"))))

  (testing "repeat with map"
    (let [code (clel/emit '(map inc (repeat 3 0)))]
      (is (str/includes? code "clel-map"))
      (is (str/includes? code "clel-repeat")))))

(deftest repeatedly-compilation-test
  (testing "repeatedly compiles to clel-repeatedly"
    (is (str/includes? (clel/emit '(repeatedly 5 rand)) "clel-repeatedly")))

  (testing "repeatedly preserves argument order"
    (let [code (clel/emit '(repeatedly n f))]
      (is (re-find #"clel-repeatedly\s+n\s+f" code))))

  (testing "repeatedly with inline fn"
    (let [code (clel/emit '(repeatedly 3 (fn [] (rand))))]
      (is (str/includes? code "clel-repeatedly"))
      (is (str/includes? code "lambda"))))

  (testing "repeatedly in let binding"
    (let [code (clel/emit '(let [vals (repeatedly 5 gen-val)] vals))]
      (is (str/includes? code "clel-repeatedly"))))

  (testing "repeatedly with reduce"
    (let [code (clel/emit '(reduce + (repeatedly 10 random-int)))]
      (is (str/includes? code "clel-reduce"))
      (is (str/includes? code "clel-repeatedly")))))

;; ============================================================================
;; Common Accessor Functions (clel-038)
;; ============================================================================

(deftest second-compilation-test
  (testing "second compiles to cadr"
    (is (= "(cadr xs)" (clel/emit '(second xs))))
    (is (= "(cadr (list 1 2 3))" (clel/emit '(second [1 2 3])))))

  (testing "second in expression context"
    (let [code (clel/emit '(let [x (second items)] x))]
      (is (str/includes? code "cadr"))
      (is (str/includes? code "let"))))

  (testing "second preserves argument"
    (is (re-find #"cadr\s+my-list" (clel/emit '(second my-list))))))

(deftest last-compilation-test
  (testing "last compiles to clel-last"
    (is (str/includes? (clel/emit '(last xs)) "clel-last"))
    (is (str/includes? (clel/emit '(last [1 2 3])) "clel-last")))

  (testing "last in expression context"
    (let [code (clel/emit '(let [x (last items)] x))]
      (is (str/includes? code "clel-last"))
      (is (str/includes? code "let"))))

  (testing "last preserves argument"
    (is (re-find #"clel-last\s+my-list" (clel/emit '(last my-list))))))

(deftest butlast-compilation-test
  (testing "butlast compiles to butlast"
    (is (= "(butlast xs)" (clel/emit '(butlast xs))))
    (is (str/includes? (clel/emit '(butlast [1 2 3])) "butlast")))

  (testing "butlast in expression context"
    (let [code (clel/emit '(let [x (butlast items)] x))]
      (is (str/includes? code "butlast"))
      (is (str/includes? code "let"))))

  (testing "butlast preserves argument"
    (is (re-find #"butlast\s+my-list" (clel/emit '(butlast my-list))))))

(deftest min-compilation-test
  (testing "min compiles to cl-min"
    (is (= "(cl-min 1 2 3)" (clel/emit '(min 1 2 3))))
    (is (= "(cl-min a b)" (clel/emit '(min a b)))))

  (testing "min in expression context"
    (let [code (clel/emit '(let [x (min a b c)] x))]
      (is (str/includes? code "cl-min"))
      (is (str/includes? code "let"))))

  (testing "min preserves arguments"
    (is (re-find #"cl-min\s+x\s+y\s+z" (clel/emit '(min x y z))))))

(deftest max-compilation-test
  (testing "max compiles to cl-max"
    (is (= "(cl-max 1 2 3)" (clel/emit '(max 1 2 3))))
    (is (= "(cl-max a b)" (clel/emit '(max a b)))))

  (testing "max in expression context"
    (let [code (clel/emit '(let [x (max a b c)] x))]
      (is (str/includes? code "cl-max"))
      (is (str/includes? code "let"))))

  (testing "max preserves arguments"
    (is (re-find #"cl-max\s+x\s+y\s+z" (clel/emit '(max x y z))))))

(deftest contains-p-compilation-test
  (testing "contains? compiles to clel-contains-p"
    (is (str/includes? (clel/emit '(contains? m :key)) "clel-contains-p"))
    (is (str/includes? (clel/emit '(contains? {:a 1} :a)) "clel-contains-p")))

  (testing "contains? in conditional"
    (let [code (clel/emit '(if (contains? m :key) (get m :key) :default))]
      (is (str/includes? code "clel-contains-p"))
      (is (str/includes? code "if"))))

  (testing "contains? preserves argument order"
    (is (re-find #"clel-contains-p\s+my-map\s+my-key" (clel/emit '(contains? my-map my-key))))))

(deftest name-compilation-test
  (testing "name compiles to symbol-name"
    (is (= "(symbol-name :foo)" (clel/emit '(name :foo))))
    (is (= "(symbol-name x)" (clel/emit '(name x)))))

  (testing "name in expression context"
    (let [code (clel/emit '(let [n (name :keyword)] n))]
      (is (str/includes? code "symbol-name"))
      (is (str/includes? code "let"))))

  (testing "name preserves argument"
    (is (re-find #"symbol-name\s+my-sym" (clel/emit '(name my-sym))))))

;; ============================================================================
;; Nested Data Functions (clel-036)
;; ============================================================================

(deftest get-in-compilation-test
  (testing "get-in compiles to clel-get-in"
    (is (str/includes? (clel/emit '(get-in m [:a :b])) "clel-get-in"))
    (is (str/includes? (clel/emit '(get-in data [:x :y :z])) "clel-get-in")))

  (testing "get-in with not-found value"
    (let [code (clel/emit '(get-in m [:a :b] :default))]
      (is (str/includes? code "clel-get-in"))
      (is (str/includes? code ":default"))))

  (testing "get-in preserves argument order"
    (let [code (clel/emit '(get-in my-map path))]
      (is (re-find #"clel-get-in\s+my-map\s+path" code))))

  (testing "get-in in let binding"
    (let [code (clel/emit '(let [val (get-in data [:a :b])] val))]
      (is (str/includes? code "clel-get-in"))
      (is (str/includes? code "let"))))

  (testing "get-in in conditional"
    (let [code (clel/emit '(if (get-in m [:x]) :found :not-found))]
      (is (str/includes? code "clel-get-in"))
      (is (str/includes? code "if")))))

(deftest assoc-in-compilation-test
  (testing "assoc-in compiles to clel-assoc-in"
    (is (str/includes? (clel/emit '(assoc-in m [:a :b] 42)) "clel-assoc-in"))
    (is (str/includes? (clel/emit '(assoc-in data [:x :y] val)) "clel-assoc-in")))

  (testing "assoc-in preserves argument order"
    (let [code (clel/emit '(assoc-in my-map [:k1 :k2] new-val))]
      (is (str/includes? code "clel-assoc-in"))
      (is (str/includes? code "my-map"))
      (is (str/includes? code "new-val"))))

  (testing "assoc-in in let binding"
    (let [code (clel/emit '(let [m2 (assoc-in m [:a] 1)] m2))]
      (is (str/includes? code "clel-assoc-in"))
      (is (str/includes? code "let"))))

  (testing "nested assoc-in"
    (let [code (clel/emit '(assoc-in (assoc-in m [:a] 1) [:b] 2))]
      (is (= 2 (count (re-seq #"clel-assoc-in" code)))))))

(deftest update-compilation-test
  (testing "update compiles to clel-update"
    (is (str/includes? (clel/emit '(update m :a inc)) "clel-update"))
    (is (str/includes? (clel/emit '(update data :count f)) "clel-update")))

  (testing "update with additional args"
    (let [code (clel/emit '(update m :vals conj item))]
      (is (str/includes? code "clel-update"))
      (is (str/includes? code "clel-conj"))
      (is (str/includes? code "item"))))

  (testing "update preserves argument order"
    (let [code (clel/emit '(update my-map my-key my-fn))]
      (is (re-find #"clel-update\s+my-map\s+my-key\s+my-fn" code))))

  (testing "update in let binding"
    (let [code (clel/emit '(let [m2 (update m :x inc)] m2))]
      (is (str/includes? code "clel-update"))
      (is (str/includes? code "let"))))

  (testing "update with inline fn"
    (let [code (clel/emit '(update m :val (fn [x] (+ x 1))))]
      (is (str/includes? code "clel-update"))
      (is (str/includes? code "lambda")))))

(deftest update-in-compilation-test
  (testing "update-in compiles to clel-update-in"
    (is (str/includes? (clel/emit '(update-in m [:a :b] inc)) "clel-update-in"))
    (is (str/includes? (clel/emit '(update-in data path f)) "clel-update-in")))

  (testing "update-in with additional args"
    (let [code (clel/emit '(update-in m [:a :b] conj item))]
      (is (str/includes? code "clel-update-in"))
      (is (str/includes? code "clel-conj"))
      (is (str/includes? code "item"))))

  (testing "update-in preserves argument order"
    (let [code (clel/emit '(update-in my-map ks my-fn))]
      (is (str/includes? code "clel-update-in"))
      (is (str/includes? code "my-map"))
      (is (str/includes? code "my-fn"))))

  (testing "update-in in let binding"
    (let [code (clel/emit '(let [m2 (update-in m [:x :y] inc)] m2))]
      (is (str/includes? code "clel-update-in"))
      (is (str/includes? code "let"))))

  (testing "nested update-in"
    (let [code (clel/emit '(update-in (update-in m [:a] inc) [:b] dec))]
      (is (= 2 (count (re-seq #"clel-update-in" code)))))))

(deftest merge-compilation-test
  (testing "merge compiles to clel-merge"
    (is (str/includes? (clel/emit '(merge m1 m2)) "clel-merge"))
    (is (str/includes? (clel/emit '(merge {:a 1} {:b 2})) "clel-merge")))

  (testing "merge with multiple maps"
    (let [code (clel/emit '(merge m1 m2 m3 m4))]
      (is (str/includes? code "clel-merge"))))

  (testing "merge preserves argument order"
    (let [code (clel/emit '(merge base-map overrides))]
      (is (re-find #"clel-merge\s+base-map\s+overrides" code))))

  (testing "merge in let binding"
    (let [code (clel/emit '(let [combined (merge a b)] combined))]
      (is (str/includes? code "clel-merge"))
      (is (str/includes? code "let"))))

  (testing "merge with get"
    (let [code (clel/emit '(get (merge m1 m2) :key))]
      (is (str/includes? code "clel-merge"))
      (is (str/includes? code "clel-get")))))

(deftest nested-data-integration-test
  (testing "get-in + assoc-in composition"
    (let [code (clel/emit '(assoc-in m [:a :b] (get-in m [:x :y])))]
      (is (str/includes? code "clel-assoc-in"))
      (is (str/includes? code "clel-get-in"))))

  (testing "update + merge composition"
    (let [code (clel/emit '(update (merge m1 m2) :count inc))]
      (is (str/includes? code "clel-update"))
      (is (str/includes? code "clel-merge"))))

  (testing "update-in on merged data"
    (let [code (clel/emit '(update-in (merge base config) [:settings :debug] not))]
      (is (str/includes? code "clel-update-in"))
      (is (str/includes? code "clel-merge"))))

  (testing "nested operations in let"
    (let [code (clel/emit '(let [data    {:a {:b 1}}
                                 updated (update-in data [:a :b] inc)
                                 value   (get-in updated [:a :b])]
                             value))]
      (is (str/includes? code "clel-update-in"))
      (is (str/includes? code "clel-get-in")))))

;; ============================================================================
;; For List Comprehension (clel-039)
;; ============================================================================

(deftest for-basic-compilation-test
  (testing "basic for compiles to cl-mapcan with list wrapper"
    (is (str/includes? (clel/emit '(for [x coll] x)) "cl-mapcan"))
    (is (str/includes? (clel/emit '(for [x coll] x)) "lambda"))
    (is (str/includes? (clel/emit '(for [x coll] x)) "(list x)")))

  (testing "for with body expression"
    (let [code (clel/emit '(for [x numbers] (inc x)))]
      (is (str/includes? code "cl-mapcan"))
      (is (str/includes? code "(list (1+ x))"))))

  (testing "for wraps collection in clel-seq"
    (let [code (clel/emit '(for [item items] item))]
      (is (str/includes? code "clel-seq"))))

  (testing "for with literal vector"
    (let [code (clel/emit '(for [x [1 2 3]] (* x x)))]
      (is (str/includes? code "cl-mapcan"))
      (is (str/includes? code "(list (* x x))")))))

(deftest for-when-compilation-test
  (testing "for with :when compiles to cl-mapcan"
    (is (str/includes? (clel/emit '(for [x coll :when (even? x)] x)) "cl-mapcan")))

  (testing "for :when includes condition"
    (let [code (clel/emit '(for [x numbers :when (pos? x)] x))]
      (is (str/includes? code "when"))
      (is (str/includes? code "cl-plusp"))))

  (testing "for :when wraps body in list"
    (let [code (clel/emit '(for [x coll :when (pred x)] (process x)))]
      (is (str/includes? code "(list (process x))"))))

  (testing "for :when with complex predicate"
    (let [code (clel/emit '(for [n data :when (and (pos? n) (even? n))] n))]
      (is (str/includes? code "cl-mapcan"))
      (is (str/includes? code "and")))))

(deftest for-let-compilation-test
  (testing "for with :let compiles with let*"
    (is (str/includes? (clel/emit '(for [x coll :let [y (inc x)]] y)) "let*")))

  (testing "for :let includes bindings"
    (let [code (clel/emit '(for [x numbers :let [doubled (* x 2)]] doubled))]
      (is (str/includes? code "let*"))
      (is (str/includes? code "doubled"))
      (is (str/includes? code "(* x 2)"))))

  (testing "for with multiple :let bindings"
    (let [code (clel/emit '(for [x coll :let [a (inc x) b (dec x)]] (+ a b)))]
      (is (str/includes? code "let*"))
      (is (str/includes? code "(a (1+ x))"))
      (is (str/includes? code "(b (1- x))")))))

(deftest for-combined-modifiers-test
  (testing "for with :when and :let"
    (let [code (clel/emit '(for [x coll :when (pos? x) :let [y (* x 2)]] y))]
      (is (str/includes? code "cl-mapcan"))
      (is (str/includes? code "when"))
      (is (str/includes? code "let*"))))

  (testing "for :when + :let preserves order"
    (let [code (clel/emit '(for [n numbers :when (even? n) :let [half (/ n 2)]] half))]
      (is (str/includes? code "cl-mapcan"))
      (is (str/includes? code "cl-evenp"))
      (is (str/includes? code "half"))))

  (testing "for in let binding"
    (let [code (clel/emit '(let [squares (for [x nums] (* x x))]
                             (first squares)))]
      (is (str/includes? code "cl-mapcan"))
      (is (str/includes? code "squares"))))

  (testing "nested for (via map)"
    (let [code (clel/emit '(for [x xs]
                             (for [y ys]
                               (+ x y))))]
      (is (= 2 (count (re-seq #"cl-mapcan" code)))))))

;; ============================================================================
;; Multi-binding for/doseq (clel-045)
;; ============================================================================

(deftest for-multi-binding-compilation-test
  (testing "for with two bindings creates nested cl-mapcan"
    (let [code (clel/emit '(for [x xs y ys] [x y]))]
      (is (= 2 (count (re-seq #"cl-mapcan" code))))
      (is (str/includes? code "(lambda (x)"))
      (is (str/includes? code "(lambda (y)"))))

  (testing "multi-binding for with :when between bindings"
    (let [code (clel/emit '(for [x xs :when (pos? x) y ys] [x y]))]
      (is (= 2 (count (re-seq #"cl-mapcan" code))))
      (is (str/includes? code "(when (cl-plusp x)"))))

  (testing "multi-binding for with :let between bindings"
    (let [code (clel/emit '(for [x xs :let [x2 (* x 2)] y ys] [x2 y]))]
      (is (= 2 (count (re-seq #"cl-mapcan" code))))
      (is (str/includes? code "(let*"))
      (is (str/includes? code "(x2 (* x 2))"))))

  (testing "complex multi-binding for (clel-045 spec example)"
    (let [code (clel/emit '(for [x     [1 2 3]
                                 y     [4 5 6]
                                 :when (even? (+ x y))
                                 :let  [z (* x y)]]
                             [x y z]))]
      (is (= 2 (count (re-seq #"cl-mapcan" code))))
      (is (str/includes? code "(when (cl-evenp (+ x y))"))
      (is (str/includes? code "(let* ((z (* x y)))"))
      (is (str/includes? code "(list (list x y z))")))))

(deftest doseq-multi-binding-compilation-test
  (testing "doseq with two bindings creates nested dolist"
    (let [code (clel/emit '(doseq [x xs y ys] (process x y)))]
      (is (= 2 (count (re-seq #"dolist" code))))
      (is (str/includes? code "(x (clel-seq"))
      (is (str/includes? code "(y (clel-seq"))))

  (testing "multi-binding doseq with :when"
    (let [code (clel/emit '(doseq [x xs :when (pos? x) y ys] (process x y)))]
      (is (= 2 (count (re-seq #"dolist" code))))
      (is (str/includes? code "(when (cl-plusp x)"))))

  (testing "multi-binding doseq with :let"
    (let [code (clel/emit '(doseq [x xs :let [x2 (* x 2)] y ys] (process x2 y)))]
      (is (= 2 (count (re-seq #"dolist" code))))
      (is (str/includes? code "(let*"))))

  (testing "doseq with :while uses cl-block"
    (let [code (clel/emit '(doseq [x xs :while (pos? x)] (process x)))]
      (is (str/includes? code "(cl-block nil"))
      (is (str/includes? code "(unless (cl-plusp x) (cl-return))")))))

(deftest for-while-compilation-test
  (testing "for with :while approximates as when"
    ;; Note: :while in map-based for is approximated as :when since
    ;; cl-mapcan doesn't support early termination
    (let [code (clel/emit '(for [x xs :while (pos? x)] x))]
      (is (str/includes? code "cl-mapcan"))
      (is (str/includes? code "(when (cl-plusp x)"))))

  (testing "for with :while and :let"
    (let [code (clel/emit '(for [x xs :while (pos? x) :let [y (* x 2)]] y))]
      (is (str/includes? code "cl-mapcan"))
      (is (str/includes? code "when"))
      (is (str/includes? code "let*")))))

;; ============================================================================
;; String Functions (clel-042)
;; ============================================================================

(deftest string-core-functions-test
  (testing "str compiles to clel-str"
    (is (str/includes? (clel/emit '(str "a" "b")) "clel-str"))
    (is (str/includes? (clel/emit '(str x y z)) "clel-str")))

  (testing "subs compiles to substring"
    (is (str/includes? (clel/emit '(subs s 0 5)) "substring")))

  (testing "format passes through"
    (is (str/includes? (clel/emit '(format "%s: %d" name count)) "format"))))

(deftest clojure-string-join-test
  (testing "clojure.string/join compiles to clel-str-join"
    (is (str/includes? (clel/emit '(clojure.string/join ", " items)) "clel-str-join")))

  (testing "join with empty separator"
    (let [code (clel/emit '(clojure.string/join "" parts))]
      (is (str/includes? code "clel-str-join"))))

  (testing "join preserves argument order"
    (let [code (clel/emit '(clojure.string/join sep coll))]
      (is (re-find #"clel-str-join\s+sep\s+coll" code)))))

(deftest clojure-string-split-test
  (testing "clojure.string/split compiles to clel-str-split"
    (is (str/includes? (clel/emit '(clojure.string/split s #",")) "clel-str-split")))

  (testing "split preserves argument order"
    (let [code (clel/emit '(clojure.string/split text pattern))]
      (is (re-find #"clel-str-split\s+text\s+pattern" code)))))

(deftest clojure-string-replace-test
  (testing "clojure.string/replace compiles to clel-str-replace"
    (is (str/includes? (clel/emit '(clojure.string/replace s "old" "new")) "clel-str-replace")))

  (testing "clojure.string/replace-first compiles to clel-str-replace-first"
    (is (str/includes? (clel/emit '(clojure.string/replace-first s "x" "y")) "clel-str-replace-first"))))

(deftest clojure-string-trim-test
  (testing "clojure.string/trim compiles to clel-str-trim"
    (is (str/includes? (clel/emit '(clojure.string/trim s)) "clel-str-trim")))

  (testing "clojure.string/triml compiles to clel-str-triml"
    (is (str/includes? (clel/emit '(clojure.string/triml s)) "clel-str-triml")))

  (testing "clojure.string/trimr compiles to clel-str-trimr"
    (is (str/includes? (clel/emit '(clojure.string/trimr s)) "clel-str-trimr"))))

(deftest clojure-string-case-test
  (testing "clojure.string/lower-case compiles to clel-str-lower"
    (is (str/includes? (clel/emit '(clojure.string/lower-case s)) "clel-str-lower")))

  (testing "clojure.string/upper-case compiles to clel-str-upper"
    (is (str/includes? (clel/emit '(clojure.string/upper-case s)) "clel-str-upper")))

  (testing "clojure.string/capitalize compiles to clel-str-capitalize"
    (is (str/includes? (clel/emit '(clojure.string/capitalize s)) "clel-str-capitalize"))))

(deftest clojure-string-predicates-test
  (testing "clojure.string/blank? compiles to clel-str-blank-p"
    (is (str/includes? (clel/emit '(clojure.string/blank? s)) "clel-str-blank-p")))

  (testing "clojure.string/includes? compiles to clel-str-includes-p"
    (is (str/includes? (clel/emit '(clojure.string/includes? s "needle")) "clel-str-includes-p")))

  (testing "clojure.string/starts-with? compiles to clel-str-starts-with-p"
    (is (str/includes? (clel/emit '(clojure.string/starts-with? s "prefix")) "clel-str-starts-with-p")))

  (testing "clojure.string/ends-with? compiles to clel-str-ends-with-p"
    (is (str/includes? (clel/emit '(clojure.string/ends-with? s "suffix")) "clel-str-ends-with-p"))))

(deftest clojure-string-misc-test
  (testing "clojure.string/reverse compiles to clel-str-reverse"
    (is (str/includes? (clel/emit '(clojure.string/reverse s)) "clel-str-reverse")))

  (testing "clojure.string/index-of compiles to clel-str-index-of"
    (is (str/includes? (clel/emit '(clojure.string/index-of s "x")) "clel-str-index-of")))

  (testing "clojure.string/last-index-of compiles to clel-str-last-index-of"
    (is (str/includes? (clel/emit '(clojure.string/last-index-of s "x")) "clel-str-last-index-of"))))

(deftest regex-string-functions-test
  (testing "re-find compiles to clel-str-re-find"
    (is (str/includes? (clel/emit '(re-find #"\\d+" s)) "clel-str-re-find")))

  (testing "re-matches compiles to clel-str-re-matches"
    (is (str/includes? (clel/emit '(re-matches #"\\d+" s)) "clel-str-re-matches")))

  (testing "re-seq compiles to clel-str-re-seq"
    (is (str/includes? (clel/emit '(re-seq #"\\w+" s)) "clel-str-re-seq"))))

(deftest string-function-composition-test
  (testing "chained string operations"
    (let [code (clel/emit '(clojure.string/upper-case
                            (clojure.string/trim s)))]
      (is (str/includes? code "clel-str-upper"))
      (is (str/includes? code "clel-str-trim"))))

  (testing "string functions in let"
    (let [code (clel/emit '(let [trimmed (clojure.string/trim s)
                                 upper   (clojure.string/upper-case trimmed)]
                             upper))]
      (is (str/includes? code "clel-str-trim"))
      (is (str/includes? code "clel-str-upper"))))

  (testing "string predicates in conditionals"
    (let [code (clel/emit '(when (clojure.string/blank? s)
                             (println "empty")))]
      (is (str/includes? code "clel-str-blank-p")))))

;; ============================================================================
;; Set Operations (clel-044)
;; ============================================================================

(deftest set-constructor-compilation-test
  (testing "set compiles to clel-set-from-coll"
    (is (str/includes? (clel/emit '(set xs)) "clel-set-from-coll"))
    (is (str/includes? (clel/emit '(set [1 2 3])) "clel-set-from-coll")))

  (testing "set preserves argument"
    (is (re-find #"clel-set-from-coll\s+my-coll" (clel/emit '(set my-coll)))))

  (testing "set in let binding"
    (let [code (clel/emit '(let [s (set items)] s))]
      (is (str/includes? code "clel-set-from-coll"))
      (is (str/includes? code "let"))))

  (testing "hash-set compiles to clel-set"
    (is (str/includes? (clel/emit '(hash-set 1 2 3)) "clel-set")))

  (testing "set? compiles to clel-set-p"
    (is (str/includes? (clel/emit '(set? x)) "clel-set-p")))

  (testing "disj compiles to clel-set-remove"
    (is (str/includes? (clel/emit '(disj s :item)) "clel-set-remove"))))

(deftest set-union-compilation-test
  (testing "clojure.set/union compiles to clel-set-union"
    (is (str/includes? (clel/emit '(clojure.set/union s1 s2)) "clel-set-union")))

  (testing "union preserves argument order"
    (let [code (clel/emit '(clojure.set/union a-set b-set))]
      (is (re-find #"clel-set-union\s+a-set\s+b-set" code))))

  (testing "union with multiple sets"
    (let [code (clel/emit '(clojure.set/union s1 s2 s3 s4))]
      (is (str/includes? code "clel-set-union"))
      (is (str/includes? code "s1"))
      (is (str/includes? code "s4"))))

  (testing "union in let binding"
    (let [code (clel/emit '(let [combined (clojure.set/union a b)] combined))]
      (is (str/includes? code "clel-set-union"))
      (is (str/includes? code "let")))))

(deftest set-intersection-compilation-test
  (testing "clojure.set/intersection compiles to clel-set-intersection"
    (is (str/includes? (clel/emit '(clojure.set/intersection s1 s2)) "clel-set-intersection")))

  (testing "intersection preserves argument order"
    (let [code (clel/emit '(clojure.set/intersection a-set b-set))]
      (is (re-find #"clel-set-intersection\s+a-set\s+b-set" code))))

  (testing "intersection with multiple sets"
    (let [code (clel/emit '(clojure.set/intersection s1 s2 s3))]
      (is (str/includes? code "clel-set-intersection"))))

  (testing "intersection in conditional"
    (let [code (clel/emit '(if (empty? (clojure.set/intersection a b)) :disjoint :overlap))]
      (is (str/includes? code "clel-set-intersection"))
      (is (str/includes? code "clel-empty-p")))))

(deftest set-difference-compilation-test
  (testing "clojure.set/difference compiles to clel-set-difference"
    (is (str/includes? (clel/emit '(clojure.set/difference s1 s2)) "clel-set-difference")))

  (testing "difference preserves argument order"
    (let [code (clel/emit '(clojure.set/difference main-set exclude-set))]
      (is (re-find #"clel-set-difference\s+main-set\s+exclude-set" code))))

  (testing "difference with multiple sets to exclude"
    (let [code (clel/emit '(clojure.set/difference s1 s2 s3))]
      (is (str/includes? code "clel-set-difference")))))

(deftest set-subset-superset-compilation-test
  (testing "clojure.set/subset? compiles to clel-set-subset-p"
    (is (str/includes? (clel/emit '(clojure.set/subset? s1 s2)) "clel-set-subset-p")))

  (testing "subset? preserves argument order"
    (let [code (clel/emit '(clojure.set/subset? child-set parent-set))]
      (is (re-find #"clel-set-subset-p\s+child-set\s+parent-set" code))))

  (testing "clojure.set/superset? compiles to clel-set-superset-p"
    (is (str/includes? (clel/emit '(clojure.set/superset? s1 s2)) "clel-set-superset-p")))

  (testing "subset/superset in conditional"
    (let [code (clel/emit '(when (clojure.set/subset? a b) :is-subset))]
      (is (str/includes? code "clel-set-subset-p"))
      (is (str/includes? code "when")))))

(deftest set-select-compilation-test
  (testing "clojure.set/select compiles to clel-set-select"
    (is (str/includes? (clel/emit '(clojure.set/select even? s)) "clel-set-select")))

  (testing "select preserves argument order"
    (let [code (clel/emit '(clojure.set/select pred my-set))]
      (is (re-find #"clel-set-select\s+pred\s+my-set" code))))

  (testing "select with inline predicate"
    (let [code (clel/emit '(clojure.set/select (fn [x] (> x 0)) numbers))]
      (is (str/includes? code "clel-set-select"))
      (is (str/includes? code "lambda"))))

  (testing "select in let binding"
    (let [code (clel/emit '(let [filtered (clojure.set/select pos? nums)] filtered))]
      (is (str/includes? code "clel-set-select")))))

(deftest set-relational-compilation-test
  (testing "clojure.set/project compiles to clel-set-project"
    (is (str/includes? (clel/emit '(clojure.set/project xrel [:a :b])) "clel-set-project")))

  (testing "project preserves argument order"
    (let [code (clel/emit '(clojure.set/project rel ks))]
      (is (re-find #"clel-set-project\s+rel\s+ks" code))))

  (testing "clojure.set/rename compiles to clel-set-rename"
    (is (str/includes? (clel/emit '(clojure.set/rename xrel {:old :new})) "clel-set-rename")))

  (testing "rename preserves argument order"
    (let [code (clel/emit '(clojure.set/rename rel kmap))]
      (is (re-find #"clel-set-rename\s+rel\s+kmap" code))))

  (testing "clojure.set/rename-keys compiles to clel-rename-keys"
    (is (str/includes? (clel/emit '(clojure.set/rename-keys m {:a :b})) "clel-rename-keys")))

  (testing "rename-keys preserves argument order"
    (let [code (clel/emit '(clojure.set/rename-keys my-map kmap))]
      (is (re-find #"clel-rename-keys\s+my-map\s+kmap" code)))))

(deftest set-join-compilation-test
  (testing "clojure.set/join compiles to clel-set-join"
    (is (str/includes? (clel/emit '(clojure.set/join xrel yrel)) "clel-set-join")))

  (testing "join preserves argument order"
    (let [code (clel/emit '(clojure.set/join left right))]
      (is (re-find #"clel-set-join\s+left\s+right" code))))

  (testing "join with key mapping"
    (let [code (clel/emit '(clojure.set/join xrel yrel {:a :x}))]
      (is (str/includes? code "clel-set-join"))))

  (testing "join in let binding"
    (let [code (clel/emit '(let [result (clojure.set/join t1 t2)] result))]
      (is (str/includes? code "clel-set-join")))))

(deftest set-index-compilation-test
  (testing "clojure.set/index compiles to clel-set-index"
    (is (str/includes? (clel/emit '(clojure.set/index xrel [:a])) "clel-set-index")))

  (testing "index preserves argument order"
    (let [code (clel/emit '(clojure.set/index rel ks))]
      (is (re-find #"clel-set-index\s+rel\s+ks" code))))

  (testing "index with multiple keys"
    (let [code (clel/emit '(clojure.set/index xrel [:a :b :c]))]
      (is (str/includes? code "clel-set-index")))))

(deftest set-map-invert-compilation-test
  (testing "clojure.set/map-invert compiles to clel-map-invert"
    (is (str/includes? (clel/emit '(clojure.set/map-invert m)) "clel-map-invert")))

  (testing "map-invert preserves argument"
    (let [code (clel/emit '(clojure.set/map-invert my-map))]
      (is (re-find #"clel-map-invert\s+my-map" code))))

  (testing "map-invert in let binding"
    (let [code (clel/emit '(let [inverted (clojure.set/map-invert m)] inverted))]
      (is (str/includes? code "clel-map-invert")))))

(deftest set-composition-compilation-test
  (testing "union + intersection composition"
    (let [code (clel/emit '(clojure.set/intersection
                            (clojure.set/union a b)
                            (clojure.set/union c d)))]
      (is (str/includes? code "clel-set-intersection"))
      (is (= 2 (count (re-seq #"clel-set-union" code))))))

  (testing "set constructor + operations"
    (let [code (clel/emit '(clojure.set/union (set xs) (set ys)))]
      (is (str/includes? code "clel-set-union"))
      (is (= 2 (count (re-seq #"clel-set-from-coll" code))))))

  (testing "difference after union"
    (let [code (clel/emit '(clojure.set/difference
                            (clojure.set/union a b)
                            c))]
      (is (str/includes? code "clel-set-difference"))
      (is (str/includes? code "clel-set-union"))))

  (testing "select from intersection"
    (let [code (clel/emit '(clojure.set/select even?
                                               (clojure.set/intersection nums1 nums2)))]
      (is (str/includes? code "clel-set-select"))
      (is (str/includes? code "clel-set-intersection")))))

;; ============================================================================
;; Additional Sequence Functions (clel-041)
;; ============================================================================

(deftest partition-all-compilation-test
  (testing "partition-all compiles to clel-partition-all"
    (is (str/includes? (clel/emit '(partition-all 3 xs)) "clel-partition-all")))

  (testing "partition-all preserves argument order"
    (is (re-find #"clel-partition-all\s+n\s+coll" (clel/emit '(partition-all n coll)))))

  (testing "partition-all in let binding"
    (let [code (clel/emit '(let [chunks (partition-all 5 items)] chunks))]
      (is (str/includes? code "clel-partition-all"))
      (is (str/includes? code "let"))))

  (testing "partition-all vs partition (semantic difference)"
    ;; partition drops incomplete final partition, partition-all keeps it
    (let [code-all (clel/emit '(partition-all 3 xs))
          code-reg (clel/emit '(partition 3 xs))]
      (is (str/includes? code-all "clel-partition-all"))
      (is (str/includes? code-reg "clel-partition"))
      (is (not (str/includes? code-reg "clel-partition-all"))))))

(deftest partition-by-compilation-test
  (testing "partition-by compiles to clel-partition-by"
    (is (str/includes? (clel/emit '(partition-by odd? xs)) "clel-partition-by")))

  (testing "partition-by preserves argument order"
    (is (re-find #"clel-partition-by\s+f\s+coll" (clel/emit '(partition-by f coll)))))

  (testing "partition-by with keyword function"
    (let [code (clel/emit '(partition-by :type items))]
      (is (str/includes? code "clel-partition-by"))
      (is (str/includes? code ":type"))))

  (testing "partition-by in let binding"
    (let [code (clel/emit '(let [groups (partition-by :category data)] groups))]
      (is (str/includes? code "clel-partition-by"))))

  (testing "partition-by with inline function"
    (let [code (clel/emit '(partition-by (fn [x] (mod x 3)) nums))]
      (is (str/includes? code "clel-partition-by"))
      (is (str/includes? code "lambda")))))

(deftest interpose-compilation-test
  (testing "interpose compiles to clel-interpose"
    (is (str/includes? (clel/emit '(interpose ", " xs)) "clel-interpose")))

  (testing "interpose preserves argument order"
    (is (re-find #"clel-interpose\s+sep\s+coll" (clel/emit '(interpose sep coll)))))

  (testing "interpose with keyword separator"
    (let [code (clel/emit '(interpose :sep items))]
      (is (str/includes? code "clel-interpose"))
      (is (str/includes? code ":sep"))))

  (testing "interpose in let binding"
    (let [code (clel/emit '(let [spaced (interpose " " words)] spaced))]
      (is (str/includes? code "clel-interpose"))))

  (testing "interpose with str/join pattern"
    (let [code (clel/emit '(apply str (interpose ", " items)))]
      (is (str/includes? code "clel-interpose"))
      (is (str/includes? code "apply")))))

(deftest distinct-compilation-test
  (testing "distinct compiles to clel-distinct"
    (is (str/includes? (clel/emit '(distinct xs)) "clel-distinct")))

  (testing "distinct preserves argument"
    (is (re-find #"clel-distinct\s+coll" (clel/emit '(distinct coll)))))

  (testing "distinct in let binding"
    (let [code (clel/emit '(let [unique (distinct items)] unique))]
      (is (str/includes? code "clel-distinct"))
      (is (str/includes? code "let"))))

  (testing "distinct with map"
    (let [code (clel/emit '(distinct (map :id users)))]
      (is (str/includes? code "clel-distinct"))
      (is (str/includes? code "clel-map"))))

  (testing "distinct in expression context"
    (let [code (clel/emit '(count (distinct xs)))]
      (is (str/includes? code "clel-distinct"))
      (is (str/includes? code "length")))))

(deftest dedupe-compilation-test
  (testing "dedupe compiles to clel-dedupe"
    (is (str/includes? (clel/emit '(dedupe xs)) "clel-dedupe")))

  (testing "dedupe preserves argument"
    (is (re-find #"clel-dedupe\s+coll" (clel/emit '(dedupe coll)))))

  (testing "dedupe in let binding"
    (let [code (clel/emit '(let [deduped (dedupe items)] deduped))]
      (is (str/includes? code "clel-dedupe"))
      (is (str/includes? code "let"))))

  (testing "dedupe vs distinct (semantic difference)"
    ;; dedupe removes consecutive duplicates, distinct removes all duplicates
    (let [code-dedupe   (clel/emit '(dedupe xs))
          code-distinct (clel/emit '(distinct xs))]
      (is (str/includes? code-dedupe "clel-dedupe"))
      (is (str/includes? code-distinct "clel-distinct"))
      (is (not (str/includes? code-dedupe "clel-distinct"))))))

(deftest split-at-compilation-test
  (testing "split-at compiles to clel-split-at"
    (is (str/includes? (clel/emit '(split-at 5 xs)) "clel-split-at")))

  (testing "split-at preserves argument order"
    (is (re-find #"clel-split-at\s+n\s+coll" (clel/emit '(split-at n coll)))))

  (testing "split-at in let binding with destructuring pattern"
    (let [code (clel/emit '(let [result (split-at 3 items)] result))]
      (is (str/includes? code "clel-split-at"))))

  (testing "split-at with numeric literal"
    (let [code (clel/emit '(split-at 10 data))]
      (is (str/includes? code "clel-split-at"))
      (is (str/includes? code "10")))))

(deftest split-with-compilation-test
  (testing "split-with compiles to clel-split-with"
    (is (str/includes? (clel/emit '(split-with pos? xs)) "clel-split-with")))

  (testing "split-with preserves argument order"
    (is (re-find #"clel-split-with\s+pred\s+coll" (clel/emit '(split-with pred coll)))))

  (testing "split-with with inline predicate"
    (let [code (clel/emit '(split-with (fn [x] (< x 5)) nums))]
      (is (str/includes? code "clel-split-with"))
      (is (str/includes? code "lambda"))))

  (testing "split-with in let binding"
    (let [code (clel/emit '(let [parts (split-with even? data)] parts))]
      (is (str/includes? code "clel-split-with")))))

(deftest additional-seq-composition-test
  (testing "partition-all + map composition"
    (let [code (clel/emit '(map count (partition-all 3 xs)))]
      (is (str/includes? code "clel-map"))
      (is (str/includes? code "clel-partition-all"))))

  (testing "interpose + apply str (common pattern)"
    (let [code (clel/emit '(apply str (interpose "-" parts)))]
      (is (str/includes? code "clel-interpose"))
      (is (str/includes? code "apply"))))

  (testing "distinct + filter composition"
    (let [code (clel/emit '(filter pos? (distinct xs)))]
      (is (str/includes? code "clel-filter"))
      (is (str/includes? code "clel-distinct"))))

  (testing "partition-by + map composition"
    (let [code (clel/emit '(map first (partition-by :type items)))]
      (is (str/includes? code "clel-map"))
      (is (str/includes? code "clel-partition-by"))))

  (testing "split-with in conditional"
    (let [code (clel/emit '(let [parts (split-with pred xs)]
                             (if (empty? (first parts)) :all-false :has-true)))]
      (is (str/includes? code "clel-split-with"))
      (is (str/includes? code "clel-empty-p")))))

;; ============================================================================
;; Transducers (clel-043)
;; ============================================================================

(deftest transduce-compilation-test
  (testing "basic transduce"
    (let [code (clel/emit '(transduce (map inc) + 0 xs))]
      (is (str/includes? code "clel-transduce"))
      (is (str/includes? code "1+"))))

  (testing "transduce with filter"
    (let [code (clel/emit '(transduce (filter even?) + xs))]
      (is (str/includes? code "clel-transduce"))
      (is (str/includes? code "cl-evenp"))))  ;; even? maps to cl-evenp

  (testing "transduce with comp"
    (let [code (clel/emit '(transduce (comp (map inc) (filter pos?)) conj [] xs))]
      (is (str/includes? code "clel-transduce"))
      (is (str/includes? code "clel-comp"))  ;; comp emits as clel-comp
      (is (str/includes? code "clel-conj")))))

(deftest reduced-compilation-test
  (testing "reduced wrapper"
    (let [code (clel/emit '(reduced x))]
      (is (str/includes? code "clel-reduced"))))

  (testing "reduced? predicate"
    (let [code (clel/emit '(reduced? val))]
      (is (str/includes? code "clel-reduced-p"))))

  (testing "unreduced unwrapper"
    (let [code (clel/emit '(unreduced wrapped))]
      (is (str/includes? code "clel-unreduced"))))

  (testing "reduced in reduce context"
    (let [code (clel/emit '(reduce (fn [acc x]
                                     (if (> acc 100)
                                       (reduced acc)
                                       (+ acc x)))
                                   0 xs))]
      (is (str/includes? code "clel-reduced"))
      (is (str/includes? code "reduce")))))

(deftest keep-compilation-test
  (testing "keep basic"
    (let [code (clel/emit '(keep identity xs))]
      (is (str/includes? code "clel-keep"))))

  (testing "keep with function"
    (let [code (clel/emit '(keep (fn [x] (when (> x 0) x)) xs))]
      (is (str/includes? code "clel-keep"))
      (is (str/includes? code "lambda"))))

  (testing "keep-indexed"
    (let [code (clel/emit '(keep-indexed (fn [i x] (when (even? i) x)) xs))]
      (is (str/includes? code "clel-keep-indexed"))
      (is (str/includes? code "cl-evenp")))))

(deftest eduction-compilation-test
  (testing "basic eduction"
    (let [code (clel/emit '(eduction (map inc) xs))]
      (is (str/includes? code "clel-eduction"))))

  (testing "eduction with multiple xforms"
    (let [code (clel/emit '(eduction (map inc) (filter pos?) xs))]
      (is (str/includes? code "clel-eduction"))))

  (testing "eduction with comp"
    (let [code (clel/emit '(eduction (comp (map inc) (take 5)) xs))]
      (is (str/includes? code "clel-eduction"))
      (is (str/includes? code "clel-comp")))))

(deftest cat-compilation-test
  (testing "cat transducer"
    (let [code (clel/emit '(transduce cat conj [] nested))]
      (is (str/includes? code "clel-transduce"))
      (is (str/includes? code "clel-cat-xf"))))

  (testing "mapcat via comp + cat"
    (let [code (clel/emit '(transduce (comp (map f) cat) conj [] xs))]
      (is (str/includes? code "clel-transduce"))
      (is (str/includes? code "clel-cat-xf")))))

(deftest transducer-composition-test
  (testing "multiple transducers with comp"
    (let [code (clel/emit '(transduce (comp (map inc)
                                            (filter even?)
                                            (take 10))
                                      conj [] xs))]
      (is (str/includes? code "clel-transduce"))
      (is (str/includes? code "clel-comp"))
      (is (str/includes? code "clel-conj"))))

  (testing "into with transducer"
    (let [code (clel/emit '(into [] (map inc) xs))]
      ;; into with 3 args should use xform version
      (is (or (str/includes? code "clel-into")
              (str/includes? code "clel-transduce")))))

  (testing "sequence with transducer"
    (let [code (clel/emit '(sequence (map inc) xs))]
      ;; sequence with xform should use lazy version
      (is (or (str/includes? code "clel-sequence")
              (str/includes? code "clel-map")))))

  (testing "nested transducer operations"
    (let [code (clel/emit '(transduce (comp (map (fn [x] (transduce (map inc) + 0 x)))
                                            (filter pos?))
                                      + 0 nested))]
      (is (str/includes? code "clel-transduce"))
      ;; Should have nested transduce
      (is (> (count (re-seq #"clel-transduce" code)) 1)))))

