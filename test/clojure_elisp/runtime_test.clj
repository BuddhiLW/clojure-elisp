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

