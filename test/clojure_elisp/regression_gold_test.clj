(ns clojure-elisp.regression-gold-test
  "Gold/snapshot tests for ClojureElisp compiler output.

   These tests lock in the current compiler output for representative forms.
   Any change that alters emission will break these tests, catching unintentional
   regressions across the full surface area. If a change is intentional, update
   the expected string to match the new output.

   Coverage: literals, arithmetic, comparisons, core mappings, special forms,
   namespace prefixing, destructuring, multi-arity, try/catch, interop,
   atoms, predicates, higher-order, protocols, threading, quote."
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-elisp.core :as clel]
            [clojure-elisp.analyzer :as ana]
            [clojure-elisp.emitter :as emit]
            [clojure.string :as str]))

;; ============================================================================
;; Helper
;; ============================================================================

(defn ae
  "Analyze form and emit to Elisp string."
  [form]
  (-> form ana/analyze emit/emit))

;; ============================================================================
;; Gold: Literals
;; ============================================================================

(deftest gold-literals
  (testing "nil"         (is (= "nil"              (ae nil))))
  (testing "true"        (is (= "t"                (ae true))))
  (testing "false"       (is (= "nil"              (ae false))))
  (testing "integer"     (is (= "42"               (ae 42))))
  (testing "negative"    (is (= "-7"               (ae -7))))
  (testing "float"       (is (= "3.14"             (ae 3.14))))
  (testing "string"      (is (= "\"hello world\""  (ae "hello world"))))
  (testing "keyword"     (is (= ":foo"             (ae :foo))))
  (testing "ns keyword"  (is (= ":bar"             (ae :my.ns/bar))))
  (testing "vector"      (is (= "(list 1 2 3)"     (ae '[1 2 3]))))
  (testing "map"         (is (= "'((:a . 1) (:b . 2))" (ae '{:a 1 :b 2}))))
  (testing "quoted list" (is (= "'(1 2 3)"         (ae '(quote (1 2 3)))))))

;; ============================================================================
;; Gold: Arithmetic
;; ============================================================================

(deftest gold-arithmetic
  (testing "+"    (is (= "(+ 1 2)"    (ae '(+ 1 2)))))
  (testing "-"    (is (= "(- 10 3)"   (ae '(- 10 3)))))
  (testing "*"    (is (= "(* 4 5)"    (ae '(* 4 5)))))
  (testing "/"    (is (= "(/ 10 2)"   (ae '(/ 10 2)))))
  (testing "inc"  (is (= "(1+ x)"     (ae '(inc x)))))
  (testing "dec"  (is (= "(1- x)"     (ae '(dec x))))))

;; ============================================================================
;; Gold: Comparisons
;; ============================================================================

(deftest gold-comparisons
  (testing "="   (is (= "(equal a b)" (ae '(= a b)))))
  (testing "<"   (is (= "(< a b)"     (ae '(< a b)))))
  (testing ">"   (is (= "(> a b)"     (ae '(> a b)))))
  (testing "<="  (is (= "(<= a b)"    (ae '(<= a b)))))
  (testing ">="  (is (= "(>= a b)"    (ae '(>= a b))))))

;; ============================================================================
;; Gold: Core function mappings
;; ============================================================================

(deftest gold-core-mappings
  (testing "first"    (is (= "(clel-first coll)"       (ae '(first coll)))))
  (testing "rest"     (is (= "(clel-rest coll)"        (ae '(rest coll)))))
  (testing "cons"     (is (= "(cons 1 coll)"           (ae '(cons 1 coll)))))
  (testing "count"    (is (= "(length coll)"           (ae '(count coll)))))
  (testing "str"      (is (= "(clel-str \"a\" \"b\")"  (ae '(str "a" "b")))))
  (testing "conj"     (is (= "(clel-conj coll 1)"      (ae '(conj coll 1)))))
  (testing "get"      (is (= "(clel-get m :key)"       (ae '(get m :key)))))
  (testing "assoc"    (is (= "(clel-assoc m :key val)"  (ae '(assoc m :key val)))))
  (testing "not"      (is (= "(not x)"                 (ae '(not x)))))
  (testing "nil?"     (is (= "(null x)"                (ae '(nil? x)))))
  (testing "println"  (is (= "(message \"hi\")"        (ae '(println "hi"))))))

;; ============================================================================
;; Gold: Special forms (single-form emit)
;; ============================================================================

(deftest gold-def
  (testing "def"  (is (= "(defvar x 42 )" (ae '(def x 42))))))

(deftest gold-defn
  (testing "defn simple"
    (is (= "(defun add (a b)\n  (+ a b))"
           (ae '(defn add [a b] (+ a b))))))
  (testing "defn-"
    (is (= "(defun secret (x)\n  (+ x 1))"
           (ae '(defn- secret [x] (+ x 1)))))))

(deftest gold-fn
  (testing "fn/lambda"
    (is (= "(lambda (x)\n    (* x 2))"
           (ae '(fn [x] (* x 2)))))))

(deftest gold-let
  (testing "let binding"
    (is (= "(let* ((a 1)\n        (b 2))\n    (+ a b))"
           (ae '(let [a 1 b 2] (+ a b)))))))

(deftest gold-if
  (testing "if"
    (is (= "(if (> x 0) \"pos\" \"non-pos\")"
           (ae '(if (> x 0) "pos" "non-pos"))))))

(deftest gold-when
  (testing "when"
    (is (= "(when (> x 0)\n    (message x))"
           (ae '(when (> x 0) (println x)))))))

(deftest gold-do
  (testing "do/progn"
    (is (= "(progn\n  (message \"a\")\n  (message \"b\")\n  42)"
           (ae '(do (println "a") (println "b") 42))))))

(deftest gold-cond
  (testing "cond"
    (is (= "(cond\n  ((> x 0) \"pos\")\n  ((< x 0) \"neg\")\n  (t \"zero\"))"
           (ae '(cond (> x 0) "pos" (< x 0) "neg" :else "zero"))))))

(deftest gold-loop-recur
  (testing "loop/recur"
    (is (= "(cl-labels ((recur (i)\n      (if (>= i 10) i (recur (1+ i)))))\n    (recur 0))"
           (ae '(loop [i 0] (if (>= i 10) i (recur (inc i)))))))))

;; ============================================================================
;; Gold: Namespace-qualified output (compile-string)
;; ============================================================================

(deftest gold-ns-header
  (testing "ns produces Elisp file header"
    (let [result (clel/compile-string "(ns my-pkg)")]
      (is (str/includes? result ";;; my-pkg.el"))
      (is (str/includes? result "lexical-binding: t"))
      (is (str/includes? result "(eval-and-compile (require 'clojure-elisp-runtime))")))))

(deftest gold-ns-with-require
  (testing "ns with :require emits (require ...)"
    (let [result (clel/compile-string "(ns my-app (:require [my-lib :as lib]))")]
      (is (str/includes? result "(require 'my-lib)")))))

(deftest gold-ns-defn-qualified
  (testing "defn in namespace is NOT ns-prefixed in single-form compile-string
            (ns-qualifying happens in compile-file-string with ns context)"
    (let [result (clel/compile-string "(ns my-pkg)\n(defn add [a b] (+ a b))")]
      (is (str/includes? result "defun"))
      (is (str/includes? result "add")))))

(deftest gold-ns-private-fn
  (testing "compile-file-string: defn- produces function (ns-qualifying in file context)"
    (let [result (clel/compile-file-string "(ns my-pkg)\n(defn- secret [x] (+ x 1))")]
      (is (str/includes? result "defun"))
      (is (str/includes? result "secret")))))

(deftest gold-ns-private-dash-fn
  (testing "compile-file-string: defn- with dash name (triple-dash fix f48384d)"
    (let [result (clel/compile-file-string "(ns my-pkg)\n(defn- -helper [x] (+ x 1))")]
      (is (str/includes? result "defun"))
      (is (str/includes? result "-helper"))
      ;; Check that the function name itself doesn't have triple-dash
      ;; (the file header contains "---" as Elisp convention, so check defun line)
      (is (not (re-find #"defun.*---" result))
          "Triple-dash in function name must never appear (regression f48384d)"))))

;; ============================================================================
;; Gold: Destructuring
;; ============================================================================

(deftest gold-destructure-vector
  (testing "vector destructuring"
    (let [result (ae '(let [[a b] [1 2]] (+ a b)))]
      (is (str/includes? result "nth"))
      (is (str/includes? result "(+ a b)")))))

(deftest gold-destructure-map
  (testing "map destructuring"
    (let [result (ae '(let [{:keys [x y]} {:x 1 :y 2}] (+ x y)))]
      (is (str/includes? result "clel-get"))
      (is (str/includes? result "(+ x y)")))))

;; ============================================================================
;; Gold: Multi-arity
;; ============================================================================

(deftest gold-multi-arity
  (testing "multi-arity defn uses cl-case dispatch"
    (let [result (ae '(defn greet
                        ([name] (str "Hello, " name))
                        ([greeting name] (str greeting ", " name))))]
      (is (str/includes? result "&rest args"))
      (is (str/includes? result "cl-case"))
      (is (str/includes? result "(length args)")))))

;; ============================================================================
;; Gold: Try/Catch
;; ============================================================================

(deftest gold-try-catch
  (testing "try/catch → condition-case"
    (is (= "(condition-case e\n    (/ 1 0)\n  (error \"err\"))"
           (ae '(try (/ 1 0) (catch Exception e "err")))))))

;; ============================================================================
;; Gold: Emacs interop special forms
;; ============================================================================

(deftest gold-save-excursion
  (testing "save-excursion"
    (is (= "(save-excursion\n    (goto-char (point-min))\n    (insert \"top\"))"
           (ae '(save-excursion (goto-char (point-min)) (insert "top")))))))

(deftest gold-with-current-buffer
  (testing "with-current-buffer"
    (is (= "(with-current-buffer buf\n    (buffer-string))"
           (ae '(with-current-buffer buf (buffer-string)))))))

(deftest gold-with-temp-buffer
  (testing "with-temp-buffer"
    (is (= "(with-temp-buffer\n    (insert \"temp\")\n    (buffer-string))"
           (ae '(with-temp-buffer (insert "temp") (buffer-string)))))))

;; ============================================================================
;; Gold: Atoms
;; ============================================================================

(deftest gold-atoms
  (testing "atom/deref" (is (= "(clel-deref (clel-atom 42))" (ae '(deref (atom 42))))))
  (testing "reset!"     (is (= "(clel-reset! a 10)"          (ae '(reset! a 10)))))
  (testing "swap!"      (is (= "(clel-swap! a 1+)"           (ae '(swap! a inc))))))

;; ============================================================================
;; Gold: Predicates
;; ============================================================================

(deftest gold-predicates
  (testing "zero?"          (is (= "(zerop x)"           (ae '(zero? x)))))
  (testing "pos?"           (is (= "(cl-plusp x)"        (ae '(pos? x)))))
  (testing "even?"          (is (= "(cl-evenp x)"        (ae '(even? x)))))
  (testing "some?"          (is (= "(clel-some-p x)"     (ae '(some? x)))))
  (testing "coll?"          (is (= "(clel-coll-p x)"     (ae '(coll? x))))))

;; ============================================================================
;; Gold: Higher-order functions
;; ============================================================================

(deftest gold-higher-order
  (testing "map"         (is (= "(clel-map 1+ coll)"                         (ae '(map inc coll)))))
  (testing "filter"      (is (= "(clel-filter cl-evenp coll)"                (ae '(filter even? coll)))))
  (testing "reduce"      (is (= "(clel-reduce + 0 coll)"                     (ae '(reduce + 0 coll)))))
  (testing "apply"       (is (= "(apply + args)"                             (ae '(apply + args)))))
  (testing "partial"     (is (= "(apply-partially + 1)"                      (ae '(partial + 1)))))
  (testing "comp"        (is (= "(clel-comp 1+ 1-)"                          (ae '(comp inc dec)))))
  (testing "complement"  (is (= "(clel-complement null)"                     (ae '(complement nil?)))))
  (testing "identity"    (is (= "(identity x)"                               (ae '(identity x))))))

;; ============================================================================
;; Gold: Threading macros
;; ============================================================================

(deftest gold-threading
  (testing "-> thread-first"
    (is (= "(* (+ x 1) 2)"
           (ae '(-> x (+ 1) (* 2))))))
  (testing "->> thread-last"
    (is (= "(clel-filter cl-evenp (clel-map 1+ coll))"
           (ae '(->> coll (map inc) (filter even?)))))))

;; ============================================================================
;; Gold: Protocols & Records
;; ============================================================================

(deftest gold-defprotocol
  (testing "defprotocol → cl-defgeneric"
    (is (= "(cl-defgeneric greet (this name))"
           (ae '(defprotocol IGreeter (greet [this name])))))))

(deftest gold-defrecord
  (testing "defrecord → cl-defstruct + constructors"
    (let [result (ae '(defrecord Person [name age]))]
      (is (str/includes? result "cl-defstruct"))
      (is (str/includes? result "Person--create"))
      (is (str/includes? result "->Person"))
      (is (str/includes? result "map->Person")))))

;; ============================================================================
;; Gold: Quote
;; ============================================================================

(deftest gold-quote
  (testing "quote symbol"  (is (= "'foo"       (ae '(quote foo)))))
  (testing "quote list"    (is (= "'(a b c)"   (ae '(quote (a b c)))))))

;; ============================================================================
;; Gold: Throw
;; ============================================================================

(deftest gold-throw
  (testing "throw → signal"
    (is (= "(signal 'error (new Exception \"boom\"))"
           (ae '(throw (Exception. "boom")))))))

;; ============================================================================
;; Gold: String operations
;; ============================================================================

(deftest gold-string-ops
  (testing "str/join"
    (is (= "(clel-str-join \", \" coll)"
           (ae '(clojure.string/join ", " coll))))))
