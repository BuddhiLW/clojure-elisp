(ns clojure-elisp.repl-test
  "Tests for the ClojureElisp REPL compiler."
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-elisp.repl :as repl]))

;; ============================================================================
;; one-line
;; ============================================================================

(deftest one-line-test
  (testing "collapses multi-line to single line"
    (is (= "(defun foo (x) (+ x 1))"
           (repl/one-line "(defun foo (x)\n  (+ x 1))"))))
  (testing "preserves single-line input"
    (is (= "(+ 1 2)" (repl/one-line "(+ 1 2)"))))
  (testing "collapses indentation"
    (is (= "(let* ((a 1) (b 2)) (+ a b))"
           (repl/one-line "(let* ((a 1)\n        (b 2))\n    (+ a b))")))))

;; ============================================================================
;; compile-input
;; ============================================================================

(deftest compile-input-simple-forms-test
  (testing "compiles integer literal"
    (let [result (repl/compile-input "42")]
      (is (contains? result :ok))
      (is (= "42" (:ok result)))))

  (testing "compiles string literal"
    (let [result (repl/compile-input "\"hello\"")]
      (is (contains? result :ok))
      (is (= "\"hello\"" (:ok result)))))

  (testing "compiles nil"
    (let [result (repl/compile-input "nil")]
      (is (= "nil" (:ok result)))))

  (testing "compiles boolean true"
    (let [result (repl/compile-input "true")]
      (is (= "t" (:ok result)))))

  (testing "compiles boolean false"
    (let [result (repl/compile-input "false")]
      (is (= "nil" (:ok result))))))

(deftest compile-input-function-calls-test
  (testing "compiles arithmetic"
    (let [result (repl/compile-input "(+ 1 2)")]
      (is (contains? result :ok))
      (is (= "(+ 1 2)" (:ok result)))))

  (testing "compiles nested calls"
    (let [result (repl/compile-input "(+ (* 2 3) 4)")]
      (is (contains? result :ok))
      (is (clojure.string/includes? (:ok result) "(* 2 3)"))))

  (testing "compiles core fn mapping"
    (let [result (repl/compile-input "(first xs)")]
      (is (= "(clel-first xs)" (:ok result))))))

(deftest compile-input-def-forms-test
  (testing "compiles def"
    (let [result (repl/compile-input "(def x 42)")]
      (is (contains? result :ok))
      (is (clojure.string/includes? (:ok result) "defvar"))
      (is (clojure.string/includes? (:ok result) "42"))))

  (testing "compiles defn"
    (let [result (repl/compile-input "(defn foo [x] (+ x 1))")]
      (is (contains? result :ok))
      (is (clojure.string/includes? (:ok result) "defun"))
      (is (clojure.string/includes? (:ok result) "foo")))))

(deftest compile-input-let-test
  (testing "compiles let binding"
    (let [result (repl/compile-input "(let [a 1 b 2] (+ a b))")]
      (is (contains? result :ok))
      (is (clojure.string/includes? (:ok result) "let*")))))

(deftest compile-input-error-handling-test
  (testing "returns error for invalid syntax"
    (let [result (repl/compile-input "(def")]
      (is (contains? result :err))
      (is (string? (:err result)))))

  (testing "returns error for unbalanced parens"
    (let [result (repl/compile-input "(+ 1 2")]
      (is (contains? result :err))))

  (testing "returns error for unreadable input"
    (let [result (repl/compile-input "@@@")]
      (is (contains? result :err)))))

;; ============================================================================
;; -main output protocol
;; ============================================================================

(deftest main-output-protocol-test
  (testing "compiles valid form with CLJEL-OK: prefix"
    (let [output (with-out-str
                   (with-in-str "(+ 1 2)\n"
                     (repl/-main)))]
      (is (clojure.string/includes? output "CLJEL-READY"))
      (is (clojure.string/includes? output "CLJEL-OK:(+ 1 2)"))))

  (testing "reports error with CLJEL-ERR: prefix"
    (let [output (with-out-str
                   (with-in-str "(def\n"
                     (repl/-main)))]
      (is (clojure.string/includes? output "CLJEL-READY"))
      (is (clojure.string/includes? output "CLJEL-ERR:"))))

  (testing "skips blank lines"
    (let [output (with-out-str
                   (with-in-str "   \n(+ 1 2)\n"
                     (repl/-main)))]
      (is (clojure.string/includes? output "CLJEL-OK:(+ 1 2)"))
      ;; Only one OK line (blank line was skipped)
      (is (= 1 (count (re-seq #"CLJEL-OK:" output))))))

  (testing "handles multiple forms sequentially"
    (let [output (with-out-str
                   (with-in-str "42\n\"hello\"\n"
                     (repl/-main)))]
      (is (clojure.string/includes? output "CLJEL-OK:42"))
      (is (clojure.string/includes? output "CLJEL-OK:\"hello\""))))

  (testing "defn output is a single CLJEL-OK: line"
    (let [output   (with-out-str
                     (with-in-str "(defn foo [x] (+ x 1))\n"
                       (repl/-main)))
          ok-lines (->> (clojure.string/split-lines output)
                        (filter #(clojure.string/starts-with? % "CLJEL-OK:")))]
      ;; Exactly one CLJEL-OK: line (not split across multiple lines)
      (is (= 1 (count ok-lines)))
      ;; That line contains the full defun
      (is (clojure.string/includes? (first ok-lines) "defun"))
      (is (clojure.string/includes? (first ok-lines) "foo")))))
