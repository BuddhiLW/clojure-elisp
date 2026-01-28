(ns clojure-elisp.nrepl-test
  "Tests for the ClojureElisp nREPL middleware."
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [clojure.string :as str]
            [clojure-elisp.nrepl :as nrepl]
            [nrepl.transport :as t]))

;; Reset session state between tests
(use-fixtures :each
  (fn [f]
    (reset! nrepl/cljel-sessions #{})
    (f)))

;; ============================================================================
;; Helper: recording transport for testing middleware handlers
;; ============================================================================

(defn recording-transport
  "Create a mock nREPL transport that records all sent messages.
   Returns {:transport <transport> :messages <atom of messages>}."
  []
  (let [messages (atom [])]
    {:transport (reify t/Transport
                  (send [_ msg] (swap! messages conj msg)))
     :messages messages}))

(defn find-response
  "Find the first response message containing the given key."
  [messages k]
  (first (filter #(contains? % k) @messages)))

;; ============================================================================
;; compile-code — pure function, no nREPL deps
;; ============================================================================

(deftest compile-code-simple-forms-test
  (testing "compiles arithmetic expression"
    (let [result (nrepl/compile-code "(+ 1 2)")]
      (is (= :ok (:status result)))
      (is (= "(+ 1 2)" (:elisp result)))))

  (testing "compiles string literal"
    (let [result (nrepl/compile-code "\"hello\"")]
      (is (= :ok (:status result)))
      (is (= "\"hello\"" (:elisp result)))))

  (testing "compiles defn to defun"
    (let [result (nrepl/compile-code "(defn foo [x] (+ x 1))")]
      (is (= :ok (:status result)))
      (is (string? (:elisp result)))
      (is (str/includes? (:elisp result) "defun"))
      (is (str/includes? (:elisp result) "foo"))))

  (testing "compiles multiple forms"
    (let [result (nrepl/compile-code "(def x 1) (def y 2)")]
      (is (= :ok (:status result)))
      (is (str/includes? (:elisp result) "defvar"))))

  (testing "compiles core fn mapping"
    (let [result (nrepl/compile-code "(first xs)")]
      (is (= :ok (:status result)))
      (is (= "(clel-first xs)" (:elisp result))))))

(deftest compile-code-error-test
  (testing "returns error for invalid syntax"
    (let [result (nrepl/compile-code "(def")]
      (is (= :error (:status result)))
      (is (string? (:error result)))))

  (testing "returns error for unbalanced parens"
    (let [result (nrepl/compile-code "(+ 1 2")]
      (is (= :error (:status result)))))

  (testing "returns error for unreadable input"
    (let [result (nrepl/compile-code "@@@")]
      (is (= :error (:status result))))))

;; ============================================================================
;; Session management
;; ============================================================================

(deftest session-management-test
  (testing "session starts inactive"
    (is (not (nrepl/cljel-active? "test-session"))))

  (testing "session becomes active after adding"
    (swap! nrepl/cljel-sessions conj "test-session")
    (is (nrepl/cljel-active? "test-session")))

  (testing "session becomes inactive after removing"
    (swap! nrepl/cljel-sessions disj "test-session")
    (is (not (nrepl/cljel-active? "test-session"))))

  (testing "multiple sessions tracked independently"
    (swap! nrepl/cljel-sessions conj "sess-1")
    (swap! nrepl/cljel-sessions conj "sess-2")
    (is (nrepl/cljel-active? "sess-1"))
    (is (nrepl/cljel-active? "sess-2"))
    (swap! nrepl/cljel-sessions disj "sess-1")
    (is (not (nrepl/cljel-active? "sess-1")))
    (is (nrepl/cljel-active? "sess-2"))))

;; ============================================================================
;; Middleware handler — start/stop ops
;; ============================================================================

(deftest wrap-cljel-start-stop-test
  (let [handler (nrepl/wrap-cljel identity)]

    (testing "cljel-start activates session"
      (let [{:keys [transport messages]} (recording-transport)]
        (handler {:op "cljel-start"
                  :session "test-sess"
                  :transport transport
                  :id "1"})
        (is (nrepl/cljel-active? "test-sess"))
        (is (some #(= "ClojureElisp session started" (:value %)) @messages))))

    (testing "cljel-stop deactivates session"
      (let [{:keys [transport messages]} (recording-transport)]
        (handler {:op "cljel-stop"
                  :session "test-sess"
                  :transport transport
                  :id "2"})
        (is (not (nrepl/cljel-active? "test-sess")))
        (is (some #(= "ClojureElisp session stopped" (:value %)) @messages))))))

;; ============================================================================
;; Middleware handler — eval op
;; ============================================================================

(deftest wrap-cljel-eval-active-test
  (let [handler (nrepl/wrap-cljel identity)]
    ;; Activate session first
    (let [{:keys [transport]} (recording-transport)]
      (handler {:op "cljel-start"
                :session "eval-sess"
                :transport transport
                :id "0"}))

    (testing "eval compiles CLJEL when session active"
      (let [{:keys [transport messages]} (recording-transport)]
        (handler {:op "eval"
                  :code "(+ 1 2)"
                  :session "eval-sess"
                  :transport transport
                  :id "1"})
        (let [value-msg (find-response messages :value)]
          (is (some? value-msg))
          (is (= "(+ 1 2)" (:value value-msg)))
          (is (= "(+ 1 2)" (:cljel-compiled-elisp value-msg)))
          (is (= "user" (:ns value-msg))))))

    (testing "eval includes :done status"
      (let [{:keys [transport messages]} (recording-transport)]
        (handler {:op "eval"
                  :code "42"
                  :session "eval-sess"
                  :transport transport
                  :id "2"})
        (is (some #(= #{:done} (:status %)) @messages))))

    (testing "eval returns error for invalid code"
      (let [{:keys [transport messages]} (recording-transport)]
        (handler {:op "eval"
                  :code "(def"
                  :session "eval-sess"
                  :transport transport
                  :id "3"})
        (let [err-msg (find-response messages :err)]
          (is (some? err-msg))
          (is (str/includes? (:err err-msg) "Compilation error")))))))

(deftest wrap-cljel-eval-passthrough-test
  (testing "eval passes through when session not active"
    (let [passthrough (atom nil)
          handler (nrepl/wrap-cljel (fn [msg] (reset! passthrough msg)))]
      (handler {:op "eval"
                :code "(+ 1 2)"
                :session "inactive-sess"
                :id "1"})
      (is (some? @passthrough))
      (is (= "(+ 1 2)" (:code @passthrough))))))

;; ============================================================================
;; Middleware handler — load-file op
;; ============================================================================

(deftest wrap-cljel-load-file-test
  (let [handler (nrepl/wrap-cljel identity)]
    ;; Activate session
    (let [{:keys [transport]} (recording-transport)]
      (handler {:op "cljel-start"
                :session "load-sess"
                :transport transport
                :id "0"}))

    (testing "load-file compiles file content when active"
      (let [{:keys [transport messages]} (recording-transport)]
        (handler {:op "load-file"
                  :file "(defn foo [x] (+ x 1))"
                  :file-name "test.cljel"
                  :file-path "/tmp/test.cljel"
                  :session "load-sess"
                  :transport transport
                  :id "1"})
        (let [value-msg (find-response messages :value)]
          (is (some? value-msg))
          (is (str/includes? (:value value-msg) "defun"))
          (is (str/includes? (:cljel-compiled-elisp value-msg) "defun")))))

    (testing "load-file passes through when session not active"
      (let [passthrough (atom nil)
            handler (nrepl/wrap-cljel (fn [msg] (reset! passthrough msg)))]
        (handler {:op "load-file"
                  :file "(+ 1 2)"
                  :session "other-sess"
                  :id "2"})
        (is (some? @passthrough))
        (is (= "load-file" (:op @passthrough)))))))

;; ============================================================================
;; Middleware handler — unknown ops passthrough
;; ============================================================================

(deftest wrap-cljel-passthrough-test
  (testing "non-CLJEL ops pass through to next handler"
    (let [passthrough (atom nil)
          handler (nrepl/wrap-cljel (fn [msg] (reset! passthrough msg)))]
      (handler {:op "complete"
                :prefix "clojure.c"
                :session "any-sess"
                :id "1"})
      (is (some? @passthrough))
      (is (= "complete" (:op @passthrough))))))
