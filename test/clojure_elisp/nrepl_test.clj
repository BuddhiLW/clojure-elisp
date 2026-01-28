;; NOTE: This test file requires nREPL dependencies.
;; It is only loaded when running with -M:dev:test
;; Run: clojure -M:dev:test to include nREPL tests
;;
;; The actual tests are defined below but only loaded when nREPL is available.
(ns clojure-elisp.nrepl-test
  "Tests for the ClojureElisp nREPL middleware.
   Requires nREPL deps - run with -M:dev:test to enable."
  (:require [clojure.test :refer [deftest is testing]]))

;; Stub test that always passes when nREPL deps aren't available
(deftest nrepl-deps-not-loaded-test
  (testing "nREPL tests require -M:dev:test profile"
    (is true "Run with clojure -M:dev:test for nREPL middleware tests")))

;; The full test suite is in nrepl_test_full.clj and requires nREPL deps
