(ns clojure-elisp.mcp-test
  "Tests for the MCP stdio server: derived inputSchema wire-identity and the
   pre-dispatch argument validation."
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-elisp.mcp :as mcp]
            [clojure.string :as str]))

;; Private helpers under test.
(def ^:private to-json       @#'mcp/to-json)
(def ^:private handle-message @#'mcp/handle-message)
(def ^:private respond       @#'mcp/respond)

;; The intended wire `inputSchema` — the single source of truth the Malli
;; derivation must serialize to byte-for-byte. The schemas are `:closed`, so each
;; carries `additionalProperties:false` (keys ordered type→properties→required→
;; additionalProperties, matching the cond-> order in malli->input-schema).
(def ^:private hand-written-tools
  [{"name" "compile_string"
    "description" "Compile a ClojureElisp code string to Emacs Lisp"
    "inputSchema" {"type" "object"
                   "properties" {"code" {"type" "string"
                                         "description" "ClojureElisp source code to compile"}}
                   "required" ["code"]
                   "additionalProperties" false}}
   {"name" "compile_file"
    "description" "Compile a .cljel file to a .el file"
    "inputSchema" {"type" "object"
                   "properties" {"input" {"type" "string"
                                          "description" "Path to input .cljel file"}
                                 "output" {"type" "string"
                                           "description" "Path to output .el file"}}
                   "required" ["input" "output"]
                   "additionalProperties" false}}
   {"name" "analyze"
    "description" "Analyze ClojureElisp code and return the AST as EDN"
    "inputSchema" {"type" "object"
                   "properties" {"code" {"type" "string"
                                         "description" "ClojureElisp source code to analyze"}}
                   "required" ["code"]
                   "additionalProperties" false}}])

(deftest derived-input-schema-is-wire-identical
  (testing "the Malli-derived inputSchema serializes byte-for-byte like the hand-written one"
    (is (= (to-json {"tools" hand-written-tools})
           (to-json {"tools" mcp/tools}))))
  (testing "tools/list response is byte-identical to the pre-change wire form"
    (is (= (to-json (respond 1 {"tools" hand-written-tools}))
           (to-json (handle-message {"method" "tools/list" "id" 1}))))))

(deftest valid-call-behaviour-preserved
  (testing "a well-formed compile_string call returns isError false and the compiled elisp"
    (let [resp (handle-message {"method" "tools/call" "id" 2
                                "params" {"name" "compile_string"
                                          "arguments" {"code" "(+ 1 2)"}}})
          result (get resp "result")]
      (is (false? (get result "isError")))
      (is (= "text" (get-in result ["content" 0 "type"])))
      (is (string? (get-in result ["content" 0 "text"]))))))

(deftest invalid-arguments-rejected-before-dispatch
  (testing "missing required key -> humanized error, isError true, compiler not invoked"
    (let [resp (handle-message {"method" "tools/call" "id" 3
                                "params" {"name" "compile_string" "arguments" {}}})
          result (get resp "result")]
      (is (true? (get result "isError")))
      (is (str/starts-with? (get-in result ["content" 0 "text"]) "Invalid arguments:"))
      (is (str/includes? (get-in result ["content" 0 "text"]) "code"))))
  (testing "wrong-typed / missing second key on compile_file is rejected"
    (let [resp (handle-message {"method" "tools/call" "id" 4
                                "params" {"name" "compile_file"
                                          "arguments" {"input" "a.cljel"}}})
          result (get resp "result")]
      (is (true? (get result "isError")))
      (is (str/includes? (get-in result ["content" 0 "text"]) "output"))))
  (testing "an unknown extra argument is rejected by the closed :input schema"
    (let [resp (handle-message {"method" "tools/call" "id" 6
                                "params" {"name" "compile_string"
                                          "arguments" {"code" "(+ 1 2)" "bogus" 1}}})
          result (get resp "result")]
      (is (true? (get result "isError")))
      (is (str/includes? (get-in result ["content" 0 "text"]) "bogus")))))

(deftest unknown-tool-behaviour-preserved
  (testing "an unknown tool keeps the pre-existing isError=false Error content"
    (let [resp (handle-message {"method" "tools/call" "id" 5
                                "params" {"name" "does_not_exist" "arguments" {}}})
          result (get resp "result")]
      (is (false? (get result "isError")))
      (is (str/starts-with? (get-in result ["content" 0 "text"]) "Error: Unknown tool")))))
