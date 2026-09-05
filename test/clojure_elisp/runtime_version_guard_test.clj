(ns clojure-elisp.runtime-version-guard-test
  "Guards kanban 20260905161109-65e51703: emitted code must be able to tell that
   the runtime it loaded is too old to run it.

   Bundled beside its application the runtime is always the one that compiled
   the file, so this cannot bite. It bites the moment the runtime is package
   managed, which is the MELPA goal: MELPA ships HEAD and MELPA Stable ships
   the latest tag, so a 0.9 file can meet a 0.7 runtime.

   These assert the emitted STRING and the stamped constant. That the guard
   actually refuses to load lives in test/elisp/clojure-elisp-runtime-guard-test.el,
   which runs it in Emacs."
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [clojure-elisp.core :as clel]
            [clojure-elisp.version :as version]))

(deftest runtime-announces-its-own-version
  (let [el (slurp "resources/clojure-elisp/clojure-elisp-runtime.el")]
    (testing "the runtime defines the constant compiled files read"
      (is (str/includes? el (str "(defconst " version/runtime-version-symbol " \""))))
    (testing "the constant carries the project version, not a placeholder"
      (is (str/includes? el (str "(defconst " version/runtime-version-symbol " \""
                                 (str/trim (slurp "VERSION")) "\""))))))

(deftest emitted-files-guard-the-runtime-version
  (let [el (clel/compile-file-string "(ns my.pkg)\n(defn f [] 1)")]
    (testing "the runtime is still required"
      (is (str/includes? el "(require 'clojure-elisp-runtime)")))
    (testing "the guard names the minimum runtime this compiler emits against"
      (is (str/includes? el (str "(version<= \"" version/minimum-runtime-version "\" "
                                 version/runtime-version-symbol ")"))))
    (testing "the read is boundp-guarded — a pre-guard runtime does not define
              the constant, and an unguarded read would signal void-variable,
              which is the undiagnosable failure the guard exists to replace"
      (is (str/includes? el (str "(boundp '" version/runtime-version-symbol ")"))))
    (testing "the guard runs at load AND compile time, so byte-compiling a
              stale combination fails too"
      (is (str/includes? el "(eval-and-compile")))
    (testing "the error says which runtime is installed and which is needed"
      (is (str/includes? el "is too old for this file")))))

(deftest minimum-runtime-version-is-not-the-project-version
  (testing "the minimum is a deliberate constant, never derived from VERSION:
            every compiled file carries it, so deriving it would rewrite all
            output on every patch release and make the message a lie"
    (is (re-matches #"\d+\.\d+\.\d+" version/minimum-runtime-version))
    (is (not (neg? (compare (str/trim (slurp "VERSION"))
                            version/minimum-runtime-version)))
        (str "minimum-runtime-version " version/minimum-runtime-version
             " is ahead of the project VERSION — no released runtime satisfies "
             "the guard, so every compiled file would refuse to load."))))
