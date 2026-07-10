(ns clojure-elisp.runtime-regen-test
  "Regression for kanban 20260710133513-7938b464: the self-hosted runtime must
   be cleanly regenerable from resources/clojure-elisp/runtime.cljel. A naive
   regen used to ship a BROKEN runtime because `nth`/`last` in core-fn-mapping
   rewrite to the very `clel-*` wrappers the runtime defines (self-recursion /
   wrong arg order), `clel-nth` and the Elisp-2 bridge defvars lived only in the
   hand-edited .el, and `defmethod`/lambda variadics emitted a bare `&`.

   These asserts compile the runtime SOURCE and prove the generated Elisp is
   self-hosting-safe, so runtime edits can go through .cljel + regen instead of
   hand-patching the .el. Functional (load-into-Emacs) verification lives in the
   batch harness; here we lock the emitted-string invariants."
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.string :as str]
            [clojure-elisp.core :as clel]))

(def ^:private runtime-src "resources/clojure-elisp/runtime.cljel")

(deftest runtime-regenerates-cleanly
  (let [el (clel/compile-file-string (slurp runtime-src))]
    (testing "compiles to a substantial runtime (not silently truncated)"
      (is (< 80 (count (re-seq #"\((?:cl-)?defun " el)))))
    (testing "runtime primitives defined in source, not hand-added to the .el"
      (is (re-find #"\(cl-defun clel-nth " el) "clel-nth must be defined in source")
      (is (str/includes? el "clojure-core-vector") "Elisp-2 bridge defvar present")
      (is (str/includes? el "clojure-core-list") "Elisp-2 bridge defvar present"))
    (testing "no self-recursion: primitives call raw elisp, not their clel- wrappers"
      (is (str/includes? el "(defun clel-deref"))
      (is (not (str/includes? el "(car (clel-last"))
          "clel-last must use raw (last ...), never (clel-last ...)")
      (is (not (re-find #"\(clel-nth \d" el))
          "atom/lazy accessors must use raw (nth N x), never (clel-nth N x)"))
    (testing "no bare `&` rest marker leaks into emitted lambdas/defmethods"
      (is (not (re-find #"\(& " el)) "bare `(& ` means a rest marker was not translated to &rest"))
    (testing "cond emits grouped clauses, never (cond (progn ...))"
      (is (not (str/includes? el "cond (progn"))))))
