(ns clojure-elisp.nrepl-ns-parity-test
  "An interactively evaluated definition must install the SAME Elisp name that
   compiling its buffer installs.

   Before this, eval always compiled in :expr mode while load-file compiled in
   :file mode, so C-c C-c defined `greet` and C-c C-k defined `my-pkg-greet`.
   The running image and the compiled artifact disagreed about every namespaced
   definition."
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [clojure-elisp.core :as core]
            [clojure-elisp.nrepl-kernel :as kernel]))

(def ns-src "(ns my.pkg)")
(def defn-src "(defn greet [n] (str \"hi \" n))")
(def buffer-src (str ns-src "\n" defn-src))

(defn- compiled-elisp
  "Pull the compiled Elisp out of a kernel response vector."
  [responses]
  (some :cljel-compiled-elisp responses))

;; ============================================================================
;; compile-string-in-ns
;; ============================================================================

(deftest in-ns-compilation-matches-file-compilation-test
  (testing "the emitted defun name is the same one the compiled buffer gets"
    (let [interactive (core/compile-string-in-ns ns-src defn-src)
          whole-file  (core/compile-file-string buffer-src)]
      (is (str/includes? interactive "(defun my-pkg-greet (n)"))
      (is (str/includes? whole-file "(defun my-pkg-greet (n)"))))
  (testing "no file scaffolding leaks into an interactive result"
    (let [interactive (core/compile-string-in-ns ns-src defn-src)]
      (is (not (str/includes? interactive "(provide ")))
      (is (not (str/includes? interactive "lexical-binding")))
      (is (not (str/includes? interactive "ends here"))))))

(deftest in-ns-without-context-is-unchanged-test
  (testing "nil and blank ns-source keep the pre-existing bare behavior"
    (doseq [empty-ns [nil ""]]
      (is (= (core/compile-string defn-src)
             (core/compile-string-in-ns empty-ns defn-src))
          (str "ns-source " (pr-str empty-ns))))))

(deftest in-ns-compiles-multiple-forms-test
  (testing "every form in the request is emitted, in order"
    (let [elisp (core/compile-string-in-ns
                 ns-src
                 (str defn-src "\n(defn shout [n] (upcase (greet n)))"))]
      (is (str/includes? elisp "(defun my-pkg-greet (n)"))
      (is (str/includes? elisp "(defun my-pkg-shout (n)"))
      (is (< (str/index-of elisp "my-pkg-greet")
             (str/index-of elisp "my-pkg-shout"))))))

(deftest in-ns-honours-requires-test
  (testing "an ns :require alias resolves for the evaluated form"
    (let [elisp (core/compile-string-in-ns
                 "(ns my.pkg (:require [clojure.string :as s]))"
                 "(defn shout [n] (s/upper-case n))")]
      (is (str/includes? elisp "(defun my-pkg-shout (n)")))))

;; ============================================================================
;; Kernel op routing
;; ============================================================================

(deftest handle-eval-uses-buffer-ns-when-supplied-test
  (testing ":cljel-ns compiles the form in that namespace"
    (let [elisp (compiled-elisp
                 (kernel/handle-eval {:code defn-src :cljel-ns ns-src}))]
      (is (str/includes? elisp "(defun my-pkg-greet (n)"))))
  (testing "without :cljel-ns the form stays bare"
    (let [elisp (compiled-elisp (kernel/handle-eval {:code defn-src}))]
      (is (str/includes? elisp "(defun greet (n)"))))
  (testing "a blank :cljel-ns is treated as absent"
    (let [elisp (compiled-elisp
                 (kernel/handle-eval {:code defn-src :cljel-ns "   "}))]
      (is (str/includes? elisp "(defun greet (n)")))))

(deftest eval-and-load-file-agree-on-the-name-test
  (testing "the two keybindings define the same function"
    (let [from-eval (compiled-elisp
                     (kernel/handle-eval {:code defn-src :cljel-ns ns-src}))
          from-load (compiled-elisp
                     (kernel/handle-load-file {:file buffer-src}))]
      (is (str/includes? from-eval "(defun my-pkg-greet (n)"))
      (is (str/includes? from-load "(defun my-pkg-greet (n)")))))

(deftest handle-eval-reports-compilation-errors-test
  (let [responses (kernel/handle-eval {:code "(defn broken [" :cljel-ns ns-src})]
    (is (nil? (compiled-elisp responses)))
    (is (some :err responses))
    (is (str/includes? (some :err responses) "Compilation error:"))))

;; ============================================================================
;; Kernel session dispatch
;; ============================================================================

(deftest handle-op-only-intercepts-active-sessions-test
  (let [session "parity-test-session"]
    (kernel/deactivate! session)
    (testing "an inactive session is not ours to answer"
      (is (nil? (kernel/handle-op {:op "eval" :session session :code defn-src})))
      (is (nil? (kernel/handle-op {:op "load-file" :session session
                                   :file buffer-src}))))
    (testing "cljel-start activates it"
      (is (some? (kernel/handle-op {:op "cljel-start" :session session})))
      (is (kernel/cljel-active? session))
      (is (some? (kernel/handle-op {:op "eval" :session session
                                    :code defn-src}))))
    (testing "cljel-stop deactivates it again"
      (is (some? (kernel/handle-op {:op "cljel-stop" :session session})))
      (is (not (kernel/cljel-active? session)))
      (is (nil? (kernel/handle-op {:op "eval" :session session
                                   :code defn-src}))))
    (testing "an unrelated op is never ours"
      (kernel/activate! session)
      (is (nil? (kernel/handle-op {:op "describe" :session session})))
      (kernel/deactivate! session))))

;; ============================================================================
;; Lightweight-host constraint
;; ============================================================================

(deftest compile-path-does-not-require-test-check-test
  (testing "no namespace on the compile path pulls in malli.generator"
    ;; test.check is absent from Babashka and ClojureWasm, so a load-time
    ;; require of malli.generator would take `clel nrepl` down with it.
    (doseq [ns-sym '[clojure-elisp.core clojure-elisp.compile clojure-elisp.ast
                     clojure-elisp.analyzer clojure-elisp.emitter
                     clojure-elisp.schema clojure-elisp.nrepl-kernel]]
      (require ns-sym)
      (is (not (contains? (set (map str (vals (ns-aliases (find-ns ns-sym)))))
                          "malli.generator"))
          (str ns-sym " must not alias malli.generator")))))
