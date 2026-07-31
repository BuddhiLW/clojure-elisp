(ns clojure-elisp.regression-multiform-load-file-test
  "Regression: an nREPL load-file must compile the WHOLE buffer with namespace
   context, and every generated top-level form must reach the client.

   Two defects are pinned here:
     1. `nrepl/handle-load-file` used expression-level `core/compile-string`,
        which drops the (ns ...) context — definitions lost their namespace
        prefix and no (provide ...) was emitted.
     2. Clients evaluated only `(car (read-from-string elisp))`, installing
        the first generated form and silently discarding the rest."
  (:require [clojure-elisp.core :as core]
            [clojure-elisp.nrepl :as nrepl]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [nrepl.transport :as t]))

;; ============================================================================
;; Fixtures
;; ============================================================================

(def buffer-src
  "A representative .cljel buffer: ns, defvar, public + private defns,
   and a keymap built from a let."
  (str "(ns demo-mod\n"
       "  \"Sample multi-form buffer.\")\n"
       "\n"
       "(defvar demo-counter 0\n"
       "  \"A counter.\")\n"
       "\n"
       "(defn bump [n]\n"
       "  \"Bump the counter by N.\"\n"
       "  (setq demo-counter (+ demo-counter n)))\n"
       "\n"
       "(defn- helper [x]\n"
       "  \"Private helper.\"\n"
       "  (* x 2))\n"
       "\n"
       "(defvar demo-mode-map\n"
       "  (let [m (make-sparse-keymap)]\n"
       "    (define-key m (kbd \"C-c C-b\") 'demo-mod-bump)\n"
       "    m)\n"
       "  \"Keymap for demo mode.\")\n"))

(def expr-src "(+ 1 2)")

(defn- recording-transport
  "Transport that appends every sent message to SENT (an atom holding a vector)."
  [sent]
  (reify t/Transport
    (recv [_] nil)
    (recv [_ _] nil)
    (send [this msg] (swap! sent conj msg) this)))

(defn- run-op
  "Invoke F with a msg carrying a recording transport; return the sent messages."
  [f msg]
  (let [sent (atom [])]
    (f (assoc msg :transport (recording-transport sent)
                  :id "test-id"
                  :session "test-session"))
    @sent))

(defn- compiled-elisp
  "Extract the :cljel-compiled-elisp payload from a sequence of sent messages."
  [msgs]
  (some :cljel-compiled-elisp msgs))

;; ============================================================================
;; Compiler layer — file mode carries namespace context
;; ============================================================================

(deftest compile-file-string-applies-namespace-context-test
  (let [elisp (core/compile-file-string buffer-src)]
    (testing "public defns are namespace-prefixed"
      (is (str/includes? elisp "(defun demo-mod-bump (n)")))
    (testing "private defns use the double-dash convention"
      (is (str/includes? elisp "(defun demo-mod--helper (x)")))
    (testing "a trailing provide is emitted for the ns"
      (is (str/includes? elisp "(provide 'demo-mod)")))
    (testing "keymap definitions survive intact"
      (is (str/includes? elisp "(make-sparse-keymap)"))
      (is (str/includes? elisp "(define-key m (kbd \"C-c C-b\") 'demo-mod-bump)")))
    (testing "every top-level definition of the buffer is present"
      (doseq [needle ["(defvar demo-counter 0" "(defun demo-mod-bump (n)"
                      "(defun demo-mod--helper (x)" "(defvar demo-mode-map"]]
        (is (str/includes? elisp needle) (str "missing: " needle))))))

(deftest compile-string-is-still-expression-level-test
  (testing "expression mode neither prefixes nor provides"
    (let [elisp (core/compile-string buffer-src)]
      (is (str/includes? elisp "(defun bump (n)"))
      (is (not (str/includes? elisp "(provide 'demo-mod)"))))))

;; ============================================================================
;; Middleware layer — compile-code mode routing
;; ============================================================================

(deftest compile-code-default-mode-is-expression-test
  (testing "single-arity compile-code keeps the pre-existing expression behavior"
    (let [result (nrepl/compile-code expr-src)]
      (is (= :ok (:status result)))
      (is (str/includes? (:elisp result) "(+ 1 2)"))))
  (testing ":expr is the explicit name of that same behavior"
    (is (= (:elisp (nrepl/compile-code buffer-src))
           (:elisp (nrepl/compile-code buffer-src :expr))))))

(deftest compile-code-file-mode-uses-file-compiler-test
  (let [result (nrepl/compile-code buffer-src :file)]
    (is (= :ok (:status result)))
    (is (= (core/compile-file-string buffer-src) (:elisp result)))
    (is (str/includes? (:elisp result) "(provide 'demo-mod)"))))

(deftest compile-code-reports-errors-in-both-modes-test
  (doseq [mode [:expr :file]]
    (let [result (nrepl/compile-code "(defn broken [" mode)]
      (is (= :error (:status result)) (str "mode " mode))
      (is (string? (:error result)) (str "mode " mode)))))

;; ============================================================================
;; Middleware layer — op handlers
;; ============================================================================

(deftest handle-load-file-compiles-whole-buffer-test
  (let [elisp (compiled-elisp (run-op nrepl/handle-load-file {:file buffer-src}))]
    (is (some? elisp) "load-file must send compiled Elisp")
    (testing "namespaced defns reach the client"
      (is (str/includes? elisp "(defun demo-mod-bump (n)"))
      (is (str/includes? elisp "(defun demo-mod--helper (x)")))
    (testing "the provide form reaches the client"
      (is (str/includes? elisp "(provide 'demo-mod)")))
    (testing "the keymap reaches the client"
      (is (str/includes? elisp "(define-key m (kbd \"C-c C-b\") 'demo-mod-bump)")))))

(deftest handle-eval-stays-expression-level-test
  (let [elisp (compiled-elisp (run-op nrepl/handle-eval {:code expr-src}))]
    (is (some? elisp))
    (is (str/includes? elisp "(+ 1 2)"))
    (is (not (str/includes? elisp "(provide ")))))

(deftest wrap-cljel-routes-load-file-only-for-active-sessions-test
  (let [passthrough (atom [])
        handler     (nrepl/wrap-cljel (fn [msg] (swap! passthrough conj msg)))]
    (testing "inactive session falls through to the wrapped handler"
      (swap! nrepl/cljel-sessions disj "test-session")
      (handler {:op "load-file" :session "test-session" :file buffer-src})
      (is (= 1 (count @passthrough))))
    (testing "active session compiles the whole buffer"
      (try
        (swap! nrepl/cljel-sessions conj "test-session")
        (let [sent (atom [])]
          (handler {:op "load-file"
                    :session "test-session"
                    :id "test-id"
                    :file buffer-src
                    :transport (recording-transport sent)})
          (is (= 1 (count @passthrough)) "must not fall through")
          (is (str/includes? (compiled-elisp @sent) "(provide 'demo-mod)")))
        (finally
          (swap! nrepl/cljel-sessions disj "test-session"))))))

;; ============================================================================
;; Client contract — every generated top-level form must be evaluable
;; ============================================================================

(deftest compiled-buffer-has-multiple-top-level-forms-test
  (testing "the payload a client receives is a multi-form program, so reading
            only the first form drops definitions"
    (let [elisp (core/compile-file-string buffer-src)
          forms (with-open [r (java.io.PushbackReader. (java.io.StringReader. elisp))]
                  (doall (take-while #(not= ::eof %)
                                     (repeatedly #(read {:read-cond :allow
                                                         :eof ::eof}
                                                        r)))))]
      (is (< 1 (count forms)))
      (is (some #(= '(provide 'demo-mod) %) forms)
          "provide must be one of the forms a client is expected to eval"))))
