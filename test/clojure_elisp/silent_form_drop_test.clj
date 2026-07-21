(ns clojure-elisp.silent-form-drop-test
  "The analyzer must never discard a subform without saying so. Two shapes of
   compliance, depending on whether the extra forms have a meaning:

   `if` — Elisp gives it an implicit-progn else, (if COND THEN ELSE...), so the
   tail HAS a meaning and is kept. Stated as an induction:
     base case   (if c a b)          emits (if c a b)
     n-th case   (if c a b1 ... bn)  emits exactly what (if c a (do b1 ... bn))
                                     emits — the n-th case IS the base case with
                                     its tail folded.
   That equality is the metamorphic relation pinned below.

   `setq` / `setf` — take pairs, so an odd trailing form is malformed in both
   languages and has no meaning to preserve. Those throw instead."
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [clojure.test.check.generators :as gen]
            [hive-test.properties :as props]
            [clojure-elisp.analyzer :as ana]
            [clojure-elisp.emitter :as emit]))

(defn ae
  "Analyze form and emit to Elisp string."
  [form]
  (-> form ana/analyze emit/emit))

(defn- squeeze
  "Collapse whitespace runs so assertions pin structure, not line breaks."
  [s]
  (str/trim (str/replace s #"\s+" " ")))

(defn- fold-tail
  "The induction: fold an n-ary if's else-tail into a single `do` form.
   An absent else stays absent — injecting an explicit nil would change it."
  [[_if test then & else]]
  (cond
    (empty? else)      (list 'if test then)
    (= 1 (count else)) (list 'if test then (first else))
    :else              (list 'if test then (cons 'do else))))

;; ============================================================================
;; Unit — the shapes observed in hive-emacs
;; ============================================================================

(deftest n-ary-if-keeps-every-else-form
  (testing "the base case is unchanged"
    (is (= "(if c a b)" (ae '(if c a b)))))

  (testing "a 4-form if folds its tail into progn rather than dropping it"
    (is (= "(if c a (progn b d))" (squeeze (ae '(if c a b d))))))

  (testing "every trailing form survives, however many"
    (let [out (ae '(if c a b d e f))]
      (doseq [sym ["b" "d" "e" "f"]]
        (is (str/includes? out sym)
            (str "form " sym " was dropped from " (pr-str out))))))

  (testing "hive-emacs commentary-interactive shape — display-buffer survives"
    (is (str/includes?
         (ae '(if (plist-get result :error)
                (message "%s" (plist-get result :error))
                (with-current-buffer (get-buffer-create "*Docs*")
                  (goto-char (point-min)))
                (display-buffer "*Docs*")))
         "display-buffer")))

  (testing "a two-armed if still collapses to when"
    (is (= "(when c a)" (ae '(if c a))))))

;; ============================================================================
;; Unit — pair forms reject an odd tail rather than truncating it
;; ============================================================================

(deftest odd-pair-forms-fail-loud
  (testing "setq with an unpaired trailing form throws, naming the arity"
    (let [e (is (thrown? clojure.lang.ExceptionInfo (ae '(setq a 1 b))))]
      (is (str/includes? (ex-message e) "setq"))))

  (testing "setf with an unpaired trailing form throws"
    (let [e (is (thrown? clojure.lang.ExceptionInfo (ae '(setf a 1 b))))]
      (is (str/includes? (ex-message e) "setf"))))

  (testing "even pair counts are untouched"
    (is (= "(setq a 1 b 2)" (squeeze (ae '(setq a 1 b 2)))))
    (is (= "(setf a 1 b 2)" (squeeze (ae '(setf a 1 b 2))))))

  (testing "a single pair still compiles"
    (is (= "(setq a 1)" (squeeze (ae '(setq a 1)))))))

;; ============================================================================
;; Metamorphic — the induction itself, over generated forms
;; ============================================================================

(def gen-atom
  (gen/elements '[a b c d e f nil t 1 2 "s"]))

(def gen-n-ary-if
  "(if test then else1 ... elseN) with N from 0 to 5."
  (gen/let [test  gen-atom
            then  gen-atom
            elses (gen/vector gen-atom 0 5)]
    (concat (list 'if test then) elses)))

(props/defprop-metamorphic n-ary-if-equals-folded-base-case
  ae
  fold-tail
  =
  gen-n-ary-if
  {:num-tests 300})

;; ============================================================================
;; Conservation — no subform is ever discarded
;; ============================================================================

(defn- leaf-tokens
  "Every non-nil leaf symbol/literal in FORM, as strings."
  [form]
  (if (seq? form)
    (mapcat leaf-tokens form)
    (when (and (some? form) (not= 'if form) (not= 'do form))
      [(pr-str form)])))

(props/defprop-invariant n-ary-if-conserves-every-leaf
  (gen/fmap (fn [f] [f [f]]) gen-n-ary-if)
  (fn [_ f] f)
  (fn [f]
    (let [out (ae f)]
      (every? #(str/includes? out %) (leaf-tokens f))))
  {:num-tests 300})
