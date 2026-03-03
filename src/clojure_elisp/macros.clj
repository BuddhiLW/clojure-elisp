(ns clojure-elisp.macros
  "Compile-time macro registry and expansion for ClojureElisp.

   Macros are registered during analysis and expanded before
   forms are analyzed. The registry is global (atom-based).")

;; ============================================================================
;; Macro Registry
;; ============================================================================

(defonce ^{:doc "Compile-time macro registry. Maps symbol -> macro fn."}
  macro-registry
  (atom {}))

(defonce ^{:doc "Built-in macros that survive clear-macros!."}
  builtin-macros
  (atom {}))

(defn register-builtin-macro!
  "Register a macro fn as built-in (survives clear-macros!)."
  [sym macro-fn]
  (swap! builtin-macros assoc sym macro-fn)
  (swap! macro-registry assoc sym macro-fn))

(defn clear-macros!
  "Clear user-defined macros. Built-in macros are preserved."
  []
  (reset! macro-registry @builtin-macros))

(defn get-macro
  "Look up a macro by name. Returns the macro fn or nil."
  [sym]
  (get @macro-registry sym))

(defn register-macro!
  "Register a macro fn under the given name."
  [sym macro-fn]
  (swap! macro-registry assoc sym macro-fn))

;; ============================================================================
;; Macro Expansion
;; ============================================================================

(defn macroexpand-1-clel
  "Expand a ClojureElisp macro form by one step.
   If the form is a list whose first element is a registered macro,
   applies the macro fn and returns the result. Otherwise returns
   the form unchanged."
  [form]
  (if (and (seq? form)
           (symbol? (first form)))
    (if-let [macro-fn (get-macro (first form))]
      (apply macro-fn (rest form))
      form)
    form))

(defn macroexpand-clel
  "Fully expand a ClojureElisp macro form.
   Repeatedly applies macroexpand-1-clel until the form stops changing."
  [form]
  (let [expanded (macroexpand-1-clel form)]
    (if (identical? expanded form)
      form
      (recur expanded))))
