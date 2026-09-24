(ns clojure-elisp.gensym
  "Deterministic generated names.

   `clojure.core/gensym`, the reader's `#()` and `x#`, and every JVM macro
   that calls `gensym` draw from one process-wide counter, so a generated name
   depends on everything the JVM compiled before it: the same file compiled
   twice emitted `p__31976`, then `p__32007`, and every committed .el churned
   on rebuild.

   Here a generated name is numbered by a counter scoped to ONE top-level form
   (`with-scope`, bound by the analyzer). The same form always yields the same
   names, and editing one form never renumbers another. One counter shared by
   every name a scope generates keeps those names distinct.

   Three sources are covered:
     `fresh`                   names the compiler itself makes (destructuring);
     `renumber-reader-gensyms` `p1__N#`, `rest__N#` and `x__N__auto__` that the
                               reader put into a form before analysis saw it;
     `renumber-expansion`      gensyms a macro expansion introduced."
  (:require [clojure.walk :as walk]))

(def ^:dynamic *counter*
  "Counter atom of the current top-level-form scope, or nil outside one."
  nil)

(defmacro with-scope
  "Evaluate body with a fresh name counter."
  [& body]
  `(binding [*counter* (atom 0)] ~@body))

(defn fresh
  "A new symbol: `prefix` followed by the scope counter's next value. Outside
   a scope (a direct call from a test or tool) it falls back to `gensym`."
  [prefix]
  (if-let [counter *counter*]
    (symbol (str prefix (swap! counter inc)))
    (gensym prefix)))

(def ^:private reader-gensym-re
  "`p1__123#` / `rest__123#` (`#()` params) and `x__123__auto__` (syntax-quote
   `x#`). The suffix makes these unmistakably reader-made."
  #"^(.+__)\d+(#|__auto__)$")

(def ^:private gensym-re
  "Any name shaped like a gensym: `G__123`, `pred__123`, `v__123__auto__`."
  #"^(.*__)\d+(#|__auto__)?$")

(defn- symbols-in [form]
  (into #{} (filter symbol?) (tree-seq coll? seq form)))

(defn- rename
  "form with every symbol in smap replaced. clojure.walk keeps collection
   metadata, which is where source line numbers ride."
  [form smap]
  (if (empty? smap) form (walk/postwalk-replace smap form)))

(defn- renumber
  "form with the unqualified symbols `generated?` accepts renamed, in order of
   first appearance, to their prefix plus a number from the scope counter."
  [form generated?]
  (let [olds (->> (tree-seq coll? seq form)
                  (filter #(and (symbol? %) (nil? (namespace %)) (generated? %)))
                  distinct)
        smap (into {}
                   (map (fn [old]
                          (let [[_ prefix suffix] (re-matches gensym-re (name old))]
                            [old (symbol (str (fresh prefix) suffix))])))
                   olds)]
    (rename form smap)))

(defn renumber-reader-gensyms
  "form with its reader-made gensyms renumbered from the scope counter."
  [form]
  (renumber form #(re-matches reader-gensym-re (name %))))

(defn renumber-expansion
  "expansion with the gensyms it introduced (gensym-shaped symbols absent from
   the form it expanded) renumbered from the scope counter. Symbols the
   original already carried are left alone: they are the user's, or were
   renumbered when the form holding them was."
  [original expansion]
  (if (identical? original expansion)
    expansion
    (let [before (symbols-in original)]
      (renumber expansion #(and (not (contains? before %))
                                (re-matches gensym-re (name %)))))))
