(ns clojure-elisp.core-macros
  "Portable expanders for the clojure.core macros that introduce names.

   The analyzer hands any clojure.core macro it has no analyzer for to the
   host's macroexpand. For these that was not portable: each host's
   expansion names its temporaries from its own gensym counter (G__8237 on
   one run, G__19400 on the next, cond_thread__2482__auto__ on ClojureWasm),
   and ClojureWasm's expansions also differ in shape (its if-not swaps the
   branches instead of calling not). The same source compiled to different
   Elisp per process and per host.

   These follow clojure.core 1.12's definitions form for form, so JVM output
   keeps its shape; only generated names change, and those come from
   clojure-elisp.names, numbered per compilation. Forms are built with list
   and quoted symbols rather than syntax-quote, which each host would expand
   and resolve its own way."
  (:require [clojure-elisp.names :as names]))

(defn- gsym
  "(gensym) as clojure.core/gensym names it, from the compilation's counter."
  []
  (names/fresh-symbol "G__"))

(defn- auto-sym
  "An auto-gensym (temp# in a syntax-quote), numbered per compilation."
  [prefix]
  (symbol (str prefix "__" (names/next-id) "__auto__")))

(defn- thread-steps
  "The let form cond->, some-> and relatives expand to: g bound to expr, then
   rebound by every step but the last, which is the body."
  [g expr steps]
  (list 'clojure.core/let
        (into [g expr] (interleave (repeat g) (butlast steps)))
        (if (empty? steps) g (last steps))))

(defn- cond-thread
  [threader expr clauses]
  (when-not (even? (count clauses))
    (throw (ex-info "cond-> requires an even number of clauses" {:clauses clauses})))
  (let [g     (gsym)
        steps (map (fn [[test step]] (list 'if test (list threader g step) g))
                   (partition 2 clauses))]
    (thread-steps g expr steps)))

(defn- some-thread
  [threader expr forms]
  (let [g     (gsym)
        steps (map (fn [step] (list 'if (list 'clojure.core/nil? g) nil (list threader g step)))
                   forms)]
    (thread-steps g expr steps)))

(defn cond->-expand [expr & clauses] (cond-thread 'clojure.core/-> expr clauses))
(defn cond->>-expand [expr & clauses] (cond-thread 'clojure.core/->> expr clauses))
(defn some->-expand [expr & forms] (some-thread 'clojure.core/-> expr forms))
(defn some->>-expand [expr & forms] (some-thread 'clojure.core/->> expr forms))

(defn as->-expand
  [expr name & forms]
  (list 'clojure.core/let
        (into [name expr] (interleave (repeat name) (butlast forms)))
        (if (empty? forms) name (last forms))))

(defn doto-expand
  [x & forms]
  (let [gx (gsym)]
    (concat
     (list 'clojure.core/let [gx x])
     (map (fn [f]
            (with-meta (if (seq? f)
                         (apply list (first f) gx (next f))
                         (list f gx))
              (meta f)))
          forms)
     (list gx))))

(defn if-not-expand
  ([test then] (if-not-expand test then nil))
  ([test then else] (list 'if (list 'clojure.core/not test) then else)))

(defn- some-binding
  [bindings]
  (when-not (and (vector? bindings) (= 2 (count bindings)))
    (throw (ex-info "if-some/when-some require a vector of exactly 2 forms"
                    {:bindings bindings})))
  bindings)

(defn if-some-expand
  ([bindings then] (if-some-expand bindings then nil))
  ([bindings then else]
   (let [[form tst] (some-binding bindings)
         temp       (auto-sym "temp")]
     (list 'clojure.core/let [temp tst]
           (list 'if (list 'clojure.core/nil? temp)
                 else
                 (list 'clojure.core/let [form temp] then))))))

(defn when-some-expand
  [bindings & body]
  (let [[form tst] (some-binding bindings)
        temp       (auto-sym "temp")]
    (list 'clojure.core/let [temp tst]
          (list 'if (list 'clojure.core/nil? temp)
                nil
                (apply list 'clojure.core/let [form temp] body)))))

(defn when-first-expand
  [bindings & body]
  (let [[x xs] bindings
        xs-sym (auto-sym "xs")]
    (list 'clojure.core/when-let [xs-sym (list 'clojure.core/seq xs)]
          (apply list 'clojure.core/let [x (list 'clojure.core/first xs-sym)] body))))

(defn condp-expand
  [pred expr & clauses]
  (let [gpred (names/fresh-symbol "pred__")
        gexpr (names/fresh-symbol "expr__")
        emit  (fn emit [args]
                (let [[[a b c :as clause] more] (split-at (if (= :>> (second args)) 3 2) args)
                      n (count clause)]
                  (cond
                    (= 0 n) (list 'throw
                                  (list 'java.lang.IllegalArgumentException.
                                        (list 'clojure.core/str "No matching clause: " gexpr)))
                    (= 1 n) a
                    (= 2 n) (list 'if (list gpred a gexpr) b (emit more))
                    :else   (let [p (auto-sym "p")]
                              (list 'clojure.core/if-let [p (list gpred a gexpr)]
                                    (list c p)
                                    (emit more))))))]
    (list 'clojure.core/let [gpred pred gexpr expr] (emit clauses))))

(def expanders
  "Macro name -> expander, applied to the macro call's arguments."
  {'cond->     cond->-expand
   'cond->>    cond->>-expand
   'some->     some->-expand
   'some->>    some->>-expand
   'as->       as->-expand
   'doto       doto-expand
   'if-not     if-not-expand
   'if-some    if-some-expand
   'when-some  when-some-expand
   'when-first when-first-expand
   'condp      condp-expand})
