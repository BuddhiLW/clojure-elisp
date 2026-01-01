(ns clojure-elisp.core
  "ClojureElisp - A Clojure dialect that compiles to Emacs Lisp.

   Similar to ClojureScript for JavaScript, ClojureElisp brings
   Clojure's syntax and semantics to Emacs Lisp.

   Usage:
     (require '[clojure-elisp.core :as clel])

     ;; Compile a form to elisp string
     (clel/emit '(defn greet [name] (str \"Hello, \" name)))

     ;; Compile a file
     (clel/compile-file \"src/my_package.cljel\" \"out/my-package.el\")

     ;; Compile a namespace
     (clel/compile-ns 'my.package)"
  (:require [clojure-elisp.analyzer :as ana]
            [clojure-elisp.emitter :as emit]
            [clojure.java.io :as io]))

(defn emit
  "Compile a Clojure form to an Elisp string."
  [form]
  (-> form
      ana/analyze
      emit/emit))

(defn emit-forms
  "Compile multiple forms to Elisp, joined by newlines."
  [forms]
  (->> forms
       (map emit)
       (clojure.string/join "\n\n")))

(defn compile-string
  "Compile a string of Clojure code to Elisp."
  [s]
  (let [forms (read-string (str "[" s "]"))]
    (emit-forms forms)))

(defn compile-file
  "Compile a .cljel file to a .el file."
  [input-path output-path]
  (let [source (slurp input-path)
        elisp (compile-string source)]
    (spit output-path elisp)
    {:input input-path
     :output output-path
     :size (count elisp)}))

(defn compile-ns
  "Compile a namespace to Elisp.
   Looks for the source file in the classpath."
  [ns-sym]
  (let [path (-> (str ns-sym)
                 (clojure.string/replace "." "/")
                 (clojure.string/replace "-" "_")
                 (str ".cljel"))
        resource (io/resource path)]
    (when resource
      (compile-string (slurp resource)))))

(comment
  ;; Quick test
  (emit '(defn foo [x] (+ x 1)))
  (emit '(let [a 1 b 2] (+ a b)))
  (emit '(if (> x 0) "positive" "non-positive")))
