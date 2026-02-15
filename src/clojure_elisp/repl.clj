(ns clojure-elisp.repl
  "JVM-side REPL server for ClojureElisp.

   Reads Clojure forms from stdin (one per line), compiles each to Elisp
   via core/emit, and prints the result prefixed with CLJEL-OK: or CLJEL-ERR:
   for the Emacs-side comint filter to parse."
  (:require [clojure-elisp.core :as core]
            [clojure.string :as str]))

(defn one-line
  "Collapse multi-line Elisp to a single line for the wire protocol."
  [s]
  (str/replace s #"\s*\n\s*" " "))

(defn compile-input
  "Compile a string input to Elisp. Returns {:ok elisp} or {:err message}."
  [input]
  (try
    (let [form  (read-string input)
          elisp (one-line (core/emit form))]
      {:ok elisp})
    (catch Exception e
      {:err (.getMessage e)})))

(defn -main
  "Entry point for the ClojureElisp REPL process.
   Reads lines from stdin and prints compiled Elisp."
  [& _args]
  (println "CLJEL-READY")
  (flush)
  (loop []
    (when-let [line (read-line)]
      (let [trimmed (str/trim line)]
        (when-not (str/blank? trimmed)
          (let [result (compile-input trimmed)]
            (if (:ok result)
              (println (str "CLJEL-OK:" (:ok result)))
              (println (str "CLJEL-ERR:" (:err result))))
            (flush))))
      (recur))))
