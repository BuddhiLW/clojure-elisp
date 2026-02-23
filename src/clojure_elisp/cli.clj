(ns clojure-elisp.cli
  "Command-line interface for the ClojureElisp compiler.
   Entry point for the standalone uberjar distribution."
  (:require [clojure-elisp.core :as clel]
            [clojure.string :as str])
  (:gen-class))

(def version
  (str/trim (or (some-> (clojure.java.io/resource "clojure-elisp/VERSION")
                        slurp)
                "dev")))

(defn- print-usage []
  (println "Usage: clel <command> [options]")
  (println)
  (println "Commands:")
  (println "  compile <file.cljel> [-o output.el]    Compile a single file")
  (println "  compile <dir/> [-o outdir/]             Compile all .cljel files in directory")
  (println "  version                                 Print version")
  (println)
  (println "Examples:")
  (println "  clel compile src/my_app.cljel -o out/my-app.el")
  (println "  clel compile src/ -o out/"))

(defn- parse-output-flag
  "Parse -o flag from remaining args. Returns [output-path remaining-args]."
  [args]
  (loop [remaining args
         result []]
    (if (empty? remaining)
      [nil result]
      (let [[flag & rest-args] remaining]
        (if (= "-o" flag)
          (if (seq rest-args)
            [(first rest-args) (into result (next rest-args))]
            (do (binding [*out* *err*]
                  (println "Error: -o flag requires an argument"))
                (System/exit 1)))
          (recur rest-args (conj result flag)))))))

(defn- compile-file-cmd [input output]
  (let [output (or output
                   (str (subs input 0 (- (count input)
                                         (count (re-find #"\.[^.]*$" input))))
                        ".el"))]
    (try
      (let [result (clel/compile-file input output)]
        (println (str "Compiled " (:input result)
                      " -> " (:output result)
                      " (" (:size result) " chars)")))
      (catch Exception e
        (binding [*out* *err*]
          (println (str "Error compiling " input ": " (.getMessage e))))
        (System/exit 1)))))

(defn- compile-dir-cmd [input output]
  (let [output (or output input)]
    (try
      (let [results (clel/compile-project [input] output)]
        (doseq [r results :when r]
          (println (str "Compiled " (:input r)
                        " -> " (:output r)
                        " (" (:size r) " chars)"))))
      (catch Exception e
        (binding [*out* *err*]
          (println (str "Error compiling directory " input ": " (.getMessage e))))
        (System/exit 1)))))

(defn -main [& args]
  (when (empty? args)
    (print-usage)
    (System/exit 1))

  (let [[cmd & rest-args] args]
    (case cmd
      "compile"
      (if (empty? rest-args)
        (do (binding [*out* *err*]
              (println "Error: compile requires an input file or directory"))
            (print-usage)
            (System/exit 1))
        (let [input (first rest-args)
              [output _] (parse-output-flag (next rest-args))
              dir? (.isDirectory (java.io.File. input))]
          (if dir?
            (compile-dir-cmd input output)
            (compile-file-cmd input output))))

      "version"
      (println (str "clojure-elisp " version))

      ;; default
      (do (binding [*out* *err*]
            (println (str "Unknown command: " cmd)))
          (print-usage)
          (System/exit 1)))))
