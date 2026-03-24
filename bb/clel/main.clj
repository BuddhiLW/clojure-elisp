(ns clel.main
  (:require [babashka.process :as p]
            [babashka.fs :as fs]
            [clojure.string :as str]))

;; ---------------------------------------------------------------------------
;; Helpers
;; ---------------------------------------------------------------------------

(defn- die
  "Print message to stderr and exit with code."
  [code & msgs]
  (binding [*out* *err*]
    (println (str/join " " msgs)))
  (System/exit code))

(defn- find-jar
  "Locate the clel uberjar.
   1. $CLEL_JAR env var
   2. ~/.local/lib/clel.jar
   Returns path string or nil."
  []
  (let [env-jar (System/getenv "CLEL_JAR")]
    (cond
      (and env-jar (fs/exists? env-jar))
      (str env-jar)

      (fs/exists? (fs/expand-home "~/.local/lib/clel.jar"))
      (str (fs/expand-home "~/.local/lib/clel.jar"))

      :else nil)))

(defn- find-project-root
  "Walk up from cwd looking for deps.edn. Also checks $CLEL_HOME.
   Returns absolute path string or nil."
  []
  (let [clel-home (System/getenv "CLEL_HOME")]
    (if (and clel-home (fs/exists? (fs/path clel-home "deps.edn")))
      (str (fs/absolutize clel-home))
      (loop [dir (fs/absolutize (fs/cwd))]
        (cond
          (nil? dir) nil

          (fs/exists? (fs/path dir "deps.edn"))
          (str dir)

          :else
          (recur (fs/parent dir)))))))

(defn- find-config-file
  "Walk up from cwd looking for clel.edn. Returns absolute path string or nil."
  []
  (loop [dir (fs/absolutize (fs/cwd))]
    (cond
      (nil? dir) nil

      (fs/exists? (fs/path dir "clel.edn"))
      (str (fs/absolutize (fs/path dir "clel.edn")))

      :else
      (recur (fs/parent dir)))))

(defn- read-version
  "Read version from VERSION file in the project root."
  []
  (let [root (find-project-root)]
    (if (and root (fs/exists? (fs/path root "VERSION")))
      (str/trim (slurp (str (fs/path root "VERSION"))))
      "dev")))

;; ---------------------------------------------------------------------------
;; Execution
;; ---------------------------------------------------------------------------

(defn- shell!
  "Run a subprocess via p/shell. On non-zero exit, exit the bb process
   with the same code (the subprocess already printed its error)."
  [opts & cmd-parts]
  (let [cmd (into [] (map str) (flatten cmd-parts))
        result (p/process cmd (merge {:inherit true} opts))]
    (let [exit (:exit @result)]
      (when-not (zero? exit)
        (System/exit exit)))))

(defn- run-jar
  "Run the uberjar with the given args. Inherits stdio."
  [jar & args]
  (shell! {} "java" "-jar" (str jar) args))

(defn- escape-clj-string
  "Escape a string for embedding in a Clojure expression."
  [s]
  (-> s
      (str/replace "\\" "\\\\")
      (str/replace "\"" "\\\"")))

(defn- run-clojure
  "Run clojure -M -e EXPR from the project root. Inherits stdio."
  [expr]
  (let [root (find-project-root)]
    (when-not root
      (die 1 "Cannot find project root (no deps.edn found); set CLEL_HOME env var"))
    (shell! {:dir root} "clojure" "-M" "-e" expr)))

;; ---------------------------------------------------------------------------
;; Compile subcommands
;; ---------------------------------------------------------------------------

(defn- compile-file
  "Compile a single .cljel file to .el."
  [input output]
  (let [abs-input  (str (fs/absolutize input))
        abs-output (str (fs/absolutize
                         (or output
                             (str (fs/strip-ext input) ".el"))))]
    ;; Ensure output directory exists
    (fs/create-dirs (fs/parent abs-output))
    (if-let [jar (find-jar)]
      (run-jar jar "compile" abs-input "-o" abs-output)
      (run-clojure
       (format "(require '[clojure-elisp.core :as clel]) (let [r (clel/compile-file \"%s\" \"%s\")] (println (str \"Compiled \" (:input r) \" -> \" (:output r) \" (\" (:size r) \" chars)\")))"
               (escape-clj-string abs-input)
               (escape-clj-string abs-output))))))

(defn- compile-dir
  "Compile all .cljel files in a directory."
  [input output]
  (let [abs-input  (str (fs/absolutize input))
        abs-output (str (fs/absolutize (or output input)))]
    (if-let [jar (find-jar)]
      (run-jar jar "compile" abs-input "-o" abs-output)
      ;; Fallback: compile file-by-file
      (let [files (fs/glob abs-input "**.cljel")]
        (when (empty? files)
          (die 1 (str "No .cljel files found in " input)))
        (doseq [f files]
          (let [rel      (str (fs/relativize abs-input f))
                out-file (str (fs/path abs-output
                                       (str (fs/strip-ext rel) ".el")))]
            (compile-file (str f) out-file)))))))

(defn- compile-from-config
  "Find clel.edn and compile the project from its config."
  []
  (let [config-path (find-config-file)]
    (when-not config-path
      (die 1 "No clel.edn found in current directory or any parent"))
    (println (str "Using project config: " config-path))
    (if-let [jar (find-jar)]
      (run-jar jar "compile-project" config-path)
      (run-clojure
       (format "(require '[clojure-elisp.core :as clel]) (let [results (clel/compile-project-from-config \"%s\")] (doseq [r results :when r] (println (str \"Compiled \" (:input r) \" -> \" (:output r) \" (\" (:size r) \" chars)\"))))"
               (escape-clj-string config-path))))))

;; ---------------------------------------------------------------------------
;; Argument parsing
;; ---------------------------------------------------------------------------

(defn- parse-output-flag
  "Parse -o flag from args. Returns the output path or nil."
  [args]
  (loop [remaining args]
    (when (seq remaining)
      (if (= "-o" (first remaining))
        (if (second remaining)
          (second remaining)
          (die 1 "Error: -o flag requires an argument"))
        (recur (rest remaining))))))

(defn- print-usage []
  (println "clel — ClojureElisp compiler CLI")
  (println)
  (println "Usage: clel <command> [options]")
  (println)
  (println "Commands:")
  (println "  compile                          Compile from clel.edn project config")
  (println "  compile <file.cljel> [-o out.el] Compile a single file")
  (println "  compile <dir/> [-o outdir/]      Compile all .cljel files in directory")
  (println "  watch <dir/> [-o outdir/]        Watch and recompile on changes")
  (println "  mcp                              Start MCP stdio server")
  (println "  version                          Print version")
  (println)
  (println "Examples:")
  (println "  clel compile")
  (println "  clel compile src/my_app.cljel -o out/my-app.el")
  (println "  clel compile src/ -o out/")
  (println "  clel watch src/ -o out/"))

;; ---------------------------------------------------------------------------
;; Main
;; ---------------------------------------------------------------------------

(defn -main [& args]
  (if (empty? args)
    (do (print-usage)
        (System/exit 0))
    (let [[cmd & rest-args] args]
      (case cmd
        ("compile" "c")
        (if (empty? rest-args)
          ;; Zero-arg compile: use clel.edn
          (compile-from-config)
          (let [input  (first rest-args)
                output (parse-output-flag (rest rest-args))]
            (if (fs/directory? input)
              (compile-dir input output)
              (compile-file input output))))

        ("watch" "w")
        (if (empty? rest-args)
          (die 1 "Error: watch requires a directory argument")
          (let [dir    (first rest-args)
                output (parse-output-flag (rest rest-args))]
            (require 'clel.watch)
            ((resolve 'clel.watch/watch-and-compile)
             dir output compile-file)))

        "mcp"
        (if-let [jar (find-jar)]
          (run-jar jar "mcp")
          (die 1 "MCP server requires the uberjar. Run: make build install"))

        ("version" "v")
        (println (str "clel " (read-version)))

        ;; default
        (do (binding [*out* *err*]
              (println (str "Unknown command: " cmd)))
            (print-usage)
            (System/exit 1))))))
