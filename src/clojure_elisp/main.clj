(ns clojure-elisp.main
  "Portable command-line entry point: one CLI for the JVM, Babashka and
   ClojureWasm.

     compile <file.cljel> [-o out.el]   compile one file
     compile <dir/> [-o outdir/]        compile every .cljel under dir
     compile                            compile the project in ./clel.edn
     version                            print the compiler version

   Without -o, a file compiles next to its source, named after its ns
   (my.app -> my-app.el), and a directory compiles in place.

   Run it with any host:
     cljw -M:cljw compile src/app.cljel -o out/app.el
     bb -m clojure-elisp.main compile src/app.cljel -o out/app.el
     clojure -M -m clojure-elisp.main compile src/app.cljel -o out/app.el
   or build a native binary with `cljw build -A:cljw -m clojure-elisp.main`.

   `run` does the work and returns an exit code; `-main` only exits with it."
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure-elisp.core :as clel]
            [clojure-elisp.fs :as fs]))

(def ^:private usage
  (str/join
   "\n"
   ["Usage: clel <command> [options]"
    ""
    "Commands:"
    "  compile                          Compile the project in ./clel.edn"
    "  compile <file.cljel> [-o out.el] Compile a single file"
    "  compile <dir/> [-o outdir/]      Compile all .cljel files in a directory"
    "  version                          Print the version"
    ""
    "Examples:"
    "  clel compile src/my_app.cljel -o out/my-app.el"
    "  clel compile src/ -o out/"]))

(defn- err
  "Print msgs to stderr."
  [& msgs]
  (binding [*out* *err*]
    (println (str/join " " msgs))))

(defn- report
  "Print one compile result the way every clel front end does."
  [{:keys [input output size cached]}]
  (if cached
    (println (str "Up to date " input " -> " output))
    (println (str "Compiled " input " -> " output " (" size " chars)"))))

(defn version
  "The compiler version from the clojure-elisp/VERSION resource, or \"dev\"."
  []
  (str/trim (or (fs/read-resource fs/default-fs "clojure-elisp/VERSION") "dev")))

(defn- strip-extension [path]
  (str/replace path #"\.[^./]*$" ""))

(defn default-output
  "Output path for input when no -o is given: the ns-derived name
   (my.app -> my-app.el) next to the source, else the source's own name."
  [input]
  (let [dir     (or (.getParent (io/file input)) ".")
        derived (clel/ns-derived-output-name (slurp input))]
    (if derived
      (str dir "/" derived)
      (str (strip-extension input) ".el"))))

(defn- parse-args
  "Split compile args into {:input :output} or {:error msg}."
  [args]
  (loop [[a & more] args, acc {}]
    (cond
      (nil? a)   acc
      (= "-o" a) (if (seq more)
                   (recur (rest more) (assoc acc :output (first more)))
                   {:error "-o requires an argument"})
      (:input acc) {:error (str "unexpected argument: " a)}
      :else      (recur more (assoc acc :input a)))))

(defn- compile-cmd
  [args]
  (let [{:keys [input output error]} (parse-args args)]
    (cond
      error
      (do (err "Error:" error) 2)

      (nil? input)
      (if (.exists (io/file "clel.edn"))
        (do (run! #(some-> % report) (clel/compile-project-from-config "clel.edn"))
            0)
        (do (err "Error: no clel.edn in the current directory") 1))

      (.isDirectory (io/file input))
      (do (run! #(some-> % report) (clel/compile-project [input] (or output input)))
          0)

      (.isFile (io/file input))
      (do (report (clel/compile-file input (or output (default-output input))))
          0)

      :else
      (do (err "Error: no such file or directory:" input) 1))))

(defn run
  "Run the CLI on args and return the process exit code."
  [args]
  (let [[cmd & more] args]
    (try
      (case cmd
        ("compile" "c") (compile-cmd more)
        ("version" "v") (do (println (str "clojure-elisp " (version))) 0)
        ("help" "-h" "--help") (do (println usage) 0)
        nil (do (println usage) 1)
        (do (err "Unknown command:" cmd) (err usage) 1))
      (catch Exception e
        (err (str "Error: " (ex-message e)))
        1))))

(defn -main
  [& args]
  (let [code (run args)]
    (when-not (zero? code)
      (System/exit code))))
