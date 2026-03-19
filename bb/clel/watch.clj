(ns clel.watch
  "Watch directory for .cljel file changes and recompile automatically.
   Uses polling via babashka.fs since Babashka lacks fsnotify.

   The watch loop accepts a `compile-fn` callback so the caller controls
   how compilation is dispatched (jar, clojure CLI, etc.)."
  (:require [babashka.fs :as fs]
            [clojure.string :as str])
  (:import [java.time LocalTime]
           [java.time.format DateTimeFormatter]))

(def ^:private time-fmt (DateTimeFormatter/ofPattern "HH:mm:ss"))

(defn- now-str []
  (.format (LocalTime/now) time-fmt))

(defn find-cljel-files
  "Recursively find all .cljel files under `dir`."
  [dir]
  (->> (fs/glob dir "**/*.cljel")
       (mapv str)))

(defn file-mtimes
  "Return a map of {filepath -> last-modified-time} for the given files."
  [files]
  (into {}
        (keep (fn [f]
                (when (fs/exists? f)
                  [f (fs/last-modified-time f)])))
        files))

(defn- compute-output-path
  "Given the watch root, the output root, and an absolute .cljel path,
   return the corresponding .el output path."
  [abs-dir output-dir cljel-path]
  (let [rel (str (fs/relativize abs-dir cljel-path))]
    (str (fs/path output-dir (str/replace rel #"\.cljel$" ".el")))))

(defn- compile-changed!
  "Compile a single changed file, printing status. Returns nil."
  [abs-dir output-dir compile-fn cljel-path]
  (let [rel (str (fs/relativize abs-dir cljel-path))
        out-file (compute-output-path abs-dir output-dir cljel-path)]
    (println (format "[%s] Compiling %s" (now-str) rel))
    (try
      (compile-fn (str cljel-path) out-file)
      (catch Exception e
        (binding [*out* *err*]
          (println (format "  error: %s" (ex-message e))))))))

(defn- compile-all!
  "Initial compilation of all .cljel files in the directory."
  [abs-dir output-dir compile-fn]
  (println "[initial] Compiling all files...")
  (let [files (find-cljel-files abs-dir)]
    (if (empty? files)
      (println "  No .cljel files found.")
      (doseq [f files]
        (compile-changed! abs-dir output-dir compile-fn f)))))

(defn- detect-changes
  "Compare current mtimes to previous. Returns seq of changed file paths."
  [prev-mtimes current-mtimes]
  (for [[f mtime] current-mtimes
        :when (not= mtime (get prev-mtimes f))]
    f))

(defn watch-and-compile
  "Watch `dir` for .cljel changes, recompiling on change via `compile-fn`.

   `compile-fn` — a function of [input-path output-path] that compiles
                   a single .cljel file to .el.
   `dir`        — source directory to watch.
   `output`     — output directory (defaults to `dir`).

   Options (keyword args after positional):
     :interval  — polling interval in ms (default 500)
     :init?     — whether to do initial full compilation (default true)

   Blocks the calling thread. Interrupt with ctrl-c or Thread/interrupt."
  [dir output compile-fn & {:keys [interval init?]
                             :or   {interval 500
                                    init?    true}}]
  (let [abs-dir    (str (fs/absolutize dir))
        output-dir (str (fs/absolutize (or output dir)))]
    (println (format "Watching %s for .cljel changes (ctrl-c to stop)" abs-dir))

    ;; Initial compilation
    (when init?
      (compile-all! abs-dir output-dir compile-fn))

    ;; Poll loop
    (let [prev-mtimes (atom (file-mtimes (find-cljel-files abs-dir)))]
      (try
        (loop []
          (Thread/sleep interval)
          (let [current-files  (find-cljel-files abs-dir)
                current-mtimes (file-mtimes current-files)
                changed        (detect-changes @prev-mtimes current-mtimes)]
            (when (seq changed)
              (doseq [f changed]
                (compile-changed! abs-dir output-dir compile-fn f))
              (reset! prev-mtimes current-mtimes)))
          (recur))
        (catch InterruptedException _
          (println "\nWatch stopped."))))))
