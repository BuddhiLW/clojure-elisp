(ns scan-dropped-forms
  "Find .cljel forms whose trailing subforms the compiler silently discards.

   Usage:
     clojure -M:dev -m scan-dropped-forms <file-or-dir>...

   Elisp gives several special forms an implicit-progn tail that their Clojure
   namesakes do not have: `if` takes ELSE..., `while`/`when-let` take bodies.
   The analyzer applies Clojure arity, so the tail is dropped with no warning.
   A hit is reported only when the extra forms cannot be valid Clojure, so
   there are no false positives."
  (:require [rewrite-clj.zip :as z]
            [clojure-elisp.compile :as compile]
            [clojure.java.io :as io]
            [clojure.string :as str]))

(def max-arity
  "Largest form count Clojure's version of each head accepts, head included.
   Empty since `if` learned Elisp's implicit-progn else; kept as the seam for
   the next head found to truncate rather than fold."
  {})

(def pair-forms
  "Heads taking var/value pairs, where an odd argument count leaves a trailing
   form that `partition 2` would discard."
  #{'setq 'setf})

(defn- safe-sexpr
  "z/sexpr, or a sentinel on tokens Clojure's reader rejects (e.g. `1+`)."
  [zloc]
  (try (z/sexpr zloc) (catch Exception _ ::unreadable)))

(defn over-arity
  "Nil, or [head actual limit] when FORM would lose trailing subforms: either
   it exceeds a fixed Clojure arity, or it is a pair form given an odd count."
  [form]
  (when (and (seq? form) (symbol? (first form)))
    (let [head (first form)
          n    (count form)]
      (cond
        (and (pair-forms head) (even? n))
        [head n (dec n)]

        (when-let [limit (max-arity head)] (> n limit))
        [head n (max-arity head)]))))

(defn- head-zloc?
  "zloc is a symbol in head position of a list."
  [zloc]
  (and (= :token (z/tag zloc))
       (symbol? (safe-sexpr zloc))
       (some-> zloc z/up z/tag (= :list))
       (= (z/node zloc) (some-> zloc z/up z/down z/node))))

(defn scan-source
  "Seq of {:head :actual :limit :line} for each over-arity form in SRC."
  [src]
  (let [masked (compile/preprocess-elisp-numbers src)
        hits   (atom [])]
    (z/prewalk
     (z/of-string* masked)
     (fn [zloc]
       (when (head-zloc? zloc)
         (let [form (safe-sexpr (z/up zloc))]
           (when-let [[head n limit] (and (seq? form) (over-arity form))]
             (swap! hits conj {:head head :actual n :limit limit
                               :line (some-> zloc z/node meta :row)}))))
       false)
     identity)
    @hits))

(defn- cljel-files [path]
  (let [f (io/file path)]
    (if (.isDirectory f)
      (->> (file-seq f)
           (filter #(str/ends-with? (.getName %) ".cljel"))
           (sort-by #(.getPath %)))
      [f])))

(defn -main [& paths]
  (let [files (mapcat cljel-files paths)
        total (atom 0)]
    (doseq [f files]
      (try
        (doseq [{:keys [head actual limit line]} (scan-source (slurp f))]
          (swap! total inc)
          (println (format "%s:%s  (%s ...) has %d forms, Clojure takes %d — %d dropped"
                           (.getPath f) (or line "?") head actual limit
                           (- actual limit))))
        (catch Exception e
          (println (format "SKIP %s -- %s" (.getPath f) (ex-message e))))))
    (println (format "\n%d silently-dropped form(s) across %d file(s)"
                     @total (count files)))))
