(ns migrate-cond
  "Migrate pre-grouped `cond` forms in .cljel sources to `elisp-cond`.

   Usage:
     clojure -M:dev -m migrate-cond <file-or-dir>...
     clojure -M:dev -m migrate-cond --dry-run <file-or-dir>...

   A `cond` is rewritten ONLY when it cannot be a valid flat Clojure `cond`:
   an odd clause count, or a test position whose head is itself a list. Both
   are impossible for genuine flat pairs, so the rewrite has no false
   positives; ambiguous forms are left alone and must be reviewed by hand.

   Formatting, comments and reader syntax are preserved (rewrite-clj)."
  (:require [rewrite-clj.zip :as z]
            [rewrite-clj.node :as n]
            [clojure-elisp.compile :as compile]
            [clojure.java.io :as io]
            [clojure.string :as str]))

(defn- uncallable-test-head?
  "True when FORM cannot occupy a `cond` test position in Elisp: function
   position admits a symbol or a literal lambda, never a computed value."
  [form]
  (and (seq? form)
       (seq? (first form))
       (not (contains? #{'lambda 'closure} (first (first form))))))

(defn pre-grouped?
  "True when the sexpr of a (cond ...) form cannot be a valid flat cond."
  [form]
  (let [clauses (rest form)]
    (or (odd? (count clauses))
        (boolean (some (fn [[test _]] (uncallable-test-head? test))
                       (partition 2 clauses))))))

(defn- safe-sexpr
  "z/sexpr, but a sentinel on unreadable cljel tokens (e.g. the Elisp symbol
   `1+`, which Clojure's reader rejects as a malformed number)."
  [zloc]
  (try (z/sexpr zloc) (catch Exception _ ::unreadable)))

(defn- cond-head?
  "zloc is the `cond` symbol in head position of a list."
  [zloc]
  (and (= :token (z/tag zloc))
       (= 'cond (safe-sexpr zloc))
       (some-> zloc z/up z/tag (= :list))
       (= (z/node zloc) (some-> zloc z/up z/down z/node))))

(defn migrate-source
  "Rewrite SRC, returning [new-source rewrite-count].

   `1+` / `1-` are masked to their reader-safe aliases before parsing and
   restored afterwards — rewrite-clj's parser rejects them as malformed
   numbers, exactly as the compiler's own reader does. Throws if the
   zero-rewrite round-trip is not byte-identical, so a file whose formatting
   this tool cannot reproduce is never written."
  [src]
  (let [masked (compile/preprocess-elisp-numbers src)]
    (when-not (= masked (z/root-string (z/of-string* masked)))
      (throw (ex-info "round-trip is not byte-identical; refusing to rewrite" {})))
    (let [counter (atom 0)
          root    (-> (z/of-string* masked)
                      (z/prewalk
                       (fn [zloc]
                         (and (cond-head? zloc)
                              (let [form (safe-sexpr (z/up zloc))]
                                (and (seq? form) (pre-grouped? form)))))
                       (fn [zloc]
                         (swap! counter inc)
                         (z/replace zloc (n/token-node 'elisp-cond)))))]
      [(compile/postprocess-elisp-numbers (z/root-string root)) @counter])))

(defn- cljel-files [path]
  (let [f (io/file path)]
    (if (.isDirectory f)
      (->> (file-seq f)
           (filter #(str/ends-with? (.getName %) ".cljel"))
           (sort-by #(.getPath %)))
      [f])))

(defn -main [& args]
  (let [dry?  (contains? (set args) "--dry-run")
        paths (remove #{"--dry-run"} args)
        files (mapcat cljel-files paths)
        total (atom 0)]
    (doseq [f files]
      (let [src (slurp f)]
        (try
          (let [[out n] (migrate-source src)]
            (when (pos? n)
              (swap! total + n)
              (when-not dry? (spit f out))
              (println (format "%3d  %s" n (.getPath f)))))
          (catch Exception e
            (println (format "SKIP %s -- %s" (.getPath f) (ex-message e)))))))
    (println (format "\n%s %d cond form(s) across %d file(s)"
                     (if dry? "Would rewrite" "Rewrote") @total (count files)))))
