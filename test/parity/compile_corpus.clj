;; Host-parity corpus compiler: a portable script, run unchanged on the JVM,
;; Babashka and ClojureWasm (see `make parity`).
;;
;;   clojure -M test/parity/compile_corpus.clj OUT-DIR
;;   bb test/parity/compile_corpus.clj OUT-DIR
;;   cljw -A:cljw -M test/parity/compile_corpus.clj OUT-DIR
;;
;; Compiles every .cljel under examples/ and test/, plus the self-hosted
;; runtime, with compile-file-string, and writes OUT-DIR/<path>.el (or
;; <path>.error with the message). The three OUT-DIRs must be identical.
;; Paths after OUT-DIR compile only those files; --list prints the corpus.
(require '[clojure-elisp.core :as clel]
         '[clojure.java.io :as io]
         '[clojure.string :as str])

(def out-dir
  (or (first *command-line-args*)
      (throw (ex-info "usage: compile_corpus.clj OUT-DIR" {}))))

(defn cljel-files [dir]
  (->> (file-seq (io/file dir))
       (filter #(.isFile %))
       (map #(.getPath %))
       (filter #(str/ends-with? % ".cljel"))))

(def corpus
  (sort (concat (cljel-files "examples")
                (cljel-files "test")
                ["resources/clojure-elisp/runtime.cljel"])))

(defn write! [path content]
  (let [f (io/file path)]
    (.mkdirs (.getParentFile f))
    (spit f content)))

(when (= "--list" out-dir)
  (run! println corpus)
  (System/exit 0))

(doseq [path (or (seq (rest *command-line-args*)) corpus)]
  (let [target (str out-dir "/" path)
        result (try {:ok (clel/compile-file-string (slurp path))}
                    (catch Exception e {:error (ex-message e)}))]
    (if-let [elisp (:ok result)]
      (do (write! (str target ".el") elisp)
          (println (str "ok     " path " (" (count elisp) " chars)")))
      (do (write! (str target ".error") (:error result))
          (println (str "error  " path ": " (:error result)))))))
