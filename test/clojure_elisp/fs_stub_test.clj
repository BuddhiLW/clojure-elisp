(ns clojure-elisp.fs-stub-test
  "Proves the compile pipeline runs through an injected IFilesystem stub with
   zero real disk access — the DIP payoff of the Boundary port."
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.string :as str]
            [clojure-elisp.fs :as fs]
            [clojure-elisp.project :as project]
            [hive-dsl.result :as r]))

(defn stub-fs
  "An in-memory IFilesystem backed by an atom {path -> content}."
  [store]
  (reify fs/IFilesystem
    (read-file [_ path]
      (if-let [c (get @store path)]
        c
        (throw (java.io.FileNotFoundException. (str path " (stub: no such file)")))))
    (write-file! [_ path content] (swap! store assoc path content) nil)
    (file-exists? [_ path] (contains? @store path))
    (file-mtime [_ _] 0)
    (list-files [_ dir] (filter #(str/starts-with? % dir) (keys @store)))
    (read-resource [_ _] nil)
    (make-dirs! [_ _] nil)))

(deftest compile-file-through-stub-fs
  (testing "compile-file reads and writes via the injected fs, no disk access"
    (let [store  (atom {"/virt/in.cljel" "(defn f [x] (+ x 1))"})
          fs*    (stub-fs store)
          result (project/compile-file fs* "/virt/in.cljel" "/virt/out.el")]
      (is (= "/virt/in.cljel" (:input result)))
      (is (= "/virt/out.el" (:output result)))
      (is (pos? (:size result)))
      (is (contains? @store "/virt/out.el"))
      (is (str/includes? (get @store "/virt/out.el") "defun f")))))

(deftest compile-file-result-error-through-stub-fs
  (testing "compile-file-result error path via stub fs for a missing file"
    (let [store (atom {})
          fs*   (stub-fs store)
          res   (project/compile-file-result fs* "/virt/missing.cljel" "/virt/o.el")]
      (is (r/err? res))
      (is (= :compile/file-error (:error res))))))
