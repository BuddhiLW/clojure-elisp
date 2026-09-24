(ns clojure-elisp.fs-stub-test
  "Proves the compile pipeline runs through an injected IFilesystem stub with
   zero real disk access — the DIP payoff of the Boundary port."
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure-elisp.fs :as fs]
            [clojure-elisp.project :as project]
            [hive-dsl.result :as r]))

(defn stub-fs
  "An in-memory IFilesystem backed by an atom {path -> content}. Every file
   reports mtime (0 unless given; nil models a host that cannot tell)."
  ([store] (stub-fs store 0))
  ([store mtime]
   (reify fs/IFilesystem
     (read-file [_ path]
       (if-let [c (get @store path)]
         c
         (throw (java.io.FileNotFoundException. (str path " (stub: no such file)")))))
     (write-file! [_ path content] (swap! store assoc path content) nil)
     (file-exists? [_ path] (contains? @store path))
     (file-mtime [_ _] mtime)
     (list-files [_ dir] (filter #(str/starts-with? % dir) (keys @store)))
     (read-resource [_ _] nil)
     (make-dirs! [_ _] nil))))

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

(deftest unknown-mtime-always-recompiles
  (let [source {"/virt/src/a.cljel" "(ns a)\n(defn f [] 1)"}]
    (testing "a known, unchanged mtime reuses the cached output"
      (let [fs* (stub-fs (atom source) 0)]
        (project/compile-project fs* ["/virt/src"] "/virt/out")
        (is (every? :cached (project/compile-project fs* ["/virt/src"] "/virt/out")))))
    (testing "an mtime the host cannot read (ClojureWasm) never counts as unchanged"
      (let [fs* (stub-fs (atom source) nil)]
        (project/compile-project fs* ["/virt/src"] "/virt/out")
        (is (not-any? :cached (project/compile-project fs* ["/virt/src"] "/virt/out")))))))

(deftest resources-fall-back-to-classpath-directories
  (let [dir (doto (io/file (System/getProperty "java.io.tmpdir")
                           (str "clel-fs-" (System/nanoTime) "/clel-test"))
              (.mkdirs))]
    (spit (io/file dir "r.txt") "found")
    (let [root (str (.getParentFile dir))]
      (is (= "found" (fs/find-resource "clel-test/r.txt" [root]))
          "a resource io/resource cannot see is found in a classpath directory")
      (is (nil? (fs/find-resource "clel-test/missing.txt" [root]))))
    (is (some? (fs/find-resource "clojure-elisp/VERSION" []))
        "io/resource is still consulted first")))

(deftest compile-file-result-error-through-stub-fs
  (testing "compile-file-result error path via stub fs for a missing file"
    (let [store (atom {})
          fs*   (stub-fs store)
          res   (project/compile-file-result fs* "/virt/missing.cljel" "/virt/o.el")]
      (is (r/err? res))
      (is (= :compile/file-error (:error res))))))
