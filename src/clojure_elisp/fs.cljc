(ns clojure-elisp.fs
  "Filesystem Boundary port for the compiler.

   The compile pipeline is pure; all disk/classpath effects go through the
   IFilesystem protocol so orchestration depends on an abstraction (DIP) and
   tests can inject a stub instead of touching the real filesystem.

   RealFs runs on the JVM, Babashka and ClojureWasm. ClojureWasm has no
   classpath resource loader and no file modification times, so the adapter
   finds resources by searching the java.class.path directories itself, and
   reports an unknown mtime as nil (the project build then recompiles, which
   is always correct)."
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(defprotocol IFilesystem
  "Filesystem effects the compiler needs. Inject an implementation; the
   pipeline never calls slurp/spit/io directly."
  (read-file [fs path] "Read the file at path as a string.")
  (write-file! [fs path content] "Write content to path, creating parent dirs.")
  (file-exists? [fs path] "True when path exists on disk.")
  (file-mtime [fs path] "Last-modified time of path in epoch millis (0 if absent), or nil when the host cannot tell.")
  (list-files [fs dir] "Seq of absolute paths of all regular files under dir, recursively.")
  (read-resource [fs path] "Read a classpath resource as a string, or nil if absent.")
  (make-dirs! [fs path] "Ensure the directory at path (and its parents) exists."))

(defn- last-modified
  "Epoch millis of path's last modification (0 if absent), or nil where the
   host has no way to tell: ClojureWasm's java.io.File has no lastModified,
   and no portable spelling exists, hence the one reader conditional here."
  [path]
  #?(:cljw    nil
     :default (.lastModified (io/file path))))

(defn- classpath-dirs
  "Directories on java.class.path. On ClojureWasm this is the source path
   (-cp, $CLJW_PATH or deps.edn), the only place its resources live."
  []
  (when-let [cp (System/getProperty "java.class.path")]
    (->> (str/split cp (re-pattern (str "[" (System/getProperty "path.separator" ":") "]")))
         (remove str/blank?)
         (filter #(.isDirectory (io/file %))))))

(defn find-resource
  "Contents of the classpath resource at path, or nil. io/resource first;
   then a search of dirs (the classpath directories by default), for hosts
   whose io/resource finds nothing (ClojureWasm)."
  ([path] (find-resource path (classpath-dirs)))
  ([path dirs]
   (if-let [r (io/resource path)]
     (slurp r)
     (some (fn [dir]
             (let [f (io/file dir path)]
               (when (.isFile f)
                 (slurp f))))
           dirs))))

(defrecord RealFs []
  IFilesystem
  (read-file [_ path]
    (slurp path))
  (write-file! [_ path content]
    (let [f (io/file path)]
      (when-let [parent (.getParentFile f)]
        (.mkdirs parent))
      (spit f content)))
  (file-exists? [_ path]
    (.exists (io/file path)))
  (file-mtime [_ path]
    (last-modified path))
  (list-files [_ dir]
    (->> (io/file dir)
         file-seq
         (filter #(.isFile ^java.io.File %))
         (map #(.getAbsolutePath ^java.io.File %))))
  (read-resource [_ path]
    (find-resource path))
  (make-dirs! [_ path]
    (.mkdirs (io/file path))))

(def default-fs
  "The production filesystem adapter."
  (->RealFs))

(def Fs
  "Malli schema for the filesystem port — anything satisfying IFilesystem
   (the RealFs adapter or an injected test stub). Reused as the leading-arg
   schema in project/config fn contracts."
  [:fn {:error/message "must satisfy clojure-elisp.fs/IFilesystem"}
   #(satisfies? IFilesystem %)])
