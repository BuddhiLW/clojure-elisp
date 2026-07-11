(ns clojure-elisp.fs
  "Filesystem Boundary port for the compiler.

   The compile pipeline is pure; all disk/classpath effects go through the
   IFilesystem protocol so orchestration depends on an abstraction (DIP) and
   tests can inject a stub instead of touching the real filesystem."
  (:require [clojure.java.io :as io]))

(defprotocol IFilesystem
  "Filesystem effects the compiler needs. Inject an implementation; the
   pipeline never calls slurp/spit/io directly."
  (read-file [fs path] "Read the file at path as a string.")
  (write-file! [fs path content] "Write content to path, creating parent dirs.")
  (file-exists? [fs path] "True when path exists on disk.")
  (file-mtime [fs path] "Last-modified time of path in epoch millis (0 if absent).")
  (list-files [fs dir] "Seq of absolute paths of all regular files under dir, recursively.")
  (read-resource [fs path] "Read a classpath resource as a string, or nil if absent.")
  (make-dirs! [fs path] "Ensure the directory at path (and its parents) exists."))

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
    (.lastModified (io/file path)))
  (list-files [_ dir]
    (->> (io/file dir)
         file-seq
         (filter #(.isFile ^java.io.File %))
         (map #(.getAbsolutePath ^java.io.File %))))
  (read-resource [_ path]
    (when-let [r (io/resource path)]
      (slurp r)))
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
