(ns clojure-elisp.deterministic-output-test
  "The same source compiles to byte-identical Elisp, however much the JVM
   compiled before it.

   Generated names used to come from the process-wide gensym counter (the
   compiler's own destructuring temporaries, the reader's #() and x# names,
   gensyms inside JVM macro expansions like cond->), so a file compiled twice
   in one process differed, and committed .el files churned on every build."
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [clojure-elisp.core :as clel]
            [clojure-elisp.fs-stub-test :refer [stub-fs]]
            [clojure-elisp.project :as project]))

(def ^:private source
  "(ns my.pkg)

(defn destructured [{:keys [a b] :or {b 2}} [x & more]]
  (list a b x more))

(defn anon [xs] (map #(+ %1 1) (apply list #(list %&) xs)))

(defn threaded [x]
  (list (cond-> x (pos? x) (inc))
        (some-> x (inc) (dec))
        (doto (list x) (identity))))

(defn let-destructure [pairs]
  (let [[[k v] & others] pairs
        {:strs [name]} (first others)]
    (list k v name)))

(defn reified [] (reify P (m [this] 1)))")

(defn- advance-global-gensym-counter!
  "What an ordinary JVM does between two builds."
  []
  (dotimes [_ 1000] (gensym))
  (clel/compile-file-string "(ns other) (defn f [{:keys [q]}] (cond-> q true inc))"))

(deftest compiling-the-same-file-twice-is-byte-identical
  (let [first-out  (clel/compile-file-string source)
        _          (advance-global-gensym-counter!)
        second-out (clel/compile-file-string source)]
    (is (= first-out second-out))
    (testing "no name carries a process-wide gensym number"
      (is (not (re-find #"__\d{3,}" second-out)) second-out))))

(deftest one-form-edit-does-not-renumber-another-form
  (let [emitted-form (fn [out] (re-find #"(?s)\(defun my-pkg-let-destructure.*?\n\n" out))
        before       (clel/compile-file-string source)
        after        (clel/compile-file-string
                      (str/replace source "(defn anon"
                                   "(defn added [{:keys [z]} [w]] (list z w))\n\n(defn anon"))]
    (is (some? (emitted-form before)))
    (is (= (emitted-form before) (emitted-form after)))))

(deftest destructured-params-take-their-as-name
  (testing "an :as name is what a docstring calls the argument; a generated
            p__N is not"
    (let [out (clel/emit '(defn f "Use STATE." [{:keys [a] :as state}] a))]
      (is (str/starts-with? out "(defun f (state)")))))

(deftest compile-project-twice-writes-identical-files
  (let [store (atom {"/src/my/pkg.cljel" source
                     "/src/my/util.cljel" "(ns my.util) (defn g [[a b]] (+ a b))"})
        fs*   (stub-fs store)
        build (fn []
                ;; drop outputs and cache so the second build recompiles
                (swap! store #(into {} (remove (fn [[p _]] (str/starts-with? p "/out/"))) %))
                (project/compile-project fs* ["/src"] "/out")
                (select-keys @store ["/out/my-pkg.el" "/out/my-util.el"]))
        one   (build)
        _     (advance-global-gensym-counter!)
        two   (build)]
    (is (= 2 (count one)))
    (is (= one two))))
