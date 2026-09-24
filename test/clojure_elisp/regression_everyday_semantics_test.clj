(ns clojure-elisp.regression-everyday-semantics-test
  "Everyday Clojure that used to compile to Elisp meaning something else.
   String-level cover; test/elisp/clojure-elisp-semantics-test.el runs the
   same constructs in Emacs."
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.string :as str]
            [clojure-elisp.analyzer :as ana]
            [clojure-elisp.core :as clel]
            [clojure-elisp.emitter :as emit]))

(defn- emit-form [form]
  (-> form ana/analyze emit/emit))

(defn- compile-ns
  "Compile SRC as a file, returning the code after the ;;; Code: marker."
  [src]
  (second (str/split (clel/compile-file-string src) #";;; Code:\n" 2)))

;;; Function values in a Lisp-2

(deftest local-fn-value-is-funcalled
  (testing "a parameter holding a function"
    (is (= "(defun g (f x)\n  (funcall f x))" (emit-form '(defn g [f x] (f x))))))
  (testing "a let-bound function, and the core fn bound to it"
    (is (= "(let* ((f #'1+))\n    (funcall f 1))" (emit-form '(let [f inc] (f 1))))))
  (testing "a computed callee"
    (is (= "(funcall (clel-comp #'1+ #'1-) 1)" (emit-form '((comp inc dec) 1)))))
  (testing "a #'-quoted callee"
    (is (= "(funcall #'car x)" (emit-form '(#'car x)))))
  (testing "a literal lambda stays in function position"
    (is (str/starts-with? (emit-form '((fn [x] x) 1)) "((lambda (x)"))))

(deftest letfn-names-live-in-the-function-namespace
  (let [out (emit-form '(letfn [(twice [x] (* 2 x))] (map twice xs) (twice 3)))]
    (is (str/includes? out "(clel-map #'twice xs)") "value reference is #'")
    (is (str/includes? out "(twice 3)") "call is direct"))
  (testing "a parameter shadows the letfn name"
    (is (str/includes? (emit-form '(letfn [(g [x] x) (f [g] (g 5))] (f 1)))
                       "(funcall g 5)"))))

(deftest def-value-vs-defn-at-call-sites
  (let [out (compile-ns "(ns a.b)
(def handler (fn [x] x))
(defn helper [x] x)
(defn run [xs] [(handler 1) (map handler xs) (helper 2) (map helper xs) (let [h helper] h)])")]
    (is (str/includes? out "(funcall a-b-handler 1)") "a def is a variable: funcall")
    (is (str/includes? out "(clel-map a-b-handler xs)") "and is passed as its value")
    (is (str/includes? out "(a-b-helper 2)") "a defn is called by name")
    (is (str/includes? out "(clel-map #'a-b-helper xs)"))
    (is (str/includes? out "(h #'a-b-helper)") "a defn read as a value is #'")))

(deftest known-function-in-value-position-is-quoted
  (is (= "(defvar my-inc #'1+)" (emit-form '(def my-inc inc))))
  (is (= "(foo #'clel-str)" (emit-form '(foo str))))
  (testing "Emacs passthrough names may be variables and stay bare"
    (is (= "(message buffer-file-name)" (emit-form '(message buffer-file-name))))))

;;; Keywords and maps in function position

(deftest keyword-in-function-position-is-get
  (is (= "(clel-get m :k)" (emit-form '(:k m))))
  (is (= "(clel-get m :k 0)" (emit-form '(:k m 0))))
  (is (= "(clel-get (clel-get m :a) :b)" (emit-form '(:b (:a m)))))
  (testing "a keyword called with no map is an analysis error, not (:k)"
    (is (thrown-with-msg? clojure.lang.ExceptionInfo #"Keyword :k called with 0"
                          (emit-form '(:k))))))

(deftest map-literal-in-function-position-is-get
  (is (str/starts-with? (emit-form '({:a 1} :a)) "(clel-get ")))

;;; Function arguments of higher-order fns

(deftest arity-dependent-function-slots
  (testing "(sort cmp coll) quotes the comparator; (sort coll) does not"
    (is (= "(clel-sort #'my-cmp xs)" (emit-form '(sort my-cmp xs))))
    (is (= "(clel-sort xs)" (emit-form '(sort xs)))))
  (testing "(sort-by keyfn cmp coll) quotes both"
    (is (= "(clel-sort-by #'k #'my-cmp xs)" (emit-form '(sort-by k my-cmp xs))))
    (is (= "(clel-sort-by :k #'> xs)" (emit-form '(sort-by :k > xs))))))

(deftest swap-threads-the-inner-fns-slots
  (is (= "(clel-swap! a #'clel-update :k #'my-f)" (emit-form '(swap! a update :k my-f))))
  (is (= "(clel-swap! a #'clel-update-in (list :k) #'my-f 1)"
         (emit-form '(swap! a update-in [:k] my-f 1))))
  (is (= "(clel-swap! a #'clel-assoc :k my-v)" (emit-form '(swap! a assoc :k my-v)))
      "assoc has no function slot, so its value stays a value"))

(deftest elisp-higher-order-fns-quote-function-names
  (is (= "(mapcar #'my-f xs)" (emit-form '(mapcar my-f xs))))
  (is (= "(add-hook 'after-save-hook #'my-f)" (emit-form '(add-hook 'after-save-hook my-f))))
  (is (= "(run-at-time 1 nil #'my-f)" (emit-form '(run-at-time 1 nil my-f)))))

(deftest compare-is-mapped
  (is (= "(clel-compare a b)" (emit-form '(compare a b))))
  (is (= "(clel-sort #'clel-compare xs)" (emit-form '(sort compare xs)))))

;;; Destructuring in iteration bindings

(deftest doseq-destructures-instead-of-binding-nil
  (let [out (emit-form '(doseq [[k v] m] (f k v)))]
    (is (re-find #"\(dolist \(elem__\d+ \(clel-seq m\)\)" out)
        "iterates m, not (clel-seq nil)")
    (is (re-find #"\(k \(clel-nth vec__\d+ 0 nil\)\)" out))
    (is (re-find #"\(v \(clel-nth vec__\d+ 1 nil\)\)" out))))

(deftest for-destructures-bindings-and-let
  (let [out (emit-form '(for [[a b] xs :let [[c] b]] c))]
    (is (re-find #"\(clel-seq xs\)" out))
    (is (re-find #"\(a \(clel-nth vec__\d+ 0 nil\)\)" out))
    (is (re-find #"\(c \(clel-nth vec__\d+ 0 nil\)\)" out))))

(deftest loop-destructures-every-iteration
  (let [out (emit-form '(loop [[x & more] xs] (when x (recur more))))]
    (is (re-find #"\(recur \(loop__\d+\)" out) "loops over a fresh symbol")
    (is (re-find #"\(more \(clel-nthnext vec__\d+ 1\)\)" out))
    (is (re-find #"\(recur xs\)\)$" out) "the init is the whole collection")))

(deftest unknown-iteration-binding-is-an-error
  (is (thrown-with-msg? clojure.lang.ExceptionInfo #"Unsupported binding form"
                        (emit-form '(doseq [1 xs] x)))))

;;; Destructuring shorter collections

(deftest vector-destructuring-defaults-to-nil
  (let [out (emit-form '(let [[a b] xs] b))]
    (is (re-find #"\(b \(clel-nth vec__\d+ 1 nil\)\)" out)
        "(nth coll i nil), as Clojure expands it")))

(deftest nested-map-destructuring-expands
  (let [out (emit-form '(let [{[x y] :pt} m] y))]
    (is (re-find #"\(vec__\d+ \(clel-get map__\d+ :pt\)\)" out))
    (is (re-find #"\(y \(clel-nth vec__\d+ 1 nil\)\)" out))))

;;; Definitions: defonce, private names, assignment

(def ^:private defs-src
  "(ns a.b (:require [c.d :as d]))
(def counter 0)
(def ^:private secret 42)
(defonce cache \"The cache.\" (atom {}))
(defonce ^{:doc \"Once.\"} once 1)
(def nothing nil)
(def declared)
(defn ^:private h [x] (+ x secret))
(defn- h2 [x] x)
(defn g []
  (setq counter (inc counter))
  (set! counter 5)
  (setq load-path nil)
  (setq d/v 1)
  (let [x 1] (setq x 2) x)
  [(h 1) (a.b/h 2) (a.b/h2 3) (map h [1])])")

(deftest defonce-is-a-documented-defvar
  (let [out (compile-ns defs-src)]
    (is (re-find #"\(defvar a-b-cache \(clel-atom .*\) \"The cache.\"\)" out))
    (is (str/includes? out "(defvar a-b-once 1 \"Once.\")"))
    (is (not (str/includes? out "hasRoot")))))

(deftest def-nil-binds-and-bare-def-declares
  (let [out (compile-ns defs-src)]
    (is (str/includes? out "(defvar a-b-nothing nil)"))
    (is (str/includes? out "(defvar a-b-declared)"))))

(deftest private-metadata-names-definition-and-call-sites
  (let [out (compile-ns defs-src)]
    (is (str/includes? out "(defvar a-b--secret 42)") "def ^:private")
    (is (str/includes? out "(defun a-b--h (x)") "defn ^:private")
    (is (str/includes? out "(+ x a-b--secret)"))
    (is (str/includes? out "(a-b--h 1)") "short call")
    (is (str/includes? out "(a-b--h 2)") "call qualified with its own namespace")
    (is (str/includes? out "(a-b--h2 3)") "defn- called qualified")
    (is (str/includes? out "(clel-map #'a-b--h (list 1))") "passed as a value")))

(deftest setq-and-set!-resolve-through-the-namespace
  (let [out (compile-ns defs-src)]
    (is (str/includes? out "(setq a-b-counter (1+ a-b-counter))"))
    (is (str/includes? out "(setf a-b-counter 5)"))
    (is (str/includes? out "(setq load-path nil)") "a global Elisp variable as written")
    (is (str/includes? out "(setq c-d-v 1)") "an aliased var")
    (is (str/includes? out "(setq x 2)") "a local")))
