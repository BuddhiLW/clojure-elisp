(ns clojure-elisp.reader-test
  "The portable reader (clojure-elisp.reader) against clojure.lang.LispReader,
   the reader it follows, plus the guarantees the compiler builds on: source
   locations, deterministic generated names and source-ordered collections.

   LispReader is the oracle here because this suite runs on the JVM. Host
   parity (JVM, Babashka, ClojureWasm) is checked by `make parity`."
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.string :as str]
            [clojure.walk :as walk]
            [clojure-elisp.compile :as cc]
            [clojure-elisp.core :as clel]
            [clojure-elisp.names :as names]
            [clojure-elisp.reader :as reader]))

;; ============================================================================
;; Oracle helpers
;; ============================================================================

(defn- lisp-read-all
  "Every form in s as clojure.core/read over a LineNumberingPushbackReader
   reads it: what compile.clj did before the portable reader."
  [s]
  (let [rdr (clojure.lang.LineNumberingPushbackReader. (java.io.StringReader. s))]
    (loop [forms []]
      (let [form (read rdr false ::eof)]
        (if (= ::eof form)
          forms
          (recur (conj forms form)))))))

(defn- generated? [x]
  (and (symbol? x)
       (re-matches #"(p\d+|rest)__\d+#|.+__\d+__auto__" (name x))))

(defn- canonical
  "form with generated names (p1__N#, rest__N#, foo__N__auto__) renamed G0,
   G1, ... by first appearance, so two readers compare regardless of how each
   numbers them. Patterns, which compare by identity, become their source."
  [form]
  (let [seen (atom {})]
    (walk/prewalk (fn [x]
                    (cond
                      (generated? x)
                      (or (get @seen x)
                          (let [g (symbol (str "G" (count @seen)))]
                            (swap! seen assoc x g)
                            g))

                      (instance? java.util.regex.Pattern x)
                      (list 'regex (str x))

                      :else x))
                  form)))

(defn- list-locations
  "{:line :column} of every list in form, depth first."
  [form]
  (->> (tree-seq coll? seq form)
       (filter seq?)
       (keep #(not-empty (select-keys (meta %) [:line :column])))
       vec))

(defn- agrees-with-lisp-reader
  "Assert both readers produce the same forms and the same list locations."
  [label source]
  (let [ours   (reader/read-forms source)
        theirs (lisp-read-all source)]
    (is (= (canonical theirs) (canonical ours)) (str label ": forms"))
    (is (= (list-locations theirs) (list-locations ours)) (str label ": locations"))))

;; ============================================================================
;; Agreement with LispReader
;; ============================================================================

(def ^:private snippets
  {"collections"   "(a b) [c d] {:e 1 :f [2]} #{1 2} () [] {}"
   "reader macros" "'x @y #'z ~w ~@v (quote q)"
   "metadata"      "^:private (f) ^{:doc \"d\" :k 1} [v] ^String s ^:a ^:b (g)"
   "characters"    "[\\a \\newline \\space \\tab \\u0041 \\o101 \\( \\\\ \\é]"
   "numbers"       "[1 -2 +3 1.5 -0.5 1e10 6.02E23 3/4 -7/8 0x1F 017 2r101 36rZZ 1N 1.5M 0]"
   "infinities"    "[##Inf ##-Inf]"
   "strings"       "[\"a\" \"tab\\there\" \"q\\\"q\" \"u\\u00e9\" \"back\\\\slash\" \"multi\nline\"]"
   "keywords"      "[:a :ns/b :c.d/e ::local]"
   "symbols"       "[a ns/b a.b/c foo# a' %x .method Ctor. clojure.core/+ / nil true false]"
   "namespaced"    "#:ns{:a 1 :b/c 2 :_/d 3} #::{:x 1}"
   "comments"      "; line comment\n(x) #_ (ignored form) (y) #! shebang-style\n(z)"
   "fn literals"   "#(+ % %2 %&) #(identity %) #(list %2 %1) #()"
   "syntax-quote"  "`(foo ~x ~@ys bar/baz str/join if do fn* & 1 :k \"s\")"
   "sq colls"      "`{:a ~x :b [1 ~@y]} `#{a} `[] `()"
   "sq gensyms"    "`(let [a# 1 b# a#] (+ a# b#)) `(x# `(y# x#))"
   "sq meta"       "`^:m (f) `^{:tag String} x"
   "regex"         "#\"a.b\""
   "tagged"        "#inst \"2020-01-01T00:00:00.000-00:00\" #uuid \"00000000-0000-0000-0000-000000000000\""
   "nesting"       "(a\n  (b\n    [c (d {:e (f)})]))\n\n   (g)"})

(deftest agrees-with-lisp-reader-on-snippets
  (doseq [[label source] snippets]
    (testing label
      (agrees-with-lisp-reader label source))))

(deftest agrees-with-lisp-reader-on-the-corpus
  (doseq [path ["examples/demo.cljel"
                "examples/buffer-demo.cljel"
                "examples/hive-mcp-eca.cljel"
                "examples/hive-mcp-log.cljel"
                "examples/olympus-ui.cljel"
                "resources/clojure-elisp/runtime.cljel"
                "test/parity/kitchen_sink.cljel"]]
    (testing path
      (agrees-with-lisp-reader path (cc/preprocess-elisp-syntax (slurp path))))))

;; ============================================================================
;; Source locations: source-location tracking depends on these
;; ============================================================================

(deftest lists-carry-line-and-column
  (let [[ns-form defn-form] (reader/read-forms "(ns demo)\n\n  (defn f [x]\n    (inc x))")]
    (is (= {:line 1 :column 1} (meta ns-form)))
    (is (= {:line 3 :column 3} (meta defn-form)))
    (is (= {:line 4 :column 5} (meta (nth defn-form 3))))
    (is (nil? (meta (nth defn-form 2))) "vectors carry none, as with LispReader")))

(deftest columns-count-tabs-and-crlf-as-lispreader-does
  (let [[a b] (reader/read-forms "\t(a)\r\n  (b)")]
    (is (= {:line 1 :column 2} (meta a)))
    (is (= {:line 2 :column 3} (meta b)))))

(deftest meta-prefix-overrides-the-list-position
  (let [[form] (reader/read-forms "  ^:private\n (f x)")]
    (is (= {:line 1 :column 3 :private true} (meta form)))))

(deftest compiled-forms-keep-their-locations
  (testing "read-all-forms (the compiler's entry) keeps metadata"
    (is (= [{:line 1 :column 1} {:line 3 :column 1}]
           (map meta (cc/read-all-forms "(def a 1)\n\n(def b 2)")))))
  (testing "source comments use them"
    (is (str/includes? (binding [clojure-elisp.emitter/*emit-source-comments* true]
                         (clel/compile-file-string "(ns loc)\n\n(defn f [] 1)"))
                       "L3:C1"))))

(deftest read-first-reads-one-form
  (is (= '(ns a) (reader/read-first "(ns a) (this is not read")))
  (is (nil? (reader/read-first "  ; nothing here\n"))))

;; ============================================================================
;; Errors
;; ============================================================================

(defn- read-error [source]
  (try (reader/read-forms source) nil
       (catch clojure.lang.ExceptionInfo e
         (assoc (ex-data e) :message (ex-message e)))))

(deftest reader-errors-carry-a-location
  (is (= {:message "EOF while reading, starting at line 2" :line 3}
         (select-keys (read-error "(a)\n(b\n c") [:message :line])))
  (is (= "Unmatched delimiter: )" (:message (read-error "(a))"))))
  (is (= "Map literal must contain an even number of forms" (:message (read-error "{:a}"))))
  (is (= "Duplicate key: :a" (:message (read-error "{:a 1 :a 2}"))))
  (is (= "Duplicate key: 1" (:message (read-error "#{1 1}"))))
  (is (= "Conditional read not allowed" (:message (read-error "#?(:clj 1)"))))
  (is (= "Nested #()s are not allowed" (:message (read-error "#(#(%))"))))
  (is (= "EOF while reading string" (:message (read-error "\"abc")))))

(deftest invalid-numbers-are-typed
  (let [data (read-error "(a)\n(2+ c)")]
    (is (= ::reader/invalid-number (:type data)))
    (is (= "Invalid number: 2+" (:message data)))
    (is (= 2 (:line data))))
  (testing "the compiler turns it into its Elisp-number hint"
    (is (thrown-with-msg? clojure.lang.ExceptionInfo
                          #"Unhandled Elisp number symbol: Invalid number: 2\+ \(line 2\)"
                          (cc/read-all-forms "(a)\n(2+ c)")))))

;; ============================================================================
;; Determinism: output is a pure function of the source
;; ============================================================================

(deftest generated-names-are-numbered-per-compilation
  (let [source "(defn f [[a b] {:keys [c]}] (map #(+ % a) [b c]))"
        first  (clel/compile-file-string source)
        second (clel/compile-file-string source)]
    (is (= first second) "the same source compiles to the same bytes")
    (is (re-find #"\(lambda \(p1__1\)" first) "numbering starts at 1 per compilation")))

(deftest fresh-names-nest-and-fall-back
  (testing "a nested compilation shares the enclosing counter"
    (is (= ['a1 'b2]
           (names/with-fresh-names
             [(names/fresh-symbol "a")
              (names/with-fresh-names (names/fresh-symbol "b"))]))))
  (testing "outside a compilation, gensym's global counter is used"
    (is (not= (names/fresh-symbol "x") (names/fresh-symbol "x")))))

(deftest collections-keep-source-order
  (testing "a map past eight entries emits in written order, not hash order"
    (is (str/includes?
         (clel/compile-string "{:k9 9 :k1 1 :k8 8 :k2 2 :k7 7 :k3 3 :k6 6 :k4 4 :k5 5 :k10 10}")
         "(cons :k9 9) (cons :k1 1) (cons :k8 8) (cons :k2 2) (cons :k7 7)")))
  (testing "a set emits in written order"
    (is (= "(list :c :a :b)" (clel/compile-string "#{:c :a :b}"))))
  (testing "quoted sets and maps print in written order too"
    (is (= "'#{zeta alpha mid beta omega}" (clel/compile-string "'#{zeta alpha mid beta omega}")))
    (is (= "'{:k9 9, :k1 1, :k8 8, :k2 2, :k7 7, :k3 3, :k6 6, :k4 4, :k5 5}"
           (clel/compile-string "'{:k9 9 :k1 1 :k8 8 :k2 2 :k7 7 :k3 3 :k6 6 :k4 4 :k5 5}"))))
  (testing "strings print as the JVM prints them, \\f and \\b escaped"
    (is (= "\"a\\fb\\bc\"" (clel/compile-string "\"a\\fb\\bc\""))))
  (testing "the order survives syntax-quote"
    (is (= '(clojure.core/apply clojure.core/hash-set
                                (clojure.core/seq (clojure.core/concat (clojure.core/list :z)
                                                                       (clojure.core/list :a))))
           (first (reader/read-forms "`#{:z :a}"))))))
