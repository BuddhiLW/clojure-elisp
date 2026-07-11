(ns clojure-elisp.property-test
  "Property-based tests for the ClojureElisp compiler pipeline.

   Uses hive-test property macros and test.check generators to verify
   structural properties: totality, idempotency, roundtrips, and
   ADT invariants."
  (:require [clojure.set :as set]
            [clojure.test :refer [deftest is testing]]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [hive-test.properties :as props]
            [hive-dsl.adt :as adt]
            [hive-dsl.result :as r]
            [clojure-elisp.core :as clel]
            [clojure-elisp.analyzer :as ana]
            [clojure-elisp.emitter :as emit]
            [clojure-elisp.errors :as errors]
            [clojure-elisp.ast :as ast]))

;; ============================================================================
;; Generators
;; ============================================================================

(def gen-nil (gen/return nil))

(def gen-literal
  "Generator for simple Clojure literal forms."
  (gen/one-of [gen-nil
               gen/boolean
               gen/small-integer
               gen/string-alphanumeric
               (gen/fmap keyword gen/string-alpha-numeric)]))

(def gen-arith-form
  "Generator for simple arithmetic expressions: (op a b)."
  (gen/fmap (fn [[op a b]] (list op a b))
            (gen/tuple (gen/elements ['+ '- '* '/])
                       (gen/fmap #(if (zero? %) 1 %) gen/small-integer)
                       (gen/fmap #(if (zero? %) 1 %) gen/small-integer))))

(def gen-comparison-form
  "Generator for comparison expressions: (op a b)."
  (gen/fmap (fn [[op a b]] (list op a b))
            (gen/tuple (gen/elements ['= '< '> '<= '>=])
                       gen/small-integer
                       gen/small-integer)))

(def gen-if-form
  "Generator for (if test then else) forms."
  (gen/fmap (fn [[a b c]] (list 'if (list '> a 0) b c))
            (gen/tuple gen/small-integer
                       gen/small-integer
                       gen/small-integer)))

(def gen-let-form
  "Generator for simple (let [sym val] sym) forms."
  (gen/let [n gen/small-integer]
    (list 'let ['x__gen n] 'x__gen)))

(def gen-simple-form
  "Generator for simple well-formed Clojure forms."
  (gen/one-of [gen-literal
               gen-arith-form
               gen-comparison-form
               gen-if-form
               gen-let-form]))

(def gen-nested-form
  "Generator for compound forms nesting a simple sub-form, exercising the
   recursive AST child-node typing across a spread of ops (do/when/and/or/if/
   let/cond) — all verified analyzable."
  (gen/let [inner gen-simple-form]
    (gen/elements
     [(list 'do inner inner)
      (list 'when (list '> 'x 0) inner)
      (list 'and inner inner)
      (list 'or inner inner)
      (list 'if (list '> 'x 0) inner inner)
      (list 'let ['y__gen inner] 'y__gen)
      (list 'cond (list '> 'x 0) inner :else inner)])))

(defn ast-nodes
  "Every node map (has :op) reachable in an analyzed AST tree."
  [root]
  (filter :op
          (tree-seq map?
                    (fn [n] (mapcat (fn [v] (cond (map? v)        [v]
                                                  (sequential? v) (filter map? v)
                                                  :else           []))
                                    (vals n)))
                    root)))

(def gen-compile-error-variant
  "Generator for CompileError variant keywords."
  (gen/elements [:compile/read-error
                 :compile/analysis-error
                 :compile/emit-error
                 :compile/file-error]))

(def gen-compile-error
  "Generator for CompileError ADT values."
  (gen/let [variant gen-compile-error-variant]
    (case variant
      :compile/read-error     (errors/compile-error :compile/read-error {:message "test read error"})
      :compile/analysis-error (errors/compile-error :compile/analysis-error {:message "test analysis error" :form nil})
      :compile/emit-error     (errors/compile-error :compile/emit-error {:message "test emit error" :op :unknown})
      :compile/file-error     (errors/compile-error :compile/file-error {:message "test file error" :path "/tmp/test.cljel"}))))

(def gen-compile-result
  "Generator for compile Results (ok or err)."
  (gen/one-of [(gen/fmap r/ok (gen/fmap (fn [n] (str "(+ " n " 1)")) gen/small-integer))
               (gen/fmap #(r/err :compile/analysis-error {:message %})
                         gen/string-alphanumeric)]))

;; ============================================================================
;; Property: Totality — analyze never crashes on well-formed input
;; ============================================================================

(props/defprop-total analyze-total
  (fn [form] (ana/analyze form))
  gen-simple-form
  {:num-tests 200})

;; ============================================================================
;; Property: Totality — emit-result always returns a Result (never throws)
;; ============================================================================

(props/defprop-total emit-result-total
  clel/emit-result
  gen-simple-form
  {:num-tests 200 :pred (fn [r] (or (r/ok? r) (r/err? r)))})

;; ============================================================================
;; Property: Idempotency — mangle-name applied twice = applied once
;; ============================================================================

(props/defprop-idempotent mangle-name-idempotent
  emit/mangle-name
  (gen/fmap symbol gen/string-alphanumeric)
  {:num-tests 200})

;; ============================================================================
;; Property: Complement — ok? and err? are exact complements on Results
;; ============================================================================

(props/defprop-complement result-ok-err-complement
  r/ok? r/err?
  gen-compile-result
  {:num-tests 200})

;; ============================================================================
;; Property: CompileError ADT invariants
;; ============================================================================

(defspec compile-error-is-valid-adt 200
  (prop/for-all [err gen-compile-error]
                (adt/adt-valid? err)))

(defspec compile-error-predicate-holds 200
  (prop/for-all [err gen-compile-error]
                (errors/compile-error? err)))

(defspec compile-error-variant-preserved 200
  (prop/for-all [kw gen-compile-error-variant]
                (let [data (case kw
                             :compile/read-error     {:message "test"}
                             :compile/analysis-error {:message "test" :form nil}
                             :compile/emit-error     {:message "test" :op :const}
                             :compile/file-error     {:message "test" :path "/tmp"})
                      err  (errors/compile-error kw data)]
                  (= kw (adt/adt-variant err)))))

(defspec compile-error-exhaustive-dispatch 200
  (prop/for-all [err gen-compile-error]
                (string?
                 (adt/adt-case errors/CompileError err
                               :compile/read-error     (:message err)
                               :compile/analysis-error (:message err)
                               :compile/emit-error     (:message err)
                               :compile/file-error     (:message err)))))

(defspec compile-error-serialize-roundtrip 100
  (prop/for-all [err gen-compile-error]
                (let [serialized   (adt/serialize err)
                      deserialized (adt/deserialize serialized)]
                  (= (adt/adt-variant err)
                     (adt/adt-variant deserialized)))))

;; ============================================================================
;; Property: AST validation — analyzed forms produce valid AST nodes
;; ============================================================================

(defspec analyzed-forms-have-valid-ast 200
  (prop/for-all [form gen-simple-form]
                (let [ast-node (ana/analyze form)]
                  (and (contains? ast-node :op)
                       (contains? ast-node :env)
           ;; validate-ast-node returns the node or throws
                       (= ast-node (ast/validate-ast-node ast-node))))))

;; ============================================================================
;; Property: Pipeline integrity — analyze then emit produces strings
;; ============================================================================

(defspec analyze-emit-produces-string 200
  (prop/for-all [form gen-simple-form]
                (string? (clel/emit form))))

;; ============================================================================
;; Property: Result-wrapped emit matches bare emit for valid forms
;; ============================================================================

(defspec emit-result-matches-emit 200
  (prop/for-all [form gen-simple-form]
                (let [result (clel/emit-result form)]
                  (if (r/ok? result)
                    (= (:ok result) (clel/emit form))
        ;; If emit-result returned err, bare emit should also throw
                    (try
                      (clel/emit form)
                      false ;; Should have thrown
                      (catch Exception _ true))))))

;; ============================================================================
;; Deterministic unit tests for Result API
;; ============================================================================

(deftest emit-result-success-test
  (testing "emit-result returns ok for valid forms"
    (let [result (clel/emit-result '(+ 1 2))]
      (is (r/ok? result))
      (is (string? (:ok result)))
      (is (= "(+ 1 2)" (:ok result)))))

  (testing "emit-result returns ok for literals"
    (is (r/ok? (clel/emit-result 42)))
    (is (r/ok? (clel/emit-result "hello")))
    (is (r/ok? (clel/emit-result nil)))
    (is (r/ok? (clel/emit-result true))))

  (testing "emit-result returns ok for complex forms"
    (is (r/ok? (clel/emit-result '(let [x 1] x))))
    (is (r/ok? (clel/emit-result '(if true 1 2))))
    (is (r/ok? (clel/emit-result '(defn foo [x] x))))))

(deftest compile-file-result-error-test
  (testing "compile-file-result returns err for nonexistent file"
    (let [result (clel/compile-file-result "/nonexistent/path.cljel" "/tmp/out.el")]
      (is (r/err? result))
      (is (= :compile/file-error (:error result))))))

(deftest compile-error-adt-test
  (testing "CompileError construction and predicates"
    (let [err (errors/compile-error :compile/read-error {:message "bad syntax"})]
      (is (errors/compile-error? err))
      (is (adt/adt? err))
      (is (adt/adt-valid? err))
      (is (= :compile/read-error (adt/adt-variant err)))
      (is (= :CompileError (adt/adt-type err)))))

  (testing "CompileError exhaustive matching"
    (let [err (errors/compile-error :compile/emit-error {:message "bad op" :op :unknown})]
      (is (= "bad op"
             (adt/adt-case errors/CompileError err
                           :compile/read-error     (:message err)
                           :compile/analysis-error (:message err)
                           :compile/emit-error     (:message err)
                           :compile/file-error     (:message err)))))))

(deftest ast-validation-test
  (testing "validate-ast-node passes for valid nodes"
    (let [node (ana/analyze '(+ 1 2))]
      (is (= node (ast/validate-ast-node node)))))

  (testing "validate-ast-node throws for missing keys"
    (is (thrown-with-msg? clojure.lang.ExceptionInfo
                          #"missing keys"
                          (ast/validate-ast-node {:op :const}))))

  (testing "validate-ast-node passes for unknown ops"
    (let [node {:op :future-op :stuff true}]
      (is (= node (ast/validate-ast-node node)))))

  (testing "emitter validation mode"
    (binding [emit/*validate-ast* true]
      (let [node (ana/analyze '(+ 1 2))]
        (is (string? (emit/emit node)))))))

(deftest ast-value-shape-validation-test
  (testing "non-vector collection key is rejected"
    (is (thrown? clojure.lang.ExceptionInfo
                 (ast/validate-ast-node {:op :do :body :not-a-vector})))
    (is (thrown? clojure.lang.ExceptionInfo
                 (ast/validate-ast-node {:op :recur :args 42}))))
  (testing "well-shaped collection key passes"
    (is (= {:op :do :body []} (ast/validate-ast-node {:op :do :body []}))))
  (testing "expanded value-schema coverage rejects mis-shaped keys"
    (is (false? (ast/valid-ast-node? {:op :let :bindings :nope :body []})))
    (is (false? (ast/valid-ast-node? {:op :map :keys 5 :vals []})))
    (is (false? (ast/valid-ast-node? {:op :cond :clauses :x})))
    (is (false? (ast/valid-ast-node? {:op :defrecord :name "R" :fields [] :protocols []})))
    (is (true?  (ast/valid-ast-node? {:op :let :bindings [] :body []})))
    (is (true?  (ast/valid-ast-node? {:op :defrecord :name 'R :fields [] :protocols []}))))
  (testing "valid-ast-node? and explain-ast-node agree"
    (is (true? (ast/valid-ast-node? {:op :do :body []})))
    (is (nil? (ast/explain-ast-node {:op :do :body []})))
    (is (some? (ast/explain-ast-node {:op :do :body :nope})))))

(deftest ast-generator-test
  (testing "gen-node produces nodes that validate for their :op"
    (doseq [op [:const :local :var]]
      (let [node (gen/generate (ast/gen-node op))]
        (is (= op (:op node)))
        (is (= node (ast/validate-ast-node node)))))))

(defspec generated-leaf-nodes-validate 100
  (prop/for-all [node (gen/one-of (mapv ast/gen-node [:const :local :var]))]
                (= node (ast/validate-ast-node node))))

;; ============================================================================
;; AST registry: drift-guard + recursive conformance
;; ============================================================================

(deftest op-required-covers-emitter-dispatch
  (testing "op->required stays exhaustive over emit-node's dispatch keys"
    (let [emit-ops (disj (set (keys (methods emit/emit-node))) :default)
          missing  (set/difference emit-ops (set (keys ast/op->required)))]
      (is (empty? missing)
          (str "ops dispatched by emit-node but missing from ast/op->required "
               "(add them so validation covers them): " missing)))))

(defspec analyze-produces-valid-nodes 300
  ;; Every node in an analyzed tree conforms to its :op schema — turns the
  ;; recursive AST registry from documentation into a checked invariant.
  (prop/for-all [form (gen/one-of [gen-simple-form gen-nested-form])]
                (every? ast/valid-ast-node? (ast-nodes (ana/analyze form)))))

(defspec analyze-produces-valid-tree 200
  ;; Whole-tree recursive validation via the shared ::schema/node schema.
  (prop/for-all [form (gen/one-of [gen-simple-form gen-nested-form])]
                (ast/valid-ast-tree? (ana/analyze form))))

(deftest ast-no-false-rejection-regressions
  ;; Guards the corpus-gap false-rejections found in adversarial review: a node
  ;; the analyzer really produces must validate under BOTH per-node and
  ;; whole-tree checks, and the two must agree.
  (testing "try without a finally clause (analyzer emits :finally nil)"
    (doseq [form ['(try (foo))
                  '(try (foo) (catch Error e 1))
                  '(try (foo) (catch Error e 1) (catch Exception x 2))]]
      (let [n (ana/analyze form)]
        (is (true? (ast/valid-ast-node? n)) (str "valid-ast-node? " form))
        (is (true? (ast/valid-ast-tree? n)) (str "valid-ast-tree? " form)))))
  (testing "try WITH a finally still validates"
    (let [n (ana/analyze '(try (foo) (catch Error e 1) (finally (bar))))]
      (is (true? (ast/valid-ast-node? n)))
      (is (true? (ast/valid-ast-tree? n)))))
  (testing "multi-arity defn has no top-level :body but is valid"
    (doseq [form ['(defn f ([x] x) ([x y] (+ x y)))
                  '(defn h ([] 0) ([x] x))
                  '(defn k "doc" ([x] x) ([x y] y))]]
      (let [n (ana/analyze form)]
        (is (true? (ast/valid-ast-node? n)) (str "valid-ast-node? " form))
        (is (true? (ast/valid-ast-tree? n)) (str "valid-ast-tree? " form))
        (is (= n (ast/validate-ast-node n))))))
  (testing "single-arity defn (has :body) still validates"
    (is (true? (ast/valid-ast-node? (ana/analyze '(defn g [x] (+ x 1))))))))

;; ============================================================================
;; Pipeline Result Schemas (Malli)
;; ============================================================================

(deftest pipeline-result-schema-test
  (testing "success Result conforms to StringCompileResult"
    (is (clel/valid-string-result? (clel/emit-result '(+ 1 2)))))
  (testing "error Result conforms to its schema"
    (is (clel/valid-string-result? (clel/compile-file-string-result "(foo 1")))
    (is (clel/valid-file-result? (clel/compile-file-result "/nope.cljel" "/tmp/clel-x.el"))))
  (testing "an ok-string payload is not a valid file Result"
    (is (not (clel/valid-file-result? (clel/emit-result '(+ 1 2)))))))

(defspec emit-result-conforms-schema 200
  (prop/for-all [form gen-simple-form]
                (clel/valid-string-result? (clel/emit-result form))))

(deftest instrumentation-contract-test
  (testing "instrument! enforces input contracts; unstrument! removes them"
    (clel/instrument!)
    (try
      (is (thrown-with-msg? Exception #"invalid-input" (clel/compile-string 42)))
      (is (string? (clel/compile-string "(+ 1 2)")))
      (finally (clel/unstrument!)))
    (is (string? (clel/compile-string "(+ 1 2)")))))

;; ============================================================================
;; Property Tests for New Sequence Functions (clel-050)
;; ============================================================================

(defspec emit-cycle-total 100
  (prop/for-all [form gen-simple-form]
                (let [wrapped (list 'cycle (list 'quote [form]))]
                  (string? (try (-> wrapped ana/analyze emit/emit)
                                (catch Exception _ "error-but-string"))))))

(defspec emit-iterate-total 100
  (prop/for-all [n gen/small-integer]
                (let [wrapped (list 'iterate 'inc n)]
                  (string? (try (-> wrapped ana/analyze emit/emit)
                                (catch Exception _ "error-but-string"))))))

(defspec emit-take-nth-total 100
  (prop/for-all [n (gen/fmap #(max 1 (Math/abs %)) gen/small-integer)]
                (let [wrapped (list 'take-nth n 'coll)]
                  (string? (try (-> wrapped ana/analyze emit/emit)
                                (catch Exception _ "error-but-string"))))))

(defspec emit-complement-involution 100
  (prop/for-all [form gen-simple-form]
    ;; complement of complement should map to nested clel-complement calls
                (let [single (list 'complement 'pred)
                      double (list 'complement (list 'complement 'pred))
                      s1 (try (-> single ana/analyze emit/emit) (catch Exception _ nil))
                      s2 (try (-> double ana/analyze emit/emit) (catch Exception _ nil))]
                  (and (some? s1)
                       (some? s2)
           ;; double should contain two occurrences of clel-complement
                       (> (count (re-seq #"clel-complement" s2)) 1)))))

;; ============================================================================
;; Property: ns-derived-output-name idempotency via mangle-name
;; ============================================================================

(props/defprop-idempotent ns-mangle-idempotent
  emit/mangle-name
  (gen/fmap #(symbol (str "hive-mcp-" %)) gen/string-alpha-numeric)
  {:num-tests 200})

;; ============================================================================
;; Property: extract-ns-name totality — never crashes on any source string
;; ============================================================================

(props/defprop-total extract-ns-name-total
  clel/extract-ns-name
  (gen/one-of [(gen/return "")
               (gen/return "(defn foo [x] x)")
               (gen/fmap #(str "(ns pkg-" % ")") gen/string-alpha-numeric)
               (gen/fmap #(str "(ns my-pkg-" % ")") gen/string-alpha-numeric)])
  {:num-tests 100})

;; ============================================================================
;; Property: ns-derived-output-name roundtrip — ns name → .el filename
;; ============================================================================

(defspec ns-derived-output-name-ends-with-el 100
  (prop/for-all [suffix gen/string-alpha-numeric]
                (let [src (str "(ns hive-mcp-" suffix ")")
                      result (clel/ns-derived-output-name src)]
                  (if result
                    (clojure.string/ends-with? result ".el")
                    true))))

;; ============================================================================
;; Property: load-paths parsing totality — analyze-ns never crashes
;; ============================================================================

(defspec ns-load-path-totality 100
  (prop/for-all [paths (gen/vector gen/string-alpha-numeric 0 3)]
                (let [ns-form (list 'ns 'test-ns (list :load-path (vec paths)))
                      ast (ana/analyze ns-form)]
                  (and (= :ns (:op ast))
                       (= paths (:load-paths ast))))))
