(ns clojure-elisp.emitter
  "Emitter for ClojureElisp.

   Transforms AST nodes into Elisp source code strings."
  (:require [clojure.string :as str]))

;; ============================================================================
;; Elisp Name Mangling
;; ============================================================================

(defn mangle-name
  "Convert a Clojure symbol to valid Elisp identifier.
   - Converts dots to dashes for namespace separation
   - Handles special characters"
  [sym]
  (-> (str sym)
      (str/replace "." "-")
      (str/replace "/" "-")
      (str/replace "?" "-p")
      (str/replace "!" "-bang")
      (str/replace ">" "-gt")
      (str/replace "<" "-lt")
      (str/replace "=" "-eq")
      (str/replace "*" "-star")
      (str/replace "+" "-plus")))

(defn ns->prefix
  "Convert namespace to Elisp prefix.
   my.package -> my-package-"
  [ns-sym]
  (str (mangle-name ns-sym) "-"))

;; ============================================================================
;; Core Functions Mapping
;; ============================================================================

(def core-fn-mapping
  "Map Clojure core functions to Elisp equivalents."
  {;; Arithmetic
   '+ "+"
   '- "-"
   '* "*"
   '/ "/"
   'mod "mod"
   'rem "%"
   'inc "1+"
   'dec "1-"

   ;; Comparison
   '= "equal"
   '== "="
   'not= "/="
   '< "<"
   '> ">"
   '<= "<="
   '>= ">="

   ;; Logic
   'not "not"
   'and "and"
   'or "or"

   ;; Type predicates
   'nil? "null"
   'string? "stringp"
   'number? "numberp"
   'symbol? "symbolp"
   'list? "listp"
   'vector? "vectorp"
   'map? "hash-table-p"
   'fn? "functionp"
   'keyword? "keywordp"

   ;; Collections
   'first "car"
   'rest "cdr"
   'next "cdr"
   'cons "cons"
   'conj "clel-conj"
   'count "length"
   'nth "nth"
   'get "clel-get"
   'assoc "clel-assoc"
   'dissoc "clel-dissoc"
   'keys "clel-keys"
   'vals "clel-vals"
   'seq "clel-seq"
   'empty? "null"
   'into "clel-into"
   'reduce "cl-reduce"
   'map "mapcar"
   'filter "cl-remove-if-not"
   'remove "cl-remove-if"
   'take "cl-subseq"
   'drop "nthcdr"
   'reverse "reverse"
   'sort "sort"
   'concat "append"
   'flatten "flatten-tree"

   ;; Strings
   'str "clel-str"
   'subs "substring"
   'format "format"
   'pr-str "prin1-to-string"
   'println "message"
   'print "princ"

   ;; Functions
   'apply "apply"
   'identity "identity"
   'constantly "clel-constantly"
   'partial "apply-partially"
   'comp "clel-comp"

   ;; Emacs-specific
   'message "message"
   'buffer-string "buffer-string"
   'point "point"
   'goto-char "goto-char"
   'insert "insert"
   'delete-char "delete-char"
   'buffer-name "buffer-name"
   'current-buffer "current-buffer"})

;; ============================================================================
;; Emission Helpers
;; ============================================================================

(defn emit-list
  "Emit a list of forms, space-separated."
  [items]
  (str/join " " items))

(defn emit-sexp
  "Wrap items in parentheses as an S-expression."
  [& items]
  (str "(" (emit-list items) ")"))

(defn indent
  "Indent a string by n spaces."
  [n s]
  (let [prefix (apply str (repeat n " "))]
    (->> (str/split-lines s)
         (map #(str prefix %))
         (str/join "\n"))))

;; ============================================================================
;; AST Emitters
;; ============================================================================

(declare emit)

(defmulti emit-node
  "Emit an AST node to Elisp string."
  :op)

(defmethod emit-node :const
  [{:keys [val type]}]
  (case type
    :nil "nil"
    :bool (if val "t" "nil")
    :number (str val)
    :string (pr-str val)
    :keyword (str ":" (name val))
    (str val)))

(defmethod emit-node :local
  [{:keys [name]}]
  (mangle-name name))

(defmethod emit-node :var
  [{:keys [name]}]
  (if-let [elisp-name (get core-fn-mapping name)]
    elisp-name
    (mangle-name name)))

(defmethod emit-node :vector
  [{:keys [items]}]
  (str "(list " (emit-list (map emit items)) ")"))

(defmethod emit-node :map
  [{:keys [keys vals]}]
  (let [pairs (map (fn [k v] (str "(" (emit k) " . " (emit v) ")"))
                   keys vals)]
    (str "'(" (str/join " " pairs) ")")))

(defmethod emit-node :set
  [{:keys [items]}]
  (str "(list " (emit-list (map emit items)) ")"))

(defmethod emit-node :quote
  [{:keys [form]}]
  (str "'" (pr-str form)))

(defmethod emit-node :def
  [{:keys [name docstring init]}]
  (let [elisp-name (mangle-name name)]
    (if init
      (emit-sexp "defvar" elisp-name (emit init)
                 (when docstring (pr-str docstring)))
      (emit-sexp "defvar" elisp-name "nil"))))

(defmethod emit-node :defn
  [{:keys [name docstring params body]}]
  (let [elisp-name (mangle-name name)
        elisp-params (str "(" (emit-list (map mangle-name params)) ")")
        elisp-body (str/join "\n  " (map emit body))]
    (if docstring
      (format "(defun %s %s\n  %s\n  %s)"
              elisp-name elisp-params (pr-str docstring) elisp-body)
      (format "(defun %s %s\n  %s)"
              elisp-name elisp-params elisp-body))))

(defmethod emit-node :fn
  [{:keys [params body]}]
  (let [elisp-params (str "(" (emit-list (map mangle-name params)) ")")
        elisp-body (str/join "\n    " (map emit body))]
    (format "(lambda %s\n    %s)" elisp-params elisp-body)))

(defmethod emit-node :let
  [{:keys [bindings body]}]
  (let [binding-strs (map (fn [{:keys [name init]}]
                            (format "(%s %s)" (mangle-name name) (emit init)))
                          bindings)
        bindings-block (str "(" (str/join "\n        " binding-strs) ")")
        body-str (str/join "\n    " (map emit body))]
    (format "(let* %s\n    %s)" bindings-block body-str)))

(defmethod emit-node :if
  [{:keys [test then else]}]
  (if else
    (emit-sexp "if" (emit test) (emit then) (emit else))
    (emit-sexp "when" (emit test) (emit then))))

(defmethod emit-node :when
  [{:keys [test body]}]
  (format "(when %s\n    %s)"
          (emit test)
          (str/join "\n    " (map emit body))))

(defmethod emit-node :cond
  [{:keys [clauses]}]
  (let [clause-strs (map (fn [{:keys [test expr]}]
                           (format "(%s %s)" (emit test) (emit expr)))
                         clauses)]
    (format "(cond\n  %s)" (str/join "\n  " clause-strs))))

(defmethod emit-node :do
  [{:keys [body]}]
  (format "(progn\n  %s)" (str/join "\n  " (map emit body))))

(defmethod emit-node :ns
  [{:keys [name clauses]}]
  (let [elisp-name (mangle-name name)
        requires (->> clauses
                      (filter #(= :require (first %)))
                      (mapcat rest)
                      (map (fn [r]
                             (if (vector? r)
                               (format "(require '%s)" (mangle-name (first r)))
                               (format "(require '%s)" (mangle-name r))))))
        provides (format "(provide '%s)" elisp-name)]
    (str ";;; " elisp-name ".el --- -*- lexical-binding: t; -*-\n"
         ";; Generated by ClojureElisp\n\n"
         "(require 'clojure-elisp-runtime)\n"
         (when (seq requires)
           (str (str/join "\n" requires) "\n"))
         "\n"
         ";;; Code:\n\n")))

(defmethod emit-node :loop
  [{:keys [bindings body]}]
  (let [names (map :name bindings)
        inits (map (comp emit :init) bindings)
        let-bindings (str/join " "
                               (map (fn [n i] (format "(%s %s)" (mangle-name n) i))
                                    names inits))
        body-str (str/join "\n      " (map emit body))]
    (format "(cl-labels ((recur (%s)\n      %s))\n    (recur %s))"
            (emit-list (map mangle-name names))
            body-str
            (emit-list inits))))

;; Emit try/catch/finally to Elisp condition-case and unwind-protect.
;; - try with catch: (condition-case err body (error handler))
;; - try with finally: (unwind-protect body cleanup)
;; - try with both: (condition-case err (unwind-protect body cleanup) (error handler))
(defmethod emit-node :try
  [{:keys [body catches finally]}]
  (let [;; Emit body expressions wrapped in progn if multiple
        body-str (if (= 1 (count body))
                   (emit (first body))
                   (format "(progn\n    %s)" (str/join "\n    " (map emit body))))

        ;; Emit finally as unwind-protect cleanup if present
        with-finally (if finally
                       (format "(unwind-protect\n    %s\n  %s)"
                               body-str
                               (str/join "\n  " (map emit finally)))
                       body-str)

        ;; Emit catch clauses - map all exception types to Elisp 'error'
        ;; Use first catch's binding name for the error variable
        catch-binding (when (seq catches)
                        (mangle-name (:name (first catches))))
        catch-handlers (when (seq catches)
                         ;; Combine all catch handlers into one error handler
                         ;; In Elisp, we use a single 'error' condition type
                         (let [handler-bodies (mapcat :body catches)
                               handler-str (if (= 1 (count handler-bodies))
                                             (emit (first handler-bodies))
                                             (format "(progn\n      %s)"
                                                     (str/join "\n      " (map emit handler-bodies))))]
                           (format "(error %s)" handler-str)))]

    (cond
      ;; Both catch and finally
      (and (seq catches) finally)
      (format "(condition-case %s\n    %s\n  %s)"
              catch-binding
              with-finally
              catch-handlers)

      ;; Only catch, no finally
      (seq catches)
      (format "(condition-case %s\n    %s\n  %s)"
              catch-binding
              body-str
              catch-handlers)

      ;; Only finally, no catch
      finally
      with-finally

      ;; Neither catch nor finally (just body)
      :else
      body-str)))

(defmethod emit-node :recur
  [{:keys [args]}]
  (emit-sexp "recur" (emit-list (map emit args))))

(defmethod emit-node :invoke
  [{:keys [fn args]}]
  (let [fn-str (emit fn)
        args-str (map emit args)]
    (apply emit-sexp fn-str args-str)))

(defmethod emit-node :default
  [node]
  (str ";; Unknown node: " (pr-str node)))

;; ============================================================================
;; Main Emit Function
;; ============================================================================

(defn emit
  "Emit an AST node to Elisp source code."
  [node]
  (emit-node node))

(comment
  (require '[clojure-elisp.analyzer :as ana])
  (emit (ana/analyze '(defn foo [x] (+ x 1))))
  (emit (ana/analyze '(let [a 1 b 2] (+ a b))))
  (emit (ana/analyze '(if (> x 0) "yes" "no"))))
