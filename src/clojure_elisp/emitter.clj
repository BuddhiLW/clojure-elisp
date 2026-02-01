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
   ;; NOTE: 'and and 'or are special forms (for short-circuit evaluation),
   ;; handled in analyzer.clj and emitted via emit-node :and/:or

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

   ;; Numeric predicates
   'zero? "zerop"
   'pos? "cl-plusp"
   'neg? "cl-minusp"
   'even? "cl-evenp"
   'odd? "cl-oddp"

;; Collection predicates
   'coll? "clel-coll-p"
   'sequential? "clel-sequential-p"
   'associative? "clel-associative-p"

   ;; Boolean/nil predicates
   'some? "clel-some-p"
   'true? "clel-true-p"
   'false? "clel-false-p"

   ;; Collections
   'first "clel-first"
   'second "cadr"
   'last "clel-last"
   'butlast "butlast"
   'rest "clel-rest"
   'next "clel-next"
   'cons "cons"
   'conj "clel-conj"
   'count "length"
   'nth "nth"
   'get "clel-get"
   'contains? "clel-contains-p"
   'assoc "clel-assoc"
   'dissoc "clel-dissoc"
   'get-in "clel-get-in"
   'assoc-in "clel-assoc-in"
   'update "clel-update"
   'update-in "clel-update-in"
   'merge "clel-merge"
   'keys "clel-keys"
   'vals "clel-vals"
   'seq "clel-seq"
   'empty? "clel-empty-p"
   'into "clel-into"
   'reverse "reverse"
   'flatten "flatten-tree"

   ;; Sequence functions (lazy)
   'map "clel-map"
   'filter "clel-filter"
   'remove "cl-remove-if"
   'take "clel-take"
   'drop "clel-drop"
   'take-while "clel-take-while"
   'drop-while "clel-drop-while"
   'concat "clel-concat"
   'mapcat "clel-mapcat"
   'interleave "clel-interleave"
   'partition "clel-partition"

   ;; Sequence functions (eager)
   'reduce "clel-reduce"
   'sort "clel-sort"
   'sort-by "clel-sort-by"
   'group-by "clel-group-by"
   'frequencies "clel-frequencies"

   ;; Sequence generators
   'range "clel-range"
   'repeat "clel-repeat"
   'repeatedly "clel-repeatedly"

   ;; Sequence predicates
   'every? "clel-every-p"
   'some "clel-some"
   'not-every? "clel-not-every-p"
   'not-any? "clel-not-any-p"

   ;; Strings
   'str "clel-str"
   'subs "substring"
   'format "format"
   'pr-str "prin1-to-string"
   'println "message"
   'print "princ"

   ;; Math
   'min "cl-min"
   'max "cl-max"

   ;; Symbols
   'name "symbol-name"

   ;; Functions
   'apply "apply"
   'identity "identity"
   'constantly "clel-constantly"
   'partial "apply-partially"
   'comp "clel-comp"

   ;; Lazy sequences
   'realized? "clel-realized-p"
   'doall "clel-doall"
   'dorun "clel-dorun"

   ;; Atoms
   'atom "clel-atom"
   'deref "clel-deref"
   'clojure.core/deref "clel-deref"  ; @a reader macro expands to clojure.core/deref
   'reset! "clel-reset!"
   'swap! "clel-swap!"
   'add-watch "clel-add-watch"
   'remove-watch "clel-remove-watch"

   ;; Emacs buffer operations
   'message "message"
   'buffer-string "buffer-string"
   'buffer-substring "buffer-substring"
   'buffer-substring-no-properties "buffer-substring-no-properties"
   'point "point"
   'point-min "point-min"
   'point-max "point-max"
   'goto-char "goto-char"
   'forward-char "forward-char"
   'backward-char "backward-char"
   'forward-line "forward-line"
   'beginning-of-line "beginning-of-line"
   'end-of-line "end-of-line"
   'insert "insert"
   'insert-buffer-substring "insert-buffer-substring"
   'delete-char "delete-char"
   'delete-region "delete-region"
   'erase-buffer "erase-buffer"
   'buffer-name "buffer-name"
   'current-buffer "current-buffer"
   'set-buffer "set-buffer"
   'get-buffer "get-buffer"
   'get-buffer-create "get-buffer-create"
   'kill-buffer "kill-buffer"
   'buffer-live-p "buffer-live-p"
   'buffer-modified-p "buffer-modified-p"
   'set-buffer-modified-p "set-buffer-modified-p"
   'mark "mark"
   'set-mark "set-mark"
   'region-beginning "region-beginning"
   'region-end "region-end"
   'use-region-p "use-region-p"
   'narrow-to-region "narrow-to-region"
   'widen "widen"
   'buffer-narrowed-p "buffer-narrowed-p"

   ;; Emacs text properties
   'put-text-property "put-text-property"
   'get-text-property "get-text-property"
   'remove-text-properties "remove-text-properties"
   'propertize "propertize"

   ;; Emacs process operations
   'start-process "start-process"
   'call-process "call-process"
   'process-send-string "process-send-string"
   'process-send-region "process-send-region"
   'process-send-eof "process-send-eof"
   'set-process-filter "set-process-filter"
   'set-process-sentinel "set-process-sentinel"
   'process-buffer "process-buffer"
   'process-status "process-status"
   'process-live-p "process-live-p"
   'delete-process "delete-process"
   'get-process "get-process"
   'process-list "process-list"

   ;; Emacs file operations
   'find-file "find-file"
   'find-file-noselect "find-file-noselect"
   'write-file "write-file"
   'save-buffer "save-buffer"
   'buffer-file-name "buffer-file-name"
   'file-exists-p "file-exists-p"
   'file-readable-p "file-readable-p"
   'file-writable-p "file-writable-p"
   'file-directory-p "file-directory-p"
   'expand-file-name "expand-file-name"
   'file-name-directory "file-name-directory"
   'file-name-nondirectory "file-name-nondirectory"

   ;; Emacs window operations
   'selected-window "selected-window"
   'select-window "select-window"
   'window-buffer "window-buffer"
   'set-window-buffer "set-window-buffer"
   'split-window "split-window"
   'delete-window "delete-window"
   'other-window "other-window"})

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
;; Namespace-Qualified Names
;; ============================================================================

(defn ns-qualify-name
  "Produce the Elisp name for a definition, prefixed by namespace if applicable.
   Definitions in 'user namespace (the default) get no prefix."
  [name env]
  (let [current-ns (:ns env)]
    (if (and current-ns (not= current-ns 'user))
      (str (mangle-name current-ns) "-" (mangle-name name))
      (mangle-name name))))

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
  [{:keys [name ns]}]
  (cond
    ;; No namespace - check core mapping, then bare name
    (nil? ns)
    (if-let [elisp-name (get core-fn-mapping name)]
      elisp-name
      (mangle-name name))

    ;; clojure.core namespace - use core mapping if available
    (= ns 'clojure.core)
    (or (get core-fn-mapping name)
        (str "clojure-core-" (mangle-name name)))

    ;; Other namespace - fully qualified mangled name
    :else
    (str (mangle-name ns) "-" (mangle-name name))))

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

(defmethod emit-node :defmacro
  [_]
  ;; Macros are compile-time only — emit nothing
  "")

(defmethod emit-node :def
  [{:keys [name docstring init env]}]
  (let [elisp-name (ns-qualify-name name env)]
    (if init
      (emit-sexp "defvar" elisp-name (emit init)
                 (when docstring (pr-str docstring)))
      (emit-sexp "defvar" elisp-name "nil"))))

(defmethod emit-node :defn
  [{:keys [name docstring params body multi-arity? arities variadic? fixed-params rest-param env]}]
  (let [elisp-name (ns-qualify-name name env)]
    (cond
      ;; Multi-arity: emit cl-case dispatch
      multi-arity?
      (let [;; Separate fixed and variadic arities
            fixed-arities (filter #(not= :variadic (:arity %)) arities)
            variadic-arity (first (filter #(= :variadic (:arity %)) arities))

            ;; Helper to emit nth accessor for args list
            nth-accessor (fn [n]
                           (case n
                             0 "(car args)"
                             1 "(cadr args)"
                             2 "(caddr args)"
                             3 "(cadddr args)"
                             (format "(nth %d args)" n)))

            ;; Emit bindings for an arity's params
            emit-param-bindings (fn [{:keys [params fixed-params rest-param variadic?] :as arity}]
                                  (if variadic?
                                    ;; Variadic: bind fixed params and rest
                                    (let [fixed-bindings (map-indexed
                                                          (fn [i p]
                                                            (format "(%s %s)" (mangle-name p) (nth-accessor i)))
                                                          fixed-params)
                                          rest-binding (format "(%s (nthcdr %d args))"
                                                               (mangle-name rest-param)
                                                               (count fixed-params))]
                                      (str/join " " (concat fixed-bindings [rest-binding])))
                                    ;; Fixed arity: bind all params by position
                                    (str/join " " (map-indexed
                                                   (fn [i p]
                                                     (format "(%s %s)" (mangle-name p) (nth-accessor i)))
                                                   params))))

            ;; Emit a single arity case clause
            emit-arity-case (fn [{:keys [arity body] :as arity-node}]
                              (let [bindings (emit-param-bindings arity-node)
                                    body-str (str/join " " (map emit body))]
                                (format "(%d (let (%s) %s))" arity bindings body-str)))

            ;; Emit variadic clause (uses 't' as catch-all)
            emit-variadic-case (fn [{:keys [fixed-params body] :as arity-node}]
                                 (let [bindings (emit-param-bindings arity-node)
                                       body-str (str/join " " (map emit body))]
                                   (format "(t (let (%s) %s))" bindings body-str)))

            ;; Build case clauses
            case-clauses (concat
                          (map emit-arity-case fixed-arities)
                          (when variadic-arity
                            [(emit-variadic-case variadic-arity)]))

            case-body (str/join "\n    " case-clauses)]
        (if docstring
          (format "(defun %s (&rest args)\n  %s\n  (cl-case (length args)\n    %s))"
                  elisp-name (pr-str docstring) case-body)
          (format "(defun %s (&rest args)\n  (cl-case (length args)\n    %s))"
                  elisp-name case-body)))

      ;; Single-arity variadic: use &rest and let to destructure
      variadic?
      (let [elisp-body (str/join "\n  " (map emit body))
            fixed-bindings (map-indexed
                            (fn [i p]
                              (format "(%s (nth %d args))" (mangle-name p) i))
                            fixed-params)
            rest-binding (format "(%s (nthcdr %d args))"
                                 (mangle-name rest-param)
                                 (count fixed-params))
            all-bindings (str/join " " (concat fixed-bindings [rest-binding]))]
        (if docstring
          (format "(defun %s (&rest args)\n  %s\n  (let (%s)\n    %s))"
                  elisp-name (pr-str docstring) all-bindings elisp-body)
          (format "(defun %s (&rest args)\n  (let (%s)\n    %s))"
                  elisp-name all-bindings elisp-body)))

      ;; Single-arity non-variadic: simple defun
      :else
      (let [elisp-params (str "(" (emit-list (map mangle-name params)) ")")
            elisp-body (str/join "\n  " (map emit body))]
        (if docstring
          (format "(defun %s %s\n  %s\n  %s)"
                  elisp-name elisp-params (pr-str docstring) elisp-body)
          (format "(defun %s %s\n  %s)"
                  elisp-name elisp-params elisp-body))))))

(defmethod emit-node :fn
  [{:keys [params body]}]
  (let [elisp-params (str "(" (emit-list (map mangle-name params)) ")")
        elisp-body (str/join "\n    " (map emit body))]
    (format "(lambda %s\n    %s)" elisp-params elisp-body)))

(defmethod emit-node :lazy-seq
  [{:keys [body]}]
  (let [body-str (str/join " " (map emit body))]
    (format "(clel-lazy-seq-create (lambda () %s))" body-str)))

(defmethod emit-node :with-eval-after-load
  [{:keys [feature body]}]
  (let [feature-str (emit feature)
        body-str (str/join "\n  " (map emit body))]
    (format "(with-eval-after-load %s\n  %s)" feature-str body-str)))

;; ============================================================================
;; Emacs Buffer/Process Interop (clel-031)
;; ============================================================================

(defmethod emit-node :save-excursion
  [{:keys [body]}]
  (let [body-str (str/join "\n    " (map emit body))]
    (format "(save-excursion\n    %s)" body-str)))

(defmethod emit-node :save-restriction
  [{:keys [body]}]
  (let [body-str (str/join "\n    " (map emit body))]
    (format "(save-restriction\n    %s)" body-str)))

(defmethod emit-node :with-current-buffer
  [{:keys [buffer body]}]
  (let [buffer-str (emit buffer)
        body-str (str/join "\n    " (map emit body))]
    (format "(with-current-buffer %s\n    %s)" buffer-str body-str)))

(defmethod emit-node :with-temp-buffer
  [{:keys [body]}]
  (let [body-str (str/join "\n    " (map emit body))]
    (format "(with-temp-buffer\n    %s)" body-str)))

(defmethod emit-node :save-current-buffer
  [{:keys [body]}]
  (let [body-str (str/join "\n    " (map emit body))]
    (format "(save-current-buffer\n    %s)" body-str)))

(defmethod emit-node :with-output-to-string
  [{:keys [body]}]
  (let [body-str (str/join "\n    " (map emit body))]
    (format "(with-output-to-string\n    %s)" body-str)))

;; ============================================================================
;; Iteration Forms (clel-035)
;; ============================================================================

(defmethod emit-node :doseq
  [{:keys [binding coll body]}]
  (let [binding-str (mangle-name binding)
        coll-str (emit coll)
        body-str (str/join "\n    " (map emit body))]
    (format "(dolist (%s (clel-seq %s))\n    %s)" binding-str coll-str body-str)))

(defmethod emit-node :dotimes
  [{:keys [binding count body]}]
  (let [binding-str (mangle-name binding)
        count-str (emit count)
        body-str (str/join "\n    " (map emit body))]
    (format "(cl-dotimes (%s %s)\n    %s)" binding-str count-str body-str)))

(defmethod emit-node :for
  [{:keys [binding coll body] :as node}]
  (let [when-clause (:when node)
        let-clause (:let node)
        binding-str (mangle-name binding)
        coll-str (emit coll)
        ;; Build the innermost body expression
        body-expr (if (= 1 (count body))
                    (emit (first body))
                    (format "(progn %s)" (str/join " " (map emit body))))
        ;; Wrap with :let bindings if present
        body-with-let (if let-clause
                        (let [let-strs (map (fn [{:keys [name init]}]
                                              (format "(%s %s)" (mangle-name name) (emit init)))
                                            let-clause)]
                          (format "(let* (%s) %s)" (str/join " " let-strs) body-expr))
                        body-expr)
        ;; If :when is present, use cl-mapcan to filter
        ;; Otherwise use simple mapcar
        lambda-body (if when-clause
                      (format "(when %s (list %s))" (emit when-clause) body-with-let)
                      body-with-let)
        map-fn (if when-clause "cl-mapcan" "mapcar")]
    (format "(%s (lambda (%s) %s) (clel-seq %s))"
            map-fn binding-str lambda-body coll-str)))

(defmethod emit-node :let
  [{:keys [bindings body]}]
  (let [binding-strs (map (fn [{:keys [name init]}]
                            (format "(%s %s)" (mangle-name name) (emit init)))
                          bindings)
        bindings-block (str "(" (str/join "\n        " binding-strs) ")")
        body-str (str/join "\n    " (map emit body))]
    (format "(let* %s\n    %s)" bindings-block body-str)))

(defmethod emit-node :letfn
  [{:keys [fns body]}]
  (let [fn-strs (map (fn [{:keys [name params body]}]
                       (let [param-str (str/join " " (map mangle-name params))
                             body-str (str/join "\n      " (map emit body))]
                         (format "(%s (%s)\n      %s)"
                                 (mangle-name name)
                                 param-str
                                 body-str)))
                     fns)
        body-str (str/join "\n  " (map emit body))]
    (format "(cl-labels (%s)\n  %s)"
            (str/join "\n            " fn-strs)
            body-str)))

(defmethod emit-node :defmulti
  [{:keys [name dispatch-fn]}]
  (let [elisp-name (mangle-name name)
        ;; cl-defgeneric defines a generic function
        ;; The dispatch-fn is stored as documentation for now
        ;; Actual dispatch happens via cl-defmethod type specializers
        dispatch-str (emit dispatch-fn)]
    (format "(cl-defgeneric %s (arg)\n  \"Multimethod with dispatch: %s\")"
            elisp-name dispatch-str)))

(defmethod emit-node :defmethod
  [{:keys [name dispatch-val params body destructure-bindings]}]
  (let [elisp-name (mangle-name name)
        ;; Handle :default as 't' (catch-all in cl-defmethod)
        type-spec (if (= :default dispatch-val)
                    "t"
                    (format "(eql %s)" (emit {:op :const :val dispatch-val :type (type dispatch-val)})))
        ;; Emit params - first param gets the type specializer
        params-str (if (= 1 (count params))
                     (format "((%s %s))" (mangle-name (first params)) type-spec)
                     (format "((%s %s) %s)"
                             (mangle-name (first params))
                             type-spec
                             (str/join " " (map mangle-name (rest params)))))
        ;; Emit body with potential destructuring
        body-str (if destructure-bindings
                   ;; Wrap body in let* for destructuring (bindings are already analyzed)
                   (let [binding-strs (map (fn [{:keys [name init]}]
                                             (format "(%s %s)" (mangle-name name) (emit init)))
                                           destructure-bindings)]
                     (format "(let* (%s)\n      %s)"
                             (str/join "\n             " binding-strs)
                             (str/join "\n      " (map emit body))))
                   (str/join "\n    " (map emit body)))]
    (format "(cl-defmethod %s %s\n  %s)"
            elisp-name params-str body-str)))

(defmethod emit-node :defprotocol
  [{:keys [name methods]}]
  (let [generics (map (fn [{:keys [name params]}]
                        (let [method-name (mangle-name name)
                              params-str (str/join " " (map mangle-name params))]
                          (format "(cl-defgeneric %s (%s))" method-name params-str)))
                      methods)]
    (str/join "\n\n" generics)))

(defn- emit-struct-def
  "Emit cl-defstruct form for a record or type."
  [elisp-name fields]
  (let [field-strs (map mangle-name fields)]
    (format "(cl-defstruct (%s (:constructor %s--create)\n               (:copier nil))\n  %s)"
            elisp-name elisp-name
            (str/join "\n  " field-strs))))

(defn- emit-positional-ctor
  "Emit positional constructor ->Name."
  [elisp-name fields]
  (let [field-strs (map mangle-name fields)
        ctor-params (str/join " " field-strs)
        ctor-args (str/join " " (map (fn [f]
                                       (format ":%s %s" (mangle-name f) (mangle-name f)))
                                     fields))]
    (format "(defun ->%s (%s)\n  (%s--create %s))"
            elisp-name ctor-params elisp-name ctor-args)))

(defn- emit-map-ctor
  "Emit map constructor map->Name."
  [elisp-name fields]
  (let [map-args (str/join " " (map (fn [f]
                                      (format ":%s (clel-get m :%s)"
                                              (mangle-name f) (mangle-name f)))
                                    fields))]
    (format "(defun map->%s (m)\n  (%s--create %s))"
            elisp-name elisp-name map-args)))

(defn- emit-record-method
  "Emit cl-defmethod for a defrecord protocol method.
   Wraps body in let* for field access, excluding fields shadowed by params."
  [elisp-name fields {:keys [name params body]}]
  (let [method-name (mangle-name name)
        this-param (first params)
        other-params (rest params)
        params-str (if (seq other-params)
                     (format "((%s %s) %s)"
                             (mangle-name this-param) elisp-name
                             (str/join " " (map mangle-name other-params)))
                     (format "((%s %s))"
                             (mangle-name this-param) elisp-name))
        ;; Only bind fields not shadowed by method params
        param-set (set params)
        accessible-fields (remove param-set fields)
        field-bindings (map (fn [f]
                              (format "(%s (%s-%s %s))"
                                      (mangle-name f) elisp-name
                                      (mangle-name f) (mangle-name this-param)))
                            accessible-fields)
        body-str (str/join "\n    " (map emit body))]
    (if (seq accessible-fields)
      (format "(cl-defmethod %s %s\n  (let* (%s)\n    %s))"
              method-name params-str
              (str/join "\n         " field-bindings)
              body-str)
      (format "(cl-defmethod %s %s\n  %s)"
              method-name params-str body-str))))

(defmethod emit-node :defrecord
  [{:keys [name fields protocols]}]
  (let [elisp-name (mangle-name name)
        struct-def (emit-struct-def elisp-name fields)
        ctor-def (emit-positional-ctor elisp-name fields)
        map-ctor-def (emit-map-ctor elisp-name fields)
        method-defs (for [{:keys [methods]} protocols
                          method methods]
                      (emit-record-method elisp-name fields method))]
    (str/join "\n\n" (concat [struct-def ctor-def map-ctor-def] method-defs))))

(defn- emit-type-method
  "Emit cl-defmethod for a deftype protocol method.
   Wraps body in cl-symbol-macrolet for field access (supports setf on mutable fields).
   Excludes fields shadowed by method params."
  [elisp-name fields {:keys [name params body]}]
  (let [method-name (mangle-name name)
        this-param (first params)
        other-params (rest params)
        params-str (if (seq other-params)
                     (format "((%s %s) %s)"
                             (mangle-name this-param) elisp-name
                             (str/join " " (map mangle-name other-params)))
                     (format "((%s %s))"
                             (mangle-name this-param) elisp-name))
        param-set (set params)
        accessible-fields (remove param-set fields)
        field-macrolets (map (fn [f]
                               (format "(%s (%s-%s %s))"
                                       (mangle-name f) elisp-name
                                       (mangle-name f) (mangle-name this-param)))
                             accessible-fields)
        body-str (str/join "\n    " (map emit body))]
    (if (seq accessible-fields)
      (format "(cl-defmethod %s %s\n  (cl-symbol-macrolet (%s)\n    %s))"
              method-name params-str
              (str/join "\n                       " field-macrolets)
              body-str)
      (format "(cl-defmethod %s %s\n  %s)"
              method-name params-str body-str))))

(defmethod emit-node :deftype
  [{:keys [name fields protocols]}]
  (let [elisp-name (mangle-name name)
        struct-def (emit-struct-def elisp-name fields)
        ctor-def (emit-positional-ctor elisp-name fields)
        method-defs (for [{:keys [methods]} protocols
                          method methods]
                      (emit-type-method elisp-name fields method))]
    (str/join "\n\n" (concat [struct-def ctor-def] method-defs))))

(defmethod emit-node :set!
  [{:keys [target value]}]
  (format "(setf %s %s)" (mangle-name target) (emit value)))

;; Clojure type → Elisp type specializer mapping
(def ^:private clojure-to-elisp-type
  {"String" "string"
   "Number" "number"
   "Integer" "integer"
   "Long" "integer"
   "Float" "float"
   "Double" "float"
   "Boolean" "boolean"
   "nil" "null"
   "Object" "t"
   ;; Collection types
   "clojure.lang.PersistentVector" "vector"
   "clojure.lang.PersistentList" "cons"
   "clojure.lang.PersistentHashMap" "hash-table"
   "clojure.lang.Symbol" "symbol"
   "clojure.lang.Keyword" "symbol"})

(defn- elisp-type-specializer
  "Convert a Clojure type symbol to an Elisp type specializer."
  [type-sym]
  (let [type-str (str type-sym)]
    (or (get clojure-to-elisp-type type-str)
        ;; If not a built-in, assume it's a user-defined struct type
        (mangle-name type-sym))))

(defn- emit-extend-method
  "Emit cl-defmethod for extend-type/extend-protocol method."
  [type-sym {:keys [name params body]}]
  (let [method-name (mangle-name name)
        elisp-type (elisp-type-specializer type-sym)
        this-param (first params)
        other-params (rest params)
        params-str (if (seq other-params)
                     (format "((%s %s) %s)"
                             (mangle-name this-param) elisp-type
                             (str/join " " (map mangle-name other-params)))
                     (format "((%s %s))"
                             (mangle-name this-param) elisp-type))
        body-str (str/join "\n  " (map emit body))]
    (format "(cl-defmethod %s %s\n  %s)"
            method-name params-str body-str)))

(defmethod emit-node :extend-type
  [{:keys [type protocols]}]
  (let [method-defs (for [{:keys [methods]} protocols
                          method methods]
                      (emit-extend-method type method))]
    (str/join "\n\n" method-defs)))

(defmethod emit-node :extend-protocol
  [{:keys [name extensions]}]
  (let [method-defs (for [{:keys [type methods]} extensions
                          method methods]
                      (emit-extend-method type method))]
    (str/join "\n\n" method-defs)))

(defmethod emit-node :satisfies?
  [{:keys [protocol value]}]
  (format "(clel-satisfies-p '%s %s)" (mangle-name protocol) (emit value)))

;; Reify counter for generating unique type names
(def ^:private reify-counter (atom 0))

(defn- generate-reify-name []
  (str "clel--reify-" (swap! reify-counter inc)))

(defmethod emit-node :reify
  [{:keys [protocols closed-over]}]
  (let [reify-name (generate-reify-name)
        ;; Emit struct definition with closed-over slots
        struct-def (if (seq closed-over)
                     (format "(cl-defstruct (%s (:constructor %s--create)\n               (:copier nil))\n  %s)"
                             reify-name reify-name
                             (str/join "\n  " (map mangle-name closed-over)))
                     (format "(cl-defstruct (%s (:constructor %s--create)\n               (:copier nil)))"
                             reify-name reify-name))
        ;; Emit methods with closure access
        method-defs (for [{:keys [methods]} protocols
                          {:keys [name params body]} methods]
                      (let [method-name (mangle-name name)
                            this-param (first params)
                            other-params (rest params)
                            params-str (if (seq other-params)
                                         (format "((%s %s) %s)"
                                                 (mangle-name this-param) reify-name
                                                 (str/join " " (map mangle-name other-params)))
                                         (format "((%s %s))"
                                                 (mangle-name this-param) reify-name))
                            ;; Bind closed-over locals from struct fields
                            field-bindings (map (fn [f]
                                                  (format "(%s (%s-%s %s))"
                                                          (mangle-name f) reify-name
                                                          (mangle-name f) (mangle-name this-param)))
                                                closed-over)
                            body-str (str/join "\n    " (map emit body))]
                        (if (seq closed-over)
                          (format "(cl-defmethod %s %s\n  (let* (%s)\n    %s))"
                                  method-name params-str
                                  (str/join "\n         " field-bindings)
                                  body-str)
                          (format "(cl-defmethod %s %s\n  %s)"
                                  method-name params-str body-str))))
        ;; Constructor call with closed-over values
        ctor-args (if (seq closed-over)
                    (str/join " " (map (fn [f] (format ":%s %s" (mangle-name f) (mangle-name f)))
                                       closed-over))
                    "")
        ctor-call (if (seq closed-over)
                    (format "(%s--create %s)" reify-name ctor-args)
                    (format "(%s--create)" reify-name))]
    ;; Emit struct and methods, then return constructor call
    ;; Note: In real use, struct/methods should be hoisted to top-level
    (str struct-def "\n\n" (str/join "\n\n" method-defs) "\n\n" ctor-call)))

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

(defmethod emit-node :case
  [{:keys [expr clauses default]}]
  (let [clause-strs (map (fn [{:keys [test expr]}]
                           (format "(%s %s)"
                                   (emit {:op :const :val test :type (type test)})
                                   (emit expr)))
                         clauses)
        all-clauses (if default
                      (conj (vec clause-strs) (format "(t %s)" (emit default)))
                      clause-strs)]
    (format "(cl-case %s\n  %s)"
            (emit expr)
            (str/join "\n  " all-clauses))))

(defmethod emit-node :do
  [{:keys [body]}]
  (format "(progn\n  %s)" (str/join "\n  " (map emit body))))

(defmethod emit-node :and
  [{:keys [exprs]}]
  (if (empty? exprs)
    "t" ;; (and) returns true in Clojure
    (format "(and %s)" (str/join " " (map emit exprs)))))

(defmethod emit-node :or
  [{:keys [exprs]}]
  (if (empty? exprs)
    "nil" ;; (or) returns nil in Clojure
    (format "(or %s)" (str/join " " (map emit exprs)))))

(defmethod emit-node :ns
  [{:keys [name requires]}]
  (let [elisp-name (mangle-name name)
        require-stmts (->> requires
                           (map (fn [{:keys [ns]}]
                                  (format "(require '%s)" (mangle-name ns)))))
        provides (format "(provide '%s)" elisp-name)]
    (str ";;; " elisp-name ".el --- -*- lexical-binding: t; -*-\n"
         ";; Generated by ClojureElisp\n\n"
         "(require 'clojure-elisp-runtime)\n"
         (when (seq require-stmts)
           (str (str/join "\n" require-stmts) "\n"))
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

(defmethod emit-node :throw
  [{:keys [exception exception-type]}]
  (case exception-type
    :ex-info
    (let [[msg-node data-node] (:args exception)]
      (format "(signal 'error (list %s %s))"
              (emit msg-node)
              (emit data-node)))

    :constructor
    (let [[msg-node] (:args exception)]
      (format "(signal 'error %s)" (emit msg-node)))

    :rethrow
    (format "(signal 'error %s)" (emit exception))

    (format "(signal 'error %s)" (emit exception))))

(defmethod emit-node :recur
  [{:keys [args]}]
  (emit-sexp "recur" (emit-list (map emit args))))

(defmethod emit-node :interop-call
  [{:keys [method args]}]
  (let [args-str (map emit args)]
    (apply emit-sexp method args-str)))

(defmethod emit-node :elisp-call
  [{:keys [fn args]}]
  (let [args-str (map emit args)]
    (apply emit-sexp fn args-str)))

(defmethod emit-node :invoke
  [{:keys [fn args]}]
  (let [fn-str (emit fn)
        args-str (map emit args)]
    (apply emit-sexp fn-str args-str)))

(defmethod emit-node :define-minor-mode
  [{:keys [name docstring options body]}]
  (let [mode-name (mangle-name name)
        ;; Helper to emit option values (handles quoted forms, etc.)
        emit-option-val (fn [v]
                          (cond
                            (nil? v) "nil"
                            (true? v) "t"
                            (false? v) "nil"
                            (string? v) (pr-str v)
                            (keyword? v) (str v)
                            (and (seq? v) (= 'quote (first v)))
                            (str "'" (second v))
                            :else (str v)))
        ;; Emit options as keyword-value pairs
        options-str (->> options
                         (map (fn [[k v]]
                                (str (str k) " " (emit-option-val v))))
                         (str/join "\n  "))
        ;; Emit body forms
        body-str (when (seq body)
                   (str/join "\n  " (map emit body)))
        ;; Build the full form
        parts (cond-> [(str "(define-minor-mode " mode-name)]
                docstring (conj (str "  " (pr-str docstring)))
                (seq options-str) (conj (str "  " options-str))
                (seq body-str) (conj (str "  " body-str)))]
    (str (str/join "\n" parts) ")")))

(defmethod emit-node :defgroup
  [{:keys [name value docstring options]}]
  (let [group-name (mangle-name name)
        ;; Helper to emit option values (handles quoted forms, etc.)
        emit-option-val (fn [v]
                          (cond
                            (nil? v) "nil"
                            (true? v) "t"
                            (false? v) "nil"
                            (string? v) (pr-str v)
                            (keyword? v) (str v)
                            (and (seq? v) (= 'quote (first v)))
                            (str "'" (second v))
                            :else (str v)))
        ;; Emit value (typically nil)
        value-str (emit-option-val value)
        ;; Emit options as keyword-value pairs
        options-str (->> options
                         (map (fn [[k v]]
                                (str (str k) " " (emit-option-val v))))
                         (str/join "\n  "))
        ;; Build the full form
        parts (cond-> [(str "(defgroup " group-name " " value-str)]
                docstring (conj (str "  " (pr-str docstring)))
                (seq options-str) (conj (str "  " options-str)))]
    (str (str/join "\n" parts) ")")))

(defmethod emit-node :defcustom
  [{:keys [name default docstring options]}]
  (let [var-name (mangle-name name)
        ;; Helper to emit option values (handles quoted forms, etc.)
        emit-option-val (fn [v]
                          (cond
                            (nil? v) "nil"
                            (true? v) "t"
                            (false? v) "nil"
                            (string? v) (pr-str v)
                            (keyword? v) (str v)
                            (and (seq? v) (= 'quote (first v)))
                            (str "'" (second v))
                            :else (str v)))
        ;; Emit default value
        default-str (emit-option-val default)
        ;; Emit options as keyword-value pairs
        options-str (->> options
                         (map (fn [[k v]]
                                (str (str k) " " (emit-option-val v))))
                         (str/join "\n  "))
        ;; Build the full form
        parts (cond-> [(str "(defcustom " var-name " " default-str)]
                docstring (conj (str "  " (pr-str docstring)))
                (seq options-str) (conj (str "  " options-str)))]
    (str (str/join "\n" parts) ")")))

(defmethod emit-node :default
  [node]
  (str ";; Unknown node: " (pr-str node)))

;; ============================================================================
;; Source Location Comments
;; ============================================================================

(def ^:dynamic *emit-source-comments*
  "When true, emit ;;; L<line>:C<col> comments before top-level forms."
  false)

(defn- source-comment
  "Generate a source location comment string for an AST node, or nil."
  [{:keys [line column]}]
  (when (and *emit-source-comments* line)
    (str ";;; L" line (when column (str ":C" column)))))

;; ============================================================================
;; Main Emit Function
;; ============================================================================

(defn emit
  "Emit an AST node to Elisp source code.
   When *emit-source-comments* is true, prepends ;;; L<line>:C<col> comments."
  [node]
  (let [code (emit-node node)
        comment (source-comment node)]
    (if comment
      (str comment "\n" code)
      code)))

;; ============================================================================
;; File Emission
;; ============================================================================

(defn emit-file
  "Emit a sequence of AST nodes as a complete Elisp file.
   If the first node is :ns, appends (provide 'ns-name) at the end."
  [ast-nodes]
  (let [ns-node (when (= :ns (:op (first ast-nodes))) (first ast-nodes))
        code (str/join "\n\n" (map emit ast-nodes))
        elisp-ns (when ns-node (mangle-name (:name ns-node)))]
    (if elisp-ns
      (str code "\n\n(provide '" elisp-ns ")\n"
           ";;; " elisp-ns ".el ends here\n")
      code)))

(comment
  (require '[clojure-elisp.analyzer :as ana])
  (emit (ana/analyze '(defn foo [x] (+ x 1))))
  (emit (ana/analyze '(let [a 1 b 2] (+ a b))))
  (emit (ana/analyze '(if (> x 0) "yes" "no"))))
