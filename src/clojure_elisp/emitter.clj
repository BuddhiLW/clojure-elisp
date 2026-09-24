(ns clojure-elisp.emitter
  "Emitter for ClojureElisp.

   Transforms AST nodes into Elisp source code strings."
  (:require [clojure.string :as str]
            [clojure-elisp.ast :as ast]
            [clojure-elisp.mappings :as mappings]
            [clojure-elisp.package-header :as package-header]
            [clojure-elisp.reader :as reader]
            [clojure-elisp.schema :as schema]
            [clojure-elisp.version :as version]
            [malli.core :as m]))

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
      ;; #()-reader auto-gensyms (p1__N#, rest__N#) carry a trailing `#` that
      ;; is invalid Elisp read syntax (e.g. `#)`); drop it — the numeric
      ;; counter already makes the name unique.
      (str/replace "#" "")
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
  "Map Clojure core functions to Elisp equivalents.
   See clojure-elisp.mappings for categorized sub-maps."
  mappings/core-fn-mapping)

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

(defn dedent-docstring
  "docstring with the indentation its continuation lines share removed.
   Clojure indents them under the opening quote; Emacs shows a docstring's
   lines as written, and checkdoc wants the second line flush left. Relative
   indentation (an indented example) is kept."
  [s]
  (let [[head & more] (str/split s #"\n" -1)
        indents (->> more (remove str/blank?) (map #(count (re-find #"^ *" %))))
        n       (if (seq indents) (apply min indents) 0)]
    (if (zero? n)
      s
      (str/join "\n" (cons head (map #(if (str/blank? %) "" (subs % n)) more))))))

(defn docstring-literal
  "Elisp string literal for a docstring, dedented (`dedent-docstring`).
   Newlines stay newlines: checkdoc reads a docstring line by line and wants
   its first line to be a sentence. A `(` opening a line is written `\\(`, as
   Emacs requires of a paren in column 0 inside a string."
  [s]
  (str "\""
       (-> (dedent-docstring s)
           (str/replace "\\" "\\\\")
           (str/replace "\"" "\\\"")
           (str/replace #"(?m)^\(" "\\\\("))
       "\""))

;; ============================================================================
;; Namespace-Qualified Names
;; ============================================================================

(defn ns-qualify-name
  "Produce the Elisp name for a definition, prefixed by namespace if applicable.
   Definitions in 'user namespace (the default) get no prefix.
   If private? is true, uses double-dash separator (Elisp convention for internal fns).
   Strips leading dash from name when ns-qualifying to avoid triple-dash
   (Clojure's -private convention conflicts with Elisp's ns--name convention)."
  ([name env]
   (ns-qualify-name name env false))
  ([name env private?]
   (let [current-ns (:ns env)
         separator   (if private? "--" "-")
         mangled     (mangle-name name)
         ;; Strip leading dash to avoid triple-dash: ns-- + -name = ns---name
         ;; Clojure's -private convention conflicts with Elisp's ns--name convention
         clean-name  (if (and private? (str/starts-with? mangled "-"))
                       (subs mangled 1)
                       mangled)]
     (if (and current-ns (not= current-ns 'user))
       (str (mangle-name current-ns) separator clean-name)
       (mangle-name name)))))

;; ============================================================================
;; AST Emitters
;; ============================================================================

(declare emit)

;; ============================================================================
;; Printing literal data
;; ============================================================================
;; The host printer is not portable: ClojureWasm's pr-str writes \f and \b
;; raw inside strings, and prints sets and large maps in its own hash order.
;; These print read data the way the JVM prints it, but collections in the
;; source order the reader recorded, so every host emits the same bytes.

(def ^:private string-char-escapes
  "clojure.core/char-escape-string: the chars pr-str escapes in a string."
  {\newline "\\n" \tab "\\t" \return "\\r" \" "\\\"" \\ "\\\\"
   \formfeed "\\f" \backspace "\\b"})

(defn pr-string
  "s as a double-quoted string literal, escaped as the JVM's pr-str does."
  [s]
  (str "\"" (str/escape s string-char-escapes) "\""))

(defn pr-data
  "Print a form read from source as the JVM's pr-str would, except that sets
   and maps list their elements in source order (reader/ordered-keys,
   reader/ordered-members) rather than in host hash order."
  [x]
  (cond
    (string? x) (pr-string x)
    (map? x)    (str "{" (str/join ", " (map #(str (pr-data %) " " (pr-data (get x %)))
                                             (reader/ordered-keys x)))
                     "}")
    (set? x)    (str "#{" (str/join " " (map pr-data (reader/ordered-members x))) "}")
    (vector? x) (str "[" (str/join " " (map pr-data x)) "]")
    (seq? x)    (str "(" (str/join " " (map pr-data x)) ")")
    :else       (pr-str x)))

(defmulti emit-node
  "Emit an AST node to Elisp string."
  :op)

(defmethod emit-node :const
  [{:keys [val type]}]
  (case type
    :nil "nil"
    :bool (if val "t" "nil")
    :number (str val)
    :string (pr-string val)
    :keyword (str ":" (name val))
    (str val)))

(defn emit-literal
  "Render a raw Clojure literal VALUE as *evaluated* Elisp source.
   Unlike `emit-node :const' (which dispatches on an analyzer-supplied :type
   keyword), this dispatches on the runtime value — safe for synthesized
   literals such as defmethod dispatch values, where passing (type v) (a Java
   Class) to the :const path silently fell through to (str v): strings lost
   their quotes, nil became an empty string, true/false became unbound symbols."
  [v]
  (cond
    (nil? v)     "nil"
    (true? v)    "t"
    (false? v)   "nil"
    (string? v)  (pr-string v)
    (char? v)    (pr-string (str v))
    (keyword? v) (str v)
    (symbol? v)  (str "'" v)
    (number? v)  (str v)
    (or (seq? v) (vector? v) (list? v)) (format "(list %s)" (str/join " " (map emit-literal v)))
    :else        (str v)))

(defn emit-case-pattern
  "Render a raw Clojure `case' test key as an Elisp `pcase' PATTERN.
   `pcase' matches strings/keywords/numbers by `equal' — the value semantics
   Clojure `case' needs. `cl-case' uses `eql' and silently fails on strings,
   nil, and booleans (its `t' clause is the else-marker)."
  [v]
  (cond
    (nil? v)     "'nil"
    (true? v)    "'t"
    (false? v)   "'nil"
    (string? v)  (pr-string v)
    (char? v)    (pr-string (str v))
    (keyword? v) (str v)
    (symbol? v)  (str "'" v)
    (number? v)  (str v)
    (or (seq? v) (vector? v) (list? v)) (format "(or %s)" (str/join " " (map emit-case-pattern v)))
    :else        (str v)))

(defmethod emit-node :local
  [{:keys [name fn-local?]}]
  ;; A letfn binding lives in the function namespace (cl-labels): as a value
  ;; it must be read with #', never as a variable.
  (if fn-local?
    (str "#'" (mangle-name name))
    (mangle-name name)))

(defmethod emit-node :var
  [{:keys [name ns private?]}]
  (cond
    ;; No namespace - check core mapping, then bare name
    (nil? ns)
    (if-let [elisp-name (get core-fn-mapping name)]
      elisp-name
      (mangle-name name))

    ;; clojure.core namespace - only through the core mapping. The analyzer
    ;; refuses an unmapped one; a node built by hand must not bring back the
    ;; `clojure-core-NAME' fallback, a function nothing defines.
    (= ns 'clojure.core)
    (or (get core-fn-mapping name)
        (throw (ex-info (str "clojure.core/" name " has no Emacs Lisp mapping")
                        {:symbol (symbol "clojure.core" (str name))})))

    ;; Other namespace - check fully-qualified symbol in mapping first
    :else
    (let [qualified-sym (symbol (str ns) (str name))
          separator     (if private? "--" "-")
          mangled       (mangle-name name)
          ;; Strip leading dash to avoid triple-dash (same as ns-qualify-name)
          clean-name    (if (and private? (str/starts-with? mangled "-"))
                          (subs mangled 1)
                          mangled)]
      (or (get core-fn-mapping qualified-sym)
          (str (mangle-name ns) separator clean-name)))))

(defmethod emit-node :vector
  [{:keys [items]}]
  (str "(list " (emit-list (map emit items)) ")"))

(defmethod emit-node :literal-vector
  [{:keys [items]}]
  (str "[" (emit-list (map emit items)) "]"))

(defmethod emit-node :transient-define-prefix
  [{:keys [name docstring arglist groups]}]
  (let [name-str   (mangle-name name)
        arglist-str (if (seq arglist)
                      (str "(" (emit-list (map str arglist)) ")")
                      "()")
        parts      (cond-> [(str "(transient-define-prefix " name-str " " arglist-str)]
                     docstring (conj (str "  " (docstring-literal docstring)))
                     (seq groups) (into (map #(str "  " (emit %)) groups)))]
    (str (str/join "\n" parts) ")")))

(defmethod emit-node :map
  [{:keys [keys vals]}]
  ;; An *evaluating* constructor, not a quoted literal: a quote would
  ;; suppress evaluation of every key and value. clel-array-map builds the
  ;; alist from entries the runtime records as entries, which is how a map
  ;; whose value is a list stays distinguishable from a list. The empty map
  ;; is nil.
  (if (empty? keys)
    "nil"
    (str "(clel-array-map "
         (str/join " " (mapcat (fn [k v] [(emit k) (emit v)]) keys vals))
         ")")))

(defmethod emit-node :set
  [{:keys [items]}]
  (str "(list " (emit-list (map emit items)) ")"))

(defn- quoted-data
  "Render quoted Clojure data as Elisp read syntax. A map is an alist and a
   set a list, as they are when evaluated; `pr-str' would print {...} and
   #{...}, which Elisp cannot read. Like `pr-data`, strings are escaped
   portably and maps and sets keep their source order."
  [x]
  (cond
    (string? x) (pr-string x)
    (map? x)    (str "(" (str/join " " (map (fn [k] (str "(" (quoted-data k) " . " (quoted-data (get x k)) ")"))
                                            (reader/ordered-keys x)))
                     ")")
    (set? x)    (str "(" (str/join " " (map quoted-data (reader/ordered-members x))) ")")
    (vector? x) (str "[" (str/join " " (map quoted-data x)) "]")
    (seq? x)    (str "(" (str/join " " (map quoted-data x)) ")")
    (true? x)   "t"
    (false? x)  "nil"
    :else       (pr-str x)))

(defmethod emit-node :quote
  [{:keys [form]}]
  (str "'" (quoted-data form)))

(defmethod emit-node :defmacro
  [{:keys [name docstring params body env]}]
  (let [elisp-name  (mangle-name name)
        elisp-params (str "(" (emit-list (map mangle-name params)) ")")
        elisp-body   (str/join "\n  " (map emit body))]
    (if docstring
      (format "(defmacro %s %s\n  %s\n  %s)"
              elisp-name elisp-params (docstring-literal docstring) elisp-body)
      (format "(defmacro %s %s\n  %s)"
              elisp-name elisp-params elisp-body))))

(defmethod emit-node :cl-defstruct
  [{:keys [name-or-opts docstring slots]}]
  (let [;; name-or-opts can be a symbol or a list with options
        name-str (if (symbol? name-or-opts)
                   (mangle-name name-or-opts)
                   ;; It's a list: (name (:constructor make-name) ...)
                   (str "(" (str/join " "
                                      (map (fn [x]
                                             (if (symbol? x) (mangle-name x) (quoted-data x)))
                                           name-or-opts)) ")"))
        ;; A slot is a name or (name default options...): data, as written
        slots-str (str/join " " (map (fn [s]
                                       (if (symbol? s)
                                         (mangle-name s)
                                         (quoted-data s)))
                                     slots))]
    (str "(cl-defstruct " name-str
         (when docstring (str "\n  " (docstring-literal docstring)))
         (when (seq slots) (str (if docstring "\n  " " ") slots-str))
         ")")))

(defmethod emit-node :cl-defun
  [{:keys [name docstring arglist body]}]
  (let [elisp-name (mangle-name name)
        ;; arglist may contain CL keywords like &optional, &key, &rest
        ;; Pass through as-is, only mangling regular symbol names
        elisp-arglist (str "(" (str/join " "
                                         (map (fn [a]
                                                (if (symbol? a)
                                                  (let [s (str a)]
                                                    (if (str/starts-with? s "&")
                                                      s  ;; preserve &optional, &key, &rest
                                                      (mangle-name a)))
                                                  (str a)))
                                              arglist)) ")")
        elisp-body (str/join "\n  " (map emit body))]
    (if docstring
      (format "(cl-defun %s %s\n  %s\n  %s)"
              elisp-name elisp-arglist (docstring-literal docstring) elisp-body)
      (format "(cl-defun %s %s\n  %s)"
              elisp-name elisp-arglist elisp-body))))

(defn- emit-cl-arglist
  "A CL-style arglist as written: parameter names mangled like the locals
   that reference them, lambda-list keywords, defaults and specializers
   passed through."
  [arglist]
  (str "("
       (str/join " "
                 (map (fn [p]
                        (cond
                          (symbol? p) (if (str/starts-with? (name p) "&") (str p) (mangle-name p))
                          (and (seq? p) (symbol? (first p)))
                          (str "(" (str/join " " (cons (mangle-name (first p)) (map quoted-data (rest p)))) ")")
                          :else (quoted-data p)))
                      arglist))
       ")"))

(defmethod emit-node :cl-defmethod
  [{:keys [name qualifiers arglist docstring body]}]
  (str "(cl-defmethod " (mangle-name name)
       (str/join (map #(str " " (quoted-data %)) qualifiers))
       " " (emit-cl-arglist arglist)
       (when docstring (str "\n  " (docstring-literal docstring)))
       (when (seq body) (str "\n  " (str/join "\n  " (map emit body))))
       ")"))

(defmethod emit-node :cl-defgeneric
  [{:keys [name arglist more]}]
  (str "(cl-defgeneric " (mangle-name name) " " (emit-cl-arglist arglist)
       (str/join (map #(str "\n  " (quoted-data %)) more))
       ")"))

(defmethod emit-node :def
  [{:keys [name docstring init env private?]}]
  ;; The docstring on its own line, as Emacs writes defvars: after a long
  ;; init its first line would pass the 80 columns checkdoc allows.
  (let [elisp-name (ns-qualify-name name env (boolean private?))]
    (cond
      docstring (format "(defvar %s %s\n  %s)" elisp-name (if init (emit init) "nil")
                        (docstring-literal docstring))
      init      (emit-sexp "defvar" elisp-name (emit init))
      :else     (format "(defvar %s)" elisp-name))))

(defn- nth-accessor
  "Emit an efficient nth accessor for an args list.
   Uses car/cadr/caddr for small indices, falls back to nth."
  [n]
  (case n
    0 "(car clel--args)"
    1 "(cadr clel--args)"
    2 "(caddr clel--args)"
    3 "(cadddr clel--args)"
    (format "(nth %d clel--args)" n)))

(defn- mangle-param
  "Mangle a parameter symbol, translating the Clojure rest marker `&` into
   Elisp's `&rest`. A bare `&` in an Elisp arglist is an ordinary required
   parameter, so `[x & xs]` must emit `(x &rest xs)`, not `(x & xs)`."
  [p]
  (if (= '& p) "&rest" (mangle-name p)))

(defn- emit-arity-param-bindings
  "Emit let-binding string for an arity's params from an args list."
  [{:keys [params fixed-params rest-param variadic?]}]
  (if variadic?
    (let [fixed-bindings (map-indexed
                          (fn [i p]
                            (format "(%s %s)" (mangle-name p) (nth-accessor i)))
                          fixed-params)
          rest-binding   (format "(%s (nthcdr %d clel--args))"
                                 (mangle-name rest-param)
                                 (count fixed-params))]
      (str/join " " (concat fixed-bindings [rest-binding])))
    (str/join " " (map-indexed
                   (fn [i p]
                     (format "(%s %s)" (mangle-name p) (nth-accessor i)))
                   params))))

(defn- emit-arity-cl-case
  "Emit the `(cl-case (length clel--args) ...)` arity-dispatch body shared by
   multi-arity defn and multi-arity fn. Fixed arities dispatch on arg count;
   the variadic arity (if any) is the `t` catch-all."
  [arities]
  (let [fixed-arities      (filter #(not= :variadic (:arity %)) arities)
        variadic-arity     (first (filter #(= :variadic (:arity %)) arities))
        emit-arity-case    (fn [{:keys [arity body] :as arity-node}]
                             (format "(%d (let (%s) %s))"
                                     arity
                                     (emit-arity-param-bindings arity-node)
                                     (str/join " " (map emit body))))
        emit-variadic-case (fn [arity-node]
                             (format "(t (let (%s) %s))"
                                     (emit-arity-param-bindings arity-node)
                                     (str/join " " (map emit (:body arity-node)))))
        case-clauses       (concat (map emit-arity-case fixed-arities)
                                   (when variadic-arity
                                     [(emit-variadic-case variadic-arity)]))]
    (format "(cl-case (length clel--args)\n    %s)" (str/join "\n    " case-clauses))))

(def ^:private recur-boundary-ops
  "Ops that establish their own recur target; recur below them is not the
   enclosing fn's."
  #{:loop :fn :defn :lambda :reify :defmethod :letfn :deftype :defrecord})

(defn- body-has-tail-recur?
  "True when a :recur node in body targets the enclosing fn, i.e. is not
   nested inside a construct that establishes its own recur target."
  [body]
  (letfn [(walk [node]
            (cond
              (map? node)        (cond
                                   (= :recur (:op node)) true
                                   (recur-boundary-ops (:op node)) false
                                   :else (some walk (vals node)))
              (sequential? node) (some walk node)
              :else              false))]
    (boolean (some walk body))))

(defn- unsupported-recur!
  [context]
  (throw (ex-info (str "recur in " context " is not supported by the Elisp emitter; "
                       "use loop/recur or an explicit named helper")
                  {:op :recur :context context})))

(defn- wrap-tail-recur
  "Wrap an emitted fn body so recur resolves to a cl-labels self-call over
   the fn's own params."
  [mangled-params body-str]
  (format "(cl-labels ((recur (%s)\n      %s))\n    (recur %s))"
          (str/join " " mangled-params)
          body-str
          (str/join " " mangled-params)))

(defn- checkdoc-params-comment
  "A `;; checkdoc-params: (...)` line naming parameters no docstring can be
   expected to mention: the ones the compiler generated (clel--args, p__N).
   checkdoc reads it and stops demanding them. Only a documented defun needs
   it: without a docstring checkdoc checks no arguments."
  [docstring generated-params]
  (when (and docstring (seq generated-params))
    (str ";; checkdoc-params: (" (str/join " " (map mangle-name generated-params)) ")")))

(defn- defun-form
  "(defun name arglist docstring? checkdoc-comment? body), one part per line."
  [elisp-name arglist docstring generated-params body-str]
  (let [lines (cond-> []
                docstring (conj (docstring-literal docstring))
                (checkdoc-params-comment docstring generated-params)
                (conj (checkdoc-params-comment docstring generated-params))
                true      (conj body-str))]
    (format "(defun %s %s\n  %s)" elisp-name arglist (str/join "\n  " lines))))

(defn- usage-name [i p]
  (if (symbol? p) (str/upper-case (mangle-name p)) (str "ARG" i)))

(defn arity-usage
  "The `\\(fn ...)` signature help and eldoc show for a multi-arity defun, whose
   real arglist is the dispatch's (&rest clel--args). Parameters every arity
   takes are required, those only longer arities take are &optional, and a
   variadic arity's rest parameter is &rest. Each position is named after the
   longest arity that has it:
     ([hour] [start end])     -> (fn START &optional END)
     ([] [x] [x y & more])    -> (fn &optional X Y &rest MORE)"
  [arities]
  (let [positional (map #(if (= :variadic (:arity %)) (:fixed-params %) (:params %)) arities)
        required   (apply min (map count positional))
        longest    (apply max-key count (reverse positional))
        rest-param (some #(when (= :variadic (:arity %)) (:rest-param %)) arities)
        names      (map-indexed usage-name longest)]
    (str "(fn "
         (str/join " " (concat (take required names)
                               (when (> (count names) required)
                                 (cons "&optional" (drop required names)))
                               (when rest-param
                                 ["&rest" (usage-name 0 rest-param)])))
         ")")))

(defn- with-usage
  "docstring ending in a `(fn ...)` usage line, unless it already has one.
   `help-split-fundoc` wants the line in the docstring's VALUE as `(fn ...)`;
   `docstring-literal` writes it `\\(fn ...)`, a paren opening a line."
  [docstring usage]
  (when docstring
    (if (re-find #"(?m)^\s*\\?\(fn[ )]" docstring)
      docstring
      (str (dedent-docstring docstring) "\n\n" usage))))

(defn- emit-multi-arity-defn
  "Emit a multi-arity defun with cl-case dispatch on arg count. The dispatch
   needs (&rest clel--args), so the docstring carries the real signature as a
   `\\(fn ...)` line and checkdoc is told clel--args is not a parameter to
   document."
  [elisp-name docstring arities]
  (when (some #(body-has-tail-recur? (:body %)) arities)
    (unsupported-recur! "a multi-arity fn"))
  (defun-form elisp-name "(&rest clel--args)"
              (with-usage docstring (arity-usage arities))
              ['clel--args]
              (emit-arity-cl-case arities)))

(defn- emit-single-arity-defn
  "Emit a single-arity defun. A variadic one gets its real arglist,
   (a b &rest more), so help, eldoc and checkdoc see the Clojure parameters
   and the docstring is the first body form."
  [elisp-name docstring params body variadic? generated-params]
  (let [_          (when (and variadic? (body-has-tail-recur? body))
                     (unsupported-recur! "a variadic fn"))
        arglist    (str "(" (emit-list (map mangle-param params)) ")")
        body-str   (str/join "\n  " (map emit body))
        elisp-body (if (and (not variadic?) (body-has-tail-recur? body))
                     (wrap-tail-recur (map mangle-name params) body-str)
                     body-str)]
    (defun-form elisp-name arglist docstring generated-params elisp-body)))

(defmethod emit-node :defn
  [{:keys [name docstring params body multi-arity? arities variadic? generated-params env private?]}]
  (let [elisp-name (ns-qualify-name name env (boolean private?))]
    (if multi-arity?
      (emit-multi-arity-defn elisp-name docstring arities)
      (emit-single-arity-defn elisp-name docstring params body variadic? generated-params))))

(defmethod emit-node :fn
  [{:keys [params body multi-arity? arities]}]
  (if multi-arity?
    ;; Multi-arity fn literal: dispatch on arg count like multi-arity defn,
    ;; but as an anonymous lambda over the synthetic clel--args arglist.
    (do
      (when (some #(body-has-tail-recur? (:body %)) arities)
        (unsupported-recur! "a multi-arity fn"))
      (format "(lambda (&rest clel--args)\n    %s)" (emit-arity-cl-case arities)))
    ;; Single arity: `&` -> `&rest` (via mangle-param); a tail recur wraps via
    ;; cl-labels, and a variadic fn containing recur is unsupported.
    (let [variadic?    (some #(= '& %) params)
          elisp-params (str "(" (emit-list (map mangle-param params)) ")")
          body-str     (str/join "\n    " (map emit body))
          elisp-body   (cond
                         (not (body-has-tail-recur? body)) body-str
                         variadic?                         (unsupported-recur! "a variadic fn")
                         :else (wrap-tail-recur (map mangle-name params) body-str))]
      (format "(lambda %s\n    %s)" elisp-params elisp-body))))

(defmethod emit-node :lazy-seq
  [{:keys [body]}]
  (let [body-str (str/join " " (map emit body))]
    (format "(clel-lazy-seq-create (lambda () %s))" body-str)))

(defmethod emit-node :with-eval-after-load
  [{:keys [feature body]}]
  (let [feature-str (emit feature)
        body-str    (str/join "\n  " (map emit body))]
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
        body-str   (str/join "\n    " (map emit body))]
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
;; Comment, Binding, Assert (clel-050)
;; ============================================================================

(defmethod emit-node :comment [_node] "")

(defmethod emit-node :binding
  [{:keys [bindings body]}]
  (let [binding-strs (map (fn [{:keys [name init]}]
                            (format "(%s %s)" (mangle-name name) (emit init)))
                          bindings)
        bindings-block (str "(" (str/join "\n        " binding-strs) ")")
        body-str (str/join "\n    " (map emit body))]
    (format "(let %s\n    %s)" bindings-block body-str)))

(defmethod emit-node :assert
  [{:keys [test message]}]
  (if message
    (format "(cl-assert %s %s)" (emit test) (emit message))
    (format "(cl-assert %s)" (emit test))))

;; ============================================================================
;; Iteration Forms (clel-035, clel-045)
;; ============================================================================

(defn- emit-doseq-clauses
  "Emit nested dolist/let/when forms for doseq clauses.
   Works from inside out: inner is the body expression,
   clauses are processed in reverse order to build nested structure."
  [clauses inner-body]
  (reduce
   (fn [inner clause]
     (case (:type clause)
       :binding
       (let [sym-str  (mangle-name (:sym clause))
             coll-str (emit (:coll clause))]
         (format "(dolist (%s (clel-seq %s))\n    %s)" sym-str coll-str inner))

       :let
       (let [let-strs (map (fn [{:keys [name init]}]
                             (format "(%s %s)" (mangle-name name) (emit init)))
                           (:bindings clause))]
         (format "(let* (%s)\n    %s)" (str/join " " let-strs) inner))

       :when
       (format "(when %s\n    %s)" (emit (:pred clause)) inner)

       :while
       ;; :while with catch/throw for early termination
       (format "(unless %s (cl-return))\n    %s" (emit (:pred clause)) inner)

       ;; Fallback
       inner))
   inner-body
   (reverse clauses)))

(defmethod emit-node :doseq
  [{:keys [clauses body]}]
  (let [;; Check if we have any :while clauses that need cl-block wrapper
        has-while? (some #(= :while (:type %)) clauses)
        body-str   (str/join "\n    " (map emit body))
        nested-str (emit-doseq-clauses clauses body-str)]
    (if has-while?
      ;; Wrap in cl-block for :while early termination
      (format "(cl-block nil\n  %s)" nested-str)
      nested-str)))

(defmethod emit-node :dotimes
  [{:keys [binding count body]}]
  (let [binding-str (mangle-name binding)
        count-str   (emit count)
        body-str    (str/join "\n    " (map emit body))]
    (format "(cl-dotimes (%s %s)\n    %s)" binding-str count-str body-str)))

(defn- emit-for-clauses
  "Emit nested mapping forms for 'for' list comprehension.
   Multi-binding for compiles to nested mapcan/mapcar chains.
   Each :binding becomes a mapcar/mapcan level,
   :when/:while filter at that level,
   :let adds intermediate bindings."
  [clauses body-expr]
  ;; Process clauses from right to left (innermost first)
  ;; Each :binding wraps the current expression in a mapcan
  (reduce
   (fn [inner clause]
     (case (:type clause)
       :binding
       (let [sym-str  (mangle-name (:sym clause))
             coll-str (emit (:coll clause))]
         ;; Use cl-mapcan to flatten nested lists
         (format "(cl-mapcan (lambda (%s) %s) (clel-seq %s))"
                 sym-str inner coll-str))

       :let
       (let [let-strs (map (fn [{:keys [name init]}]
                             (format "(%s %s)" (mangle-name name) (emit init)))
                           (:bindings clause))]
         (format "(let* (%s) %s)" (str/join " " let-strs) inner))

       :when
       ;; Filter: return list on match, nil otherwise
       (format "(when %s %s)" (emit (:pred clause)) inner)

       :while
       ;; :while - similar to :when but with throw for early termination
       ;; In practice, map-based for doesn't support early termination well
       ;; We approximate with :when semantics
       (format "(when %s %s)" (emit (:pred clause)) inner)

       ;; Fallback
       inner))
   body-expr
   (reverse clauses)))

(defmethod emit-node :for
  [{:keys [clauses body]}]
  (let [;; Build the innermost body expression wrapped in (list ...)
        body-expr (if (= 1 (count body))
                    (format "(list %s)" (emit (first body)))
                    (format "(list (progn %s))" (str/join " " (map emit body))))]
    (emit-for-clauses clauses body-expr)))

;; ============================================================================
;; Elisp Passthrough Forms (clel-060)
;; ============================================================================

(defmethod emit-node :dolist
  [{:keys [var list-form result body]}]
  (let [var-str      (mangle-name var)
        list-str     (emit list-form)
        binding-spec (if result
                       (format "(%s %s %s)" var-str list-str (emit result))
                       (format "(%s %s)" var-str list-str))
        body-str     (str/join "\n    " (map emit body))]
    (format "(dolist %s\n    %s)" binding-spec body-str)))

(defmethod emit-node :unless
  [{:keys [test body]}]
  (format "(unless %s\n    %s)"
          (emit test)
          (str/join "\n    " (map emit body))))

;; Clojure's single-binding when-let / if-let emit Emacs's STARRED macros:
;; the unstarred ones are obsolete since Emacs 31.1, so the byte-compiler
;; warns on every use, and with one binding the two spellings mean the same.

(defmethod emit-node :when-let
  [{:keys [var val body]}]
  (let [var-str  (mangle-name var)
        val-str  (emit val)
        body-str (str/join "\n    " (map emit body))]
    (format "(when-let* ((%s %s))\n    %s)" var-str val-str body-str)))

(defmethod emit-node :if-let
  [{:keys [var val then else]}]
  (let [var-str  (mangle-name var)
        val-str  (emit val)
        then-str (emit then)]
    (if else
      (format "(if-let* ((%s %s))\n    %s\n  %s)" var-str val-str then-str (emit else))
      (format "(if-let* ((%s %s))\n    %s)" var-str val-str then-str))))

(defmethod emit-node :when-let*
  [{:keys [bindings body]}]
  (let [binding-strs (map (fn [{:keys [var val]}]
                            (format "(%s %s)" (mangle-name var) (emit val)))
                          bindings)
        bindings-block (str "(" (str/join "\n         " binding-strs) ")")
        body-str (str/join "\n    " (map emit body))]
    (format "(when-let* %s\n    %s)" bindings-block body-str)))

(defmethod emit-node :if-let*
  [{:keys [bindings then else]}]
  (let [binding-strs (map (fn [{:keys [var val]}]
                            (format "(%s %s)" (mangle-name var) (emit val)))
                          bindings)
        bindings-block (str "(" (str/join "\n        " binding-strs) ")")
        then-str (emit then)]
    (if else
      (format "(if-let* %s\n    %s\n  %s)" bindings-block then-str (emit else))
      (format "(if-let* %s\n    %s)" bindings-block then-str))))

(defmethod emit-node :condition-case
  [{:keys [var body handlers]}]
  (let [var-str      (if var (mangle-name var) "nil")
        body-str     (emit body)
        handler-strs (map (fn [{:keys [condition body]}]
                            (format "(%s %s)" condition
                                    (str/join "\n      " (map emit body))))
                          handlers)]
    (format "(condition-case %s\n    %s\n  %s)"
            var-str body-str (str/join "\n  " handler-strs))))

(defmethod emit-node :pcase
  [{:keys [expr clauses]}]
  (let [expr-str    (emit expr)
        clause-strs (map (fn [{:keys [pattern body]}]
                           (let [;; Emit pattern - pass through raw for elisp patterns
                                 pat-str (cond
                                           ;; Wildcard _ must be checked before symbol? — it's the
                                           ;; pcase catch-all and must emit as bare _, not quoted '_
                                           (= pattern '_) "_"
                                           (symbol? pattern) (str "'" (name pattern))
                                           (keyword? pattern) (str "'" (name pattern))
                                           (string? pattern) (pr-string pattern)
                                           (number? pattern) (str pattern)
                                           ;; For list patterns like (or 'nil 'staged), (pred stringp), etc.
                                           ;; emit them raw
                                           (seq? pattern) (pr-data pattern)
                                           :else (str pattern))]
                             (format "(%s %s)" pat-str
                                     (str/join " " (map emit body)))))
                         clauses)]
    (format "(pcase %s\n  %s)" expr-str (str/join "\n  " clause-strs))))

(defn- emit-assign-target
  "The variable name a setq/set! writes: the analyzer's resolved TARGET node
   when there is one, otherwise NAME as written (a global Elisp variable)."
  [target name]
  (cond
    (nil? target)             (mangle-name name)
    (= :local (:op target))   (mangle-name (:name target))
    :else                     (emit-node target)))

(defmethod emit-node :setq
  [{:keys [pairs]}]
  (let [pair-strs (map (fn [{:keys [name target value]}]
                         (format "%s %s" (emit-assign-target target name) (emit value)))
                       pairs)]
    (format "(setq %s)" (str/join " " pair-strs))))

(defmethod emit-node :setf
  [{:keys [pairs]}]
  (let [pair-strs (map (fn [{:keys [place value]}]
                         (format "%s %s" (emit place) (emit value)))
                       pairs)]
    (format "(setf %s)" (str/join " " pair-strs))))

(defmethod emit-node :push
  [{:keys [value place]}]
  (format "(push %s %s)" (emit value) (emit place)))

(defmethod emit-node :unwind-protect
  [{:keys [body cleanup]}]
  (let [body-str    (emit body)
        cleanup-str (str/join "\n  " (map emit cleanup))]
    (format "(unwind-protect\n    %s\n  %s)" body-str cleanup-str)))

(defmethod emit-node :while
  [{:keys [test body]}]
  (format "(while %s\n    %s)"
          (emit test)
          (str/join "\n    " (map emit body))))

(defmethod emit-node :defvar-elisp
  [{:keys [name init docstring]}]
  (let [elisp-name (mangle-name name)]
    (cond
      (and init docstring)
      (format "(defvar %s %s\n  %s)" elisp-name (emit init) (docstring-literal docstring))

      init
      (format "(defvar %s %s)" elisp-name (emit init))

      docstring
      (format "(defvar %s nil\n  %s)" elisp-name (docstring-literal docstring))

      :else
      (format "(defvar %s)" elisp-name))))

(defmethod emit-node :function-quote
  [{:keys [symbol expr env]}]
  (if expr
    ;; Analyzer-resolved reference (auto-quoted higher-order fn arg):
    ;; reuse the inner :var/:local emit, just prefix the #' reader macro.
    (str "#'" (emit expr))
    (let [defs     (get env :defs)
          private? (get-in defs [symbol :private?])
          resolved (cond
                     (get core-fn-mapping symbol)
                     (get core-fn-mapping symbol)

                     ;; Symbol is a known def in the current namespace — qualify it
                     (and env (contains? defs symbol))
                     (ns-qualify-name symbol env (boolean private?))

                     :else
                     (mangle-name symbol))]
      (format "#'%s" resolved))))

(defmethod emit-node :let
  [{:keys [bindings body]}]
  (let [binding-strs   (map (fn [{:keys [name init]}]
                              (format "(%s %s)" (mangle-name name) (emit init)))
                            bindings)
        bindings-block (str "(" (str/join "\n        " binding-strs) ")")
        body-str       (str/join "\n    " (map emit body))]
    (format "(let* %s\n    %s)" bindings-block body-str)))

(defmethod emit-node :letfn
  [{:keys [fns body]}]
  (let [fn-strs  (map (fn [{:keys [name params body]}]
                        (let [param-str (str/join " " (map mangle-name params))
                              body-str  (str/join "\n      " (map emit body))]
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
  (let [elisp-name   (mangle-name name)
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
        type-spec  (if (= :default dispatch-val)
                     "t"
                     (format "(eql %s)" (emit-literal dispatch-val)))
        ;; Emit params - first param gets the type specializer. Remaining params
        ;; use mangle-param so a Clojure rest marker `&` becomes Elisp `&rest`.
        params-str (if (= 1 (count params))
                     (format "((%s %s))" (mangle-name (first params)) type-spec)
                     (format "((%s %s) %s)"
                             (mangle-name (first params))
                             type-spec
                             (str/join " " (map mangle-param (rest params)))))
        ;; Emit body with potential destructuring
        body-str   (if destructure-bindings
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
  [{:keys [_name methods]}]
  (let [generics (map (fn [{:keys [name params]}]
                        (let [method-name (mangle-name name)
                              params-str  (str/join " " (map mangle-name params))]
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
  (let [field-strs  (map mangle-name fields)
        ctor-params (str/join " " field-strs)
        ctor-args   (str/join " " (map (fn [f]
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
  (let [method-name       (mangle-name name)
        this-param        (first params)
        other-params      (rest params)
        params-str        (if (seq other-params)
                            (format "((%s %s) %s)"
                                    (mangle-name this-param) elisp-name
                                    (str/join " " (map mangle-name other-params)))
                            (format "((%s %s))"
                                    (mangle-name this-param) elisp-name))
        ;; Only bind fields not shadowed by method params
        param-set         (set params)
        accessible-fields (remove param-set fields)
        field-bindings    (map (fn [f]
                                 (format "(%s (%s-%s %s))"
                                         (mangle-name f) elisp-name
                                         (mangle-name f) (mangle-name this-param)))
                               accessible-fields)
        body-str          (str/join "\n    " (map emit body))]
    (if (seq accessible-fields)
      (format "(cl-defmethod %s %s\n  (let* (%s)\n    %s))"
              method-name params-str
              (str/join "\n         " field-bindings)
              body-str)
      (format "(cl-defmethod %s %s\n  %s)"
              method-name params-str body-str))))

(defmethod emit-node :defrecord
  [{:keys [name fields protocols]}]
  (let [elisp-name   (mangle-name name)
        struct-def   (emit-struct-def elisp-name fields)
        ctor-def     (emit-positional-ctor elisp-name fields)
        map-ctor-def (emit-map-ctor elisp-name fields)
        method-defs  (for [{:keys [methods]} protocols
                           method            methods]
                       (emit-record-method elisp-name fields method))]
    (str/join "\n\n" (concat [struct-def ctor-def map-ctor-def] method-defs))))

(defn- emit-type-method
  "Emit cl-defmethod for a deftype protocol method.
   Wraps body in cl-symbol-macrolet for field access (supports setf on mutable fields).
   Excludes fields shadowed by method params."
  [elisp-name fields {:keys [name params body]}]
  (let [method-name       (mangle-name name)
        this-param        (first params)
        other-params      (rest params)
        params-str        (if (seq other-params)
                            (format "((%s %s) %s)"
                                    (mangle-name this-param) elisp-name
                                    (str/join " " (map mangle-name other-params)))
                            (format "((%s %s))"
                                    (mangle-name this-param) elisp-name))
        param-set         (set params)
        accessible-fields (remove param-set fields)
        field-macrolets   (map (fn [f]
                                 (format "(%s (%s-%s %s))"
                                         (mangle-name f) elisp-name
                                         (mangle-name f) (mangle-name this-param)))
                               accessible-fields)
        body-str          (str/join "\n    " (map emit body))]
    (if (seq accessible-fields)
      (format "(cl-defmethod %s %s\n  (cl-symbol-macrolet (%s)\n    %s))"
              method-name params-str
              (str/join "\n                       " field-macrolets)
              body-str)
      (format "(cl-defmethod %s %s\n  %s)"
              method-name params-str body-str))))

(defmethod emit-node :deftype
  [{:keys [name fields protocols]}]
  (let [elisp-name  (mangle-name name)
        struct-def  (emit-struct-def elisp-name fields)
        ctor-def    (emit-positional-ctor elisp-name fields)
        method-defs (for [{:keys [methods]} protocols
                          method            methods]
                      (emit-type-method elisp-name fields method))]
    (str/join "\n\n" (concat [struct-def ctor-def] method-defs))))

(defmethod emit-node :set!
  [{:keys [target target-node value]}]
  (format "(setf %s %s)" (emit-assign-target target-node target) (emit value)))

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

(defmethod emit-node :instance?
  [{:keys [type value]}]
  ;; java.lang.String and String name the same class
  (let [type-sym (symbol (str/replace (str type) #"^java\.lang\." ""))]
    (format "(cl-typep %s '%s)" (emit value) (elisp-type-specializer type-sym))))

(defn- emit-extend-method
  "Emit cl-defmethod for extend-type/extend-protocol method."
  [type-sym {:keys [name params body]}]
  (let [method-name  (mangle-name name)
        elisp-type   (elisp-type-specializer type-sym)
        this-param   (first params)
        other-params (rest params)
        params-str   (if (seq other-params)
                       (format "((%s %s) %s)"
                               (mangle-name this-param) elisp-type
                               (str/join " " (map mangle-name other-params)))
                       (format "((%s %s))"
                               (mangle-name this-param) elisp-type))
        body-str     (str/join "\n  " (map emit body))]
    (format "(cl-defmethod %s %s\n  %s)"
            method-name params-str body-str)))

(defmethod emit-node :extend-type
  [{:keys [type protocols]}]
  (let [method-defs (for [{:keys [methods]} protocols
                          method            methods]
                      (emit-extend-method type method))]
    (str/join "\n\n" method-defs)))

(defmethod emit-node :extend-protocol
  [{:keys [_name extensions]}]
  (let [method-defs (for [{:keys [type methods]} extensions
                          method                 methods]
                      (emit-extend-method type method))]
    (str/join "\n\n" method-defs)))

(defmethod emit-node :satisfies?
  [{:keys [protocol value]}]
  (format "(clel-satisfies-p '%s %s)" (mangle-name protocol) (emit value)))

;; Reify type names are content-addressed: the same reify form gets the same
;; name in every compilation, on every host, and different forms get different
;; names. A process-wide counter made the name depend on what the process had
;; compiled before (a warm REPL and a fresh process disagreed), and restarted
;; at 1 in every process, so two files compiled separately could both define
;; clel--reify-1 and clobber each other once loaded into one Emacs. The name
;; carries the namespace prefix, as every definition of a package must.

(def ^:private reify-self
  "Stands in for a reify type's name until the name, a hash of the emitted
   definition, is known."
  "clel--reify-SELF")

(defn- utf-16-units
  "The UTF-16 code units of code point cp: what a JVM string holds for it."
  [cp]
  (if (< cp 0x10000)
    [cp]
    (let [v (- cp 0x10000)]
      [(+ 0xD800 (quot v 0x400)) (+ 0xDC00 (mod v 0x400))])))

(defn- content-hash
  "32-bit FNV-1a hash of s's UTF-16 code units, as 8 lowercase hex digits.
   The same on every host: exact integer arithmetic that fits in a long, and
   a host whose strings hold code points (ClojureWasm) hashes them as the
   JVM's UTF-16 units."
  [s]
  (let [h (reduce (fn [h unit]
                    (mod (* (bit-xor h unit) 16777619) 4294967296))
                  2166136261
                  (mapcat #(utf-16-units (int %)) s))]
    (apply str (map #(nth "0123456789abcdef" (mod (quot h %) 16))
                    [268435456 16777216 1048576 65536 4096 256 16 1]))))

(declare emit-reify)

(defmethod emit-node :reify
  [{:keys [env] :as node}]
  (let [code   (emit-reify node)
        ns     (:ns env)
        prefix (if (and ns (not= ns 'user)) (mangle-name ns) "clel")]
    (str/replace code reify-self
                 (str prefix "--reify-" (content-hash (str ns "\n" code))))))

(defn- emit-reify
  "The reify definition with reify-self standing in for its type name."
  [{:keys [protocols closed-over]}]
  (let [reify-name  reify-self
        ;; Emit struct definition with closed-over slots
        struct-def  (if (seq closed-over)
                      (format "(cl-defstruct (%s (:constructor %s--create)\n               (:copier nil))\n  %s)"
                              reify-name reify-name
                              (str/join "\n  " (map mangle-name closed-over)))
                      (format "(cl-defstruct (%s (:constructor %s--create)\n               (:copier nil)))"
                              reify-name reify-name))
        ;; Emit methods with closure access
        method-defs (for [{:keys [methods]}          protocols
                          {:keys [name params body]} methods]
                      (let [method-name    (mangle-name name)
                            this-param     (first params)
                            other-params   (rest params)
                            params-str     (if (seq other-params)
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
                            body-str       (str/join "\n    " (map emit body))]
                        (if (seq closed-over)
                          (format "(cl-defmethod %s %s\n  (let* (%s)\n    %s))"
                                  method-name params-str
                                  (str/join "\n         " field-bindings)
                                  body-str)
                          (format "(cl-defmethod %s %s\n  %s)"
                                  method-name params-str body-str))))
        ;; Constructor call with closed-over values
        ctor-args   (if (seq closed-over)
                      (str/join " " (map (fn [f] (format ":%s %s" (mangle-name f) (mangle-name f)))
                                         closed-over))
                      "")
        ctor-call   (if (seq closed-over)
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
                           (let [test-str (if (and (= :const (:op test))
                                                   (= :else (:val test)))
                                            "t"
                                            (emit test))]
                             (format "(%s %s)" test-str (emit expr))))
                         clauses)]
    (format "(cond\n  %s)" (str/join "\n  " clause-strs))))

(defmethod emit-node :case
  [{:keys [expr clauses default]}]
  ;; Use `pcase' (not `cl-case'): Clojure `case' dispatches by value, and
  ;; `pcase' matches strings/keywords/numbers by `equal'. `cl-case' uses `eql'
  ;; and silently mis-dispatches strings, nil, and booleans.
  (let [clause-strs (map (fn [{:keys [test expr]}]
                           (format "(%s %s)" (emit-case-pattern test) (emit expr)))
                         clauses)
        all-clauses (if default
                      (conj (vec clause-strs) (format "(_ %s)" (emit default)))
                      clause-strs)]
    (format "(pcase %s\n  %s)"
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
  [{:keys [name requires load-paths doc package]}]
  (let [elisp-name    (mangle-name name)
        ;; clojure.string & co. are compiled to runtime calls, not loaded, and
        ;; a namespace required with both :as and :refer is one feature.
        require-stmts (->> requires
                           (map :ns)
                           (remove mappings/runtime-provided-ns?)
                           distinct
                           (map #(format "(require '%s)" (mangle-name %))))
        load-path-block
        (when (seq load-paths)
          (let [add-stmts (str/join "\n    "
                                    (map (fn [dir]
                                           (format "(add-to-list 'load-path (expand-file-name \"%s\" this-dir))"
                                                   dir))
                                         load-paths))]
            (str "(let* ((this-dir (file-name-directory (or load-file-name buffer-file-name))))\n"
                 "    " add-stmts ")\n")))
        _provides     (format "(provide '%s)" elisp-name)
        prelude       (str (version/runtime-guard)
                           (when load-path-block
                             (str load-path-block))
                           (when (seq require-stmts)
                             (str (str/join "\n" require-stmts) "\n")))]
    ;; No trailing newline: emit-file puts one blank line between top-level
    ;; forms, and more would stack up before the first definition.
    (if package
      (str (package-header/render elisp-name doc package) "\n"
           (str/trimr prelude))
      (str ";;; " elisp-name ".el --- -*- lexical-binding: t; -*-\n"
           ";; Generated by ClojureElisp\n\n"
           prelude
           "\n"
           ";;; Code:"))))

(defmethod emit-node :loop
  [{:keys [bindings body]}]
  (let [names         (map :name bindings)
        inits         (map (comp emit :init) bindings)
        _let-bindings (str/join " "
                                (map (fn [n i] (format "(%s %s)" (mangle-name n) i))
                                     names inits))
        body-str      (str/join "\n      " (map emit body))]
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
        body-str       (if (= 1 (count body))
                         (emit (first body))
                         (format "(progn\n    %s)" (str/join "\n    " (map emit body))))

        ;; Emit finally as unwind-protect cleanup if present
        with-finally   (if finally
                         (format "(unwind-protect\n    %s\n  %s)"
                                 body-str
                                 (str/join "\n  " (map emit finally)))
                         body-str)

        ;; Emit catch clauses - map all exception types to Elisp 'error'
        ;; Use first catch's binding name for the error variable
        catch-binding  (when (seq catches)
                         (mangle-name (:name (first catches))))
        catch-handlers (when (seq catches)
                         ;; Combine all catch handlers into one error handler
                         ;; In Elisp, we use a single 'error' condition type
                         (let [handler-bodies (mapcat :body catches)
                               handler-str    (if (= 1 (count handler-bodies))
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
  [{:keys [exception exception-type tag value]}]
  (case exception-type
    :elisp-throw
    (format "(throw %s %s)" (emit tag) (emit value))

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

(defn- direct-call?
  "True when the callee node can sit in Elisp function position as is: a
   named function, a letfn binding, or a literal lambda. Anything else is a
   function VALUE, which a Lisp-2 must funcall."
  [{:keys [op fn-local? value?]}]
  (case op
    :var   (not value?)
    :local (boolean fn-local?)
    :fn    true
    false))

(defmethod emit-node :invoke
  [{:keys [fn args]}]
  (let [args-str (map emit args)]
    (if (direct-call? fn)
      (let [fn-str (if (= :local (:op fn)) (mangle-name (:name fn)) (emit fn))]
        ;; Arity-aware dispatch for assoc: 2-arg = Elisp native alist lookup,
        ;; 3+ = clel-assoc (Clojure put). Clojure assoc always needs 3+ args.
        (if (and (= fn-str "clel-assoc") (= 2 (count args)))
          (apply emit-sexp "assoc" args-str)
          (apply emit-sexp fn-str args-str)))
      (apply emit-sexp "funcall" (emit fn) args-str))))

(defmethod emit-node :define-minor-mode
  [{:keys [name docstring options body]}]
  (let [mode-name       (mangle-name name)
        ;; Emit options as keyword-value pairs — values are already analyzed AST nodes
        options-str     (->> options
                             (map (fn [[k v]]
                                    (str k " " (emit v))))
                             (str/join "\n  "))
        ;; Emit body forms
        body-str        (when (seq body)
                          (str/join "\n  " (map emit body)))
        ;; Build the full form
        parts           (cond-> [(str "(define-minor-mode " mode-name)]
                          docstring (conj (str "  " (docstring-literal docstring)))
                          (seq options-str) (conj (str "  " options-str))
                          (seq body-str) (conj (str "  " body-str)))]
    (str (str/join "\n" parts) ")")))

(defn- emit-option-val
  "Render a defgroup/defcustom keyword option's value, written as data. A
   function reference #'p reads as (var p) and must print as #'p: (var p)
   is a call to the void function `var' when the defcustom is evaluated."
  [v]
  (cond
    (nil? v) "nil"
    (true? v) "t"
    (false? v) "nil"
    (string? v) (pr-string v)
    (keyword? v) (str v)
    (and (seq? v) (= 'quote (first v)))
    (str "'" (quoted-data (second v)))
    (and (seq? v) (#{'var 'function} (first v)) (symbol? (second v)))
    (str "#'" (second v))
    :else (str v)))

(defmethod emit-node :defgroup
  [{:keys [name value docstring options]}]
  (let [group-name      (mangle-name name)
        ;; Emit value (typically nil)
        value-str       (emit-option-val value)
        ;; Emit options as keyword-value pairs
        options-str     (->> options
                             (map (fn [[k v]]
                                    (str k " " (emit-option-val v))))
                             (str/join "\n  "))
        ;; Build the full form
        parts           (cond-> [(str "(defgroup " group-name " " value-str)]
                          docstring (conj (str "  " (docstring-literal docstring)))
                          (seq options-str) (conj (str "  " options-str)))]
    (str (str/join "\n" parts) ")")))

(defmethod emit-node :defcustom
  [{:keys [name default docstring options]}]
  (let [var-name        (mangle-name name)
        ;; Emit default value (may be an analyzed AST node, e.g. :function-quote from #')
        default-str     (if (and (map? default) (:op default))
                          (emit default)
                          (emit-option-val default))
        ;; Emit options as keyword-value pairs
        options-str     (->> options
                             (map (fn [[k v]]
                                    (str k " " (emit-option-val v))))
                             (str/join "\n  "))
        ;; Build the full form
        parts           (cond-> [(str "(defcustom " var-name " " default-str)]
                          docstring (conj (str "  " (docstring-literal docstring)))
                          (seq options-str) (conj (str "  " options-str)))]
    (str (str/join "\n" parts) ")")))

(defmethod emit-node :default
  [node]
  ;; A comment spliced into a form comments out the rest of its line, so the
  ;; output silently loses code; refuse instead.
  (throw (ex-info (str "No Emacs Lisp emitter for AST node :op " (pr-str (:op node)))
                  (select-keys node [:op :form :line :column]))))

;; ============================================================================
;; Source Location Comments
;; ============================================================================

(def ^:dynamic *emit-source-comments*
  "When true, emit ;;; L<line>:C<col> comments before top-level forms."
  false)

(def ^:dynamic *validate-ast*
  "When true, validate AST nodes against ast-schemas before emission.
   Useful for development and testing to catch malformed AST nodes early.
   Default false for production performance."
  false)

(defn- source-comment
  "Generate a source location comment string for an AST node, or nil."
  [{:keys [line column]}]
  (when (and *emit-source-comments* line)
    (str ";;; L" line (when column (str ":C" column)))))

;; ============================================================================
;; Main Emit Function
;; ============================================================================

(def autoload-cookie
  "The magic comment package.el and loaddefs look for on the line before a
   definition to autoload."
  ";;;###autoload")

(defn emit
  "Emit an AST node to Elisp source code.
   When *emit-source-comments* is true, prepends ;;; L<line>:C<col> comments.
   When *validate-ast* is true, validates node structure before emission.
   A node marked :autoload? (^:autoload on a defn, define-minor-mode or
   defcustom name) gets the autoload cookie on the line before it."
  [node]
  (when *validate-ast*
    (ast/validate-ast-node node))
  (let [code    (cond->> (emit-node node)
                  (:autoload? node) (str autoload-cookie "\n"))
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
  (let [ns-node  (when (= :ns (:op (first ast-nodes))) (first ast-nodes))
        code     (str/join "\n\n" (mapv emit ast-nodes))
        elisp-ns (when ns-node (mangle-name (:name ns-node)))]
    (if elisp-ns
      (str code "\n\n(provide '" elisp-ns ")\n"
           ";;; " elisp-ns ".el ends here\n")
      code)))

;; ============================================================================
;; Function Contracts (Malli)
;; ============================================================================
;;
;; Contracts live on the plain-fn emit surface + the pure name helpers. `emit`
;; takes a shallow AST node (a map with :op) and returns Elisp source; the deep
;; recursive node shape is checked on demand via *validate-ast*, not per call.
;;
;; `emit-node` is a defmulti: instrumenting it would replace the MultiFn var root
;; with a plain fn (breaking methods/get-method/defmethod dispatch), so it is
;; intentionally left uncontracted — its input contract is `emit`'s, enforced one
;; call up. instrument! only wraps vars that carry an m/=> schema, so emit-node
;; stays an untouched MultiFn even when this ns is instrumented.

(m/=> mangle-name [:=> [:cat [:or :symbol :string]] :string])
(m/=> ns->prefix  [:=> [:cat :symbol] :string])
(m/=> ns-qualify-name
      [:function
       [:=> [:cat [:or :symbol :string] [:maybe :map]] :string]
       [:=> [:cat [:or :symbol :string] [:maybe :map] :boolean] :string]])
(m/=> emit        [:=> [:cat schema/ast-node-schema] :string])
(m/=> emit-file   [:=> [:cat [:sequential schema/ast-node-schema]] :string])

(comment
  (require '[clojure-elisp.analyzer :as ana])
  (emit (ana/analyze '(defn foo [x] (+ x 1))))
  (emit (ana/analyze '(let [a 1 b 2] (+ a b))))
  (emit (ana/analyze '(if (> x 0) "yes" "no"))))