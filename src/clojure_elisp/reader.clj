(ns clojure-elisp.reader
  "Portable reader for .cljel source: text in, forms out.

   The compiler runs on the JVM, on Babashka and on ClojureWasm. Their readers
   disagree: ClojureWasm has no clojure.core/read and no line numbers,
   Babashka's syntax-quote and #() expand to different forms than the JVM's,
   and every host numbers generated names from its own global counter. This
   namespace is one reader for all of them, written in portable Clojure, so
   the analyzer sees the same forms on every host.

   It follows clojure.lang.LispReader's grammar and output shape:
   - a non-empty list carries {:line :column} metadata, 1-based, the position
     of its opening paren (a ^meta prefix overrides both with its own
     position, as LispReader's does);
   - 'x @x #'x ~x ~@x read as (quote x), (clojure.core/deref x), (var x),
     (clojure.core/unquote x) and (clojure.core/unquote-splicing x);
   - syntax-quote expands to the same clojure.core/seq + concat forms, with
     symbols resolved against *ns*;
   - #(...) reads as (fn* [p1__N# ...] ...).

   Atoms (numbers, strings, characters, symbols, keywords, ##Inf) are
   delimited here and converted by the host's read-string, which already
   agrees across hosts on them.

   Deliberate differences from LispReader, all in service of identical output
   on every host:
   - a set, or a map past eight entries, iterates in hash order, and hash
     order differs per host; such literals carry their source order as
     ::order metadata, read back by ordered-keys and ordered-members, so the
     analyzer emits them as written on every host;
   - generated names come from clojure-elisp.names, numbered per compilation;
   - #? is rejected, as clojure.core/read rejects it without
     {:read-cond :allow}, and #= is rejected rather than evaluated.

   Pure: the only state is a cursor local to one read-forms call. Source is
   indexed as a char vector because string indexing is O(n) on ClojureWasm."
  (:require [clojure.string :as str]
            [clojure-elisp.names :as names]))

;; ============================================================================
;; Character classes
;; ============================================================================

(def ^:private whitespace-chars
  "The characters java.lang.Character/isWhitespace accepts, by code point."
  (into #{\space \tab \newline \return \formfeed}
        (map char)
        [0x0B 0x1C 0x1D 0x1E 0x1F 0x1680 0x2000 0x2001 0x2002 0x2003 0x2004
         0x2005 0x2006 0x2008 0x2009 0x200A 0x2028 0x2029 0x205F 0x3000]))

(defn whitespace-char?
  "True when c is whitespace as java.lang.Character/isWhitespace defines it
   (the comma is not). Portable: no host call."
  [c]
  (contains? whitespace-chars c))

(def ^:private macro-chars
  #{\" \; \' \@ \^ \` \~ \( \) \[ \] \{ \} \\ \% \#})

(def ^:private terminating-chars
  "Macro characters that end a token. # ' and % may appear inside one."
  (disj macro-chars \# \' \%))

(defn- whitespace?
  "Whitespace to the reader: whitespace-char? or the comma."
  [c]
  (or (= \, c) (whitespace-char? c)))

(defn- digit? [c]
  (and (some? c) (<= (int \0) (int c) (int \9))))

(defn- token-end? [c]
  (or (nil? c) (whitespace? c) (contains? terminating-chars c)))

(defn- number-end? [c]
  (or (nil? c) (whitespace? c) (contains? macro-chars c)))

;; ============================================================================
;; Cursor
;; ============================================================================

(defn- line-starts
  "Offsets at which each line of cs begins."
  [cs]
  (let [n (count cs)]
    (loop [i 0 acc (transient [0])]
      (if (< i n)
        (recur (inc i) (if (= \newline (nth cs i)) (conj! acc (inc i)) acc))
        (persistent! acc)))))

(defn- make-ctx
  [s]
  (let [cs (vec s)]
    {:cs          cs
     :n           (count cs)
     :pos         (volatile! 0)
     :line-starts (line-starts cs)
     ;; #() argument env: nil outside #(), else a map of arg index -> symbol.
     :arg-env     (volatile! nil)
     ;; syntax-quote gensym env: nil outside `, else a map of foo# -> symbol.
     :gensym-env  (volatile! nil)}))

(defn- location
  "{:line :column} of offset p, both 1-based."
  [{:keys [line-starts]} p]
  (loop [lo 0 hi (dec (count line-starts))]
    (if (< lo hi)
      (let [mid (quot (+ lo hi 1) 2)]
        (if (<= (nth line-starts mid) p)
          (recur mid hi)
          (recur lo (dec mid))))
      {:line (inc lo) :column (inc (- p (nth line-starts lo)))})))

(defn- pos [ctx] @(:pos ctx))

(defn- peek-char
  ([ctx] (peek-char ctx 0))
  ([{:keys [cs n] :as ctx} ahead]
   (let [p (+ (pos ctx) ahead)]
     (when (< p n) (nth cs p)))))

(defn- next-char!
  [ctx]
  (when-let [c (peek-char ctx)]
    (vswap! (:pos ctx) inc)
    c))

(defn- text
  "Source text between offsets start (inclusive) and end (exclusive)."
  [{:keys [cs]} start end]
  (apply str (subvec cs start end)))

(defn- reader-error
  "Throw a reader error located at the cursor (or at offset p)."
  ([ctx msg] (reader-error ctx msg {}))
  ([ctx msg data]
   (throw (ex-info msg (merge {:type ::reader-error}
                              (location ctx (min (pos ctx) (max 0 (dec (:n ctx)))))
                              data)))))

;; ============================================================================
;; Tokens and atoms
;; ============================================================================

(defn- read-token-text
  "Consume a token that began at offset start: every char up to whitespace or
   a terminating macro char."
  [ctx start]
  (loop []
    (when-not (token-end? (peek-char ctx))
      (next-char! ctx)
      (recur)))
  (text ctx start (pos ctx)))

(defn- host-read
  "Read one atom from its source text with the host reader."
  [ctx token]
  (try
    (read-string token)
    (catch Exception e
      (reader-error ctx (or (ex-message e) (str "Invalid token: " token))
                    {:token token}))))

(def ^:private int-re
  "clojure.lang.LispReader's intPat without its capture groups."
  #"[-+]?(?:0|[1-9][0-9]*|0[xX][0-9A-Fa-f]+|0[0-7]+|[1-9][0-9]?[rR][0-9A-Za-z]+)N?")

(def ^:private ratio-re #"[-+]?[0-9]+/[0-9]+")

(def ^:private float-re #"[-+]?[0-9]+(?:\.[0-9]*)?(?:[eE][-+]?[0-9]+)?M?")

(defn- invalid-number
  [ctx token]
  (reader-error ctx (str "Invalid number: " token)
                {:type ::invalid-number :token token}))

(defn- read-number
  [ctx]
  (let [start (pos ctx)]
    (next-char! ctx)
    (loop []
      (when-not (number-end? (peek-char ctx))
        (next-char! ctx)
        (recur)))
    (let [token (text ctx start (pos ctx))]
      (if (or (re-matches int-re token)
              (re-matches ratio-re token)
              (re-matches float-re token))
        (let [n (try (read-string token) (catch Exception _ nil))]
          (if (number? n) n (invalid-number ctx token)))
        (invalid-number ctx token)))))

(defn- read-symbolic
  "Read a symbol, keyword, nil, true or false."
  [ctx]
  (let [start (pos ctx)]
    (next-char! ctx)
    (host-read ctx (read-token-text ctx start))))

(defn- read-string-literal
  [ctx]
  (let [start (pos ctx)]
    (next-char! ctx)
    (loop []
      (case (next-char! ctx)
        nil  (reader-error ctx "EOF while reading string")
        \\   (do (next-char! ctx) (recur))
        \"   nil
        (recur)))
    (host-read ctx (text ctx start (pos ctx)))))

(defn- read-regex
  "#\"...\": the text between the quotes goes to re-pattern verbatim."
  [ctx]
  (next-char! ctx)
  (let [start (pos ctx)]
    (loop []
      (case (next-char! ctx)
        nil (reader-error ctx "EOF while reading regex")
        \\  (do (next-char! ctx) (recur))
        \"  nil
        (recur)))
    (re-pattern (text ctx start (dec (pos ctx))))))

(defn- read-character
  [ctx]
  (let [start (pos ctx)]
    (next-char! ctx)
    (when-not (next-char! ctx)
      (reader-error ctx "EOF while reading character"))
    (let [token (read-token-text ctx (inc start))]
      (if (= 1 (count token))
        (first token)
        (host-read ctx (str "\\" token))))))

;; ============================================================================
;; Forms
;; ============================================================================

(declare read-form)

(def ^:private skip
  "Returned by comment and #_ readers: nothing was read. Compared with
   identical?, so no form read from source can be mistaken for it."
  (list ::skip))

(def ^:private eof
  "Returned by read-next at end of input when no other value is asked for."
  (list ::eof))

(defn- skip-whitespace!
  [ctx]
  (loop []
    (when-let [c (peek-char ctx)]
      (when (whitespace? c)
        (next-char! ctx)
        (recur)))))

(defn- skip-line!
  [ctx]
  (loop []
    (when-let [c (next-char! ctx)]
      (when-not (= \newline c)
        (recur))))
  skip)

(defn- read-next
  "Read the next form. At end of input return eof-value when given, else throw
   the error for an unterminated form."
  [ctx eof-value]
  (loop []
    (skip-whitespace! ctx)
    (if-let [c (peek-char ctx)]
      (let [form (read-form ctx c)]
        (if (identical? form skip) (recur) form))
      eof-value)))

(defn- read-required
  "Read the next form; end of input is an error."
  [ctx]
  (let [form (read-next ctx eof)]
    (if (identical? form eof)
      (reader-error ctx "EOF while reading")
      form)))

(defn- read-delimited
  "Read forms up to the closing delimiter. The opening one is already consumed
   and started at offset start."
  [ctx close start]
  (loop [items (transient [])]
    (skip-whitespace! ctx)
    (let [c (peek-char ctx)]
      (cond
        (nil? c)
        (reader-error ctx (str "EOF while reading, starting at line "
                               (:line (location ctx start))))

        (= close c)
        (do (next-char! ctx) (persistent! items))

        :else
        (let [form (read-form ctx c)]
          (recur (if (identical? form skip) items (conj! items form))))))))

(defn- read-list
  [ctx]
  (let [start (pos ctx)]
    (next-char! ctx)
    (let [items (read-delimited ctx \) start)]
      (if (empty? items)
        ()
        (with-meta (apply list items) (location ctx start))))))

(defn- read-vector
  [ctx]
  (let [start (pos ctx)]
    (next-char! ctx)
    (read-delimited ctx \] start)))

(defn- duplicate
  "The first element of xs that occurs more than once, wrapped in a vector, or
   nil when all are distinct."
  [xs]
  (loop [seen #{} [x & more :as xs] (seq xs)]
    (cond
      (empty? xs)         nil
      (contains? seen x)  [x]
      :else               (recur (conj seen x) more))))

(defn ordered-keys
  "The keys of map m in the order its literal wrote them, when this reader
   read it; otherwise (keys m)."
  [m]
  (or (::order (meta m)) (keys m)))

(defn ordered-members
  "The members of set s in the order its literal wrote them, when this reader
   read it; otherwise (seq s)."
  [s]
  (or (::order (meta s)) (seq s)))

(defn- with-order
  "Record the source order of coll's elements (map keys or set members) as
   ::order metadata, when the host might iterate coll in another order: any
   set, and a map past eight entries (smaller maps are array maps, which
   iterate in insertion order on every host)."
  [coll ordered]
  (if (or (and (set? coll) (> (count coll) 1))
          (and (map? coll) (> (count coll) 8)))
    (vary-meta coll assoc ::order (vec ordered))
    coll))

(defn- build-map
  "Map literal from alternating keys and values."
  [ctx kvs]
  (when (odd? (count kvs))
    (reader-error ctx "Map literal must contain an even number of forms"))
  (let [ks (take-nth 2 kvs)]
    (when-let [[k] (duplicate ks)]
      (reader-error ctx (str "Duplicate key: " (pr-str k))))
    (with-order (apply array-map kvs) ks)))

(defn- read-map
  [ctx]
  (let [start (pos ctx)]
    (next-char! ctx)
    (build-map ctx (read-delimited ctx \} start))))

(defn- read-set
  [ctx start]
  (next-char! ctx)
  (let [items (read-delimited ctx \} start)]
    (when-let [[x] (duplicate items)]
      (reader-error ctx (str "Duplicate key: " (pr-str x))))
    (with-order (set items) items)))

(defn- read-wrapped
  "'x @x ~x: consume the prefix, wrap the next form as (sym form)."
  [ctx sym width]
  (dotimes [_ width] (next-char! ctx))
  (list sym (read-required ctx)))

;; ----------------------------------------------------------------------------
;; Metadata
;; ----------------------------------------------------------------------------

(defn- meta-map
  [ctx m]
  (cond
    (or (symbol? m) (string? m)) {:tag m}
    (keyword? m)                 {m true}
    (vector? m)                  {:param-tags m}
    (map? m)                     m
    :else (reader-error ctx "Metadata must be Symbol,Keyword,String,Map or Vector")))

(defn- read-meta
  "^m form (and the older #^m form). start is the offset of the ^."
  [ctx start]
  (let [m    (meta-map ctx (read-required ctx))
        form (read-required ctx)]
    (when-not (or (symbol? form) (seq? form) (vector? form) (map? form)
                  (set? form))
      (reader-error ctx "Metadata can only be applied to IMetas"))
    (let [m (if (seq? form) (merge m (location ctx start)) m)]
      (with-meta form (merge (meta form) m)))))

;; ----------------------------------------------------------------------------
;; Syntax-quote
;; ----------------------------------------------------------------------------

(def ^:private special-forms
  "clojure.lang.Compiler/specials: quoted as-is by syntax-quote. Built from a
   vector because `cljw build` cannot serialize a hash-backed set constant."
  (set '[def loop* recur if case* let* letfn* do fn* quote var
         clojure.core/import* . set! deftype* reify* try throw monitor-enter
         monitor-exit catch finally new &]))

(defn- unquote? [form]
  (and (seq? form) (= 'clojure.core/unquote (first form))))

(defn- unquote-splicing? [form]
  (and (seq? form) (= 'clojure.core/unquote-splicing (first form))))

(defn- class-name
  "The name syntax-quote gives a class: java.lang.String on the JVM."
  [c]
  (str/replace (str c) #"^(class|interface) " ""))

(defn- resolve-symbol
  "clojure.lang.Compiler/resolveSymbol against *ns*."
  [sym]
  (let [ns-part (namespace sym)
        nm      (name sym)]
    (cond
      (pos? (or (str/index-of nm ".") -1))
      sym

      ns-part
      (let [target (get (ns-aliases *ns*) (symbol ns-part))]
        (if target
          (symbol (str (ns-name target)) nm)
          sym))

      :else
      (let [mapping (get (ns-map *ns*) sym)]
        (cond
          (nil? mapping) (symbol (str (ns-name *ns*)) nm)
          (var? mapping) (let [m (meta mapping)]
                           (symbol (str (ns-name (:ns m))) (str (:name m))))
          :else          (symbol (class-name mapping)))))))

(defn- syntax-quote-symbol
  [ctx sym]
  (let [ns-part (namespace sym)
        nm      (name sym)]
    (cond
      (and (nil? ns-part) (str/ends-with? nm "#"))
      (let [genv @(:gensym-env ctx)]
        (or (get genv sym)
            (let [gs (symbol (str (subs nm 0 (dec (count nm)))
                                  "__" (names/next-id) "__auto__"))]
              (vswap! (:gensym-env ctx) assoc sym gs)
              gs)))

      (and (nil? ns-part) (str/ends-with? nm "."))
      (let [resolved (resolve-symbol (symbol (subs nm 0 (dec (count nm)))))]
        (symbol (str (name resolved) ".")))

      (and (nil? ns-part) (str/starts-with? nm "."))
      sym

      :else
      (let [maybe-class (when ns-part (get (ns-map *ns*) (symbol ns-part)))]
        (if (and maybe-class (not (var? maybe-class)))
          (symbol (class-name maybe-class) nm)
          (resolve-symbol sym))))))

(declare syntax-quote syntax-quote-form)

(defn- sq-expand-list
  [ctx items]
  (map (fn [item]
         (cond
           (unquote? item)          (list 'clojure.core/list (second item))
           (unquote-splicing? item) (second item)
           :else                    (list 'clojure.core/list (syntax-quote ctx item))))
       items))

(defn- sq-collection
  [ctx ctor items]
  (list 'clojure.core/apply ctor
        (list 'clojure.core/seq
              (cons 'clojure.core/concat (sq-expand-list ctx items)))))

(defn- syntax-quote
  "clojure.lang.LispReader's syntaxQuote."
  [ctx form]
  (if (unquote? form)
    (second form)
    (syntax-quote-form ctx form)))

(defn- syntax-quote-form
  [ctx form]
  (let [expanded
        (cond
          (contains? special-forms form)
          (list 'quote form)

          (symbol? form)
          (list 'quote (syntax-quote-symbol ctx form))

          (unquote-splicing? form)
          (reader-error ctx "splice not in list")

          (map? form)
          (sq-collection ctx 'clojure.core/hash-map
                         (mapcat (fn [k] [k (get form k)]) (ordered-keys form)))

          (vector? form)
          (sq-collection ctx 'clojure.core/vector form)

          (set? form)
          (sq-collection ctx 'clojure.core/hash-set (ordered-members form))

          (seq? form)
          (if (seq form)
            (list 'clojure.core/seq
                  (cons 'clojure.core/concat (sq-expand-list ctx form)))
            (list 'clojure.core/list))

          (or (keyword? form) (number? form) (char? form) (string? form))
          form

          :else
          (list 'quote form))
        form-meta (when (or (symbol? form) (coll? form))
                    (not-empty (dissoc (meta form) ::order)))]
    ;; As LispReader does: wrap only when there is metadata beyond the
    ;; location, but then carry the location too.
    (if (seq (dissoc form-meta :line :column))
      (list 'clojure.core/with-meta expanded (syntax-quote ctx form-meta))
      expanded)))

(defn- read-syntax-quote
  [ctx]
  (next-char! ctx)
  (let [outer @(:gensym-env ctx)]
    (vreset! (:gensym-env ctx) {})
    (try
      (syntax-quote ctx (read-required ctx))
      (finally
        (vreset! (:gensym-env ctx) outer)))))

(defn- read-unquote
  [ctx]
  (if (= \@ (peek-char ctx 1))
    (read-wrapped ctx 'clojure.core/unquote-splicing 2)
    (read-wrapped ctx 'clojure.core/unquote 1)))

;; ----------------------------------------------------------------------------
;; #() and its % arguments
;; ----------------------------------------------------------------------------

(defn- arg-symbol
  "clojure.lang.LispReader's garg: p1__N#, rest__N#."
  [n]
  (symbol (str (if (= -1 n) "rest" (str "p" n)) "__" (names/next-id) "#")))

(defn- register-arg
  [ctx n]
  (or (get @(:arg-env ctx) n)
      (let [sym (arg-symbol n)]
        (vswap! (:arg-env ctx) assoc n sym)
        sym)))

(defn- read-arg
  "% inside #(): %, %N or %&. Outside #(), % begins an ordinary symbol."
  [ctx]
  (if (nil? @(:arg-env ctx))
    (read-symbolic ctx)
    (do (next-char! ctx)
        (if (token-end? (peek-char ctx))
          (register-arg ctx 1)
          (let [n (read-required ctx)]
            (cond
              (= '& n)   (register-arg ctx -1)
              (integer? n) (register-arg ctx n)
              :else      (reader-error ctx "arg literal must be %, %& or %integer")))))))

(defn- read-fn
  "#(...): the # is consumed; the cursor is on the (."
  [ctx]
  (when @(:arg-env ctx)
    (reader-error ctx "Nested #()s are not allowed"))
  (vreset! (:arg-env ctx) {})
  (try
    (let [body    (read-list ctx)
          env     @(:arg-env ctx)
          high    (reduce max 0 (keys env))
          params  (mapv #(or (get env %) (arg-symbol %)) (range 1 (inc high)))
          params  (if-let [rest-sym (get env -1)]
                    (conj params '& rest-sym)
                    params)]
      (list 'fn* params body))
    (finally
      (vreset! (:arg-env ctx) nil))))

;; ----------------------------------------------------------------------------
;; # dispatch
;; ----------------------------------------------------------------------------

(defn- read-namespaced-map
  "#:ns{...} and #::{...}; the #: is consumed."
  [ctx]
  (let [auto? (= \: (peek-char ctx))
        _     (when auto? (next-char! ctx))
        prefix (when-not (or (whitespace? (peek-char ctx)) (= \{ (peek-char ctx)))
                 (read-required ctx))
        _     (when-not (or prefix auto?)
                (reader-error ctx "Namespaced map must specify a namespace"))
        _     (skip-whitespace! ctx)
        _     (when-not (= \{ (peek-char ctx))
                (reader-error ctx "Namespaced map must specify a map"))
        valid? (and (symbol? prefix) (nil? (namespace prefix)))
        ns-str (cond
                 (and auto? (nil? prefix))
                 (str (ns-name *ns*))

                 (not valid?)
                 (reader-error ctx (str "Namespaced map must specify a valid namespace: "
                                        prefix))

                 auto?
                 (if-let [target (or (get (ns-aliases *ns*) prefix) (find-ns prefix))]
                   (str (ns-name target))
                   (reader-error ctx (str "Unknown auto-resolved namespace alias: " prefix)))

                 :else
                 (name prefix))
        start (pos ctx)
        _     (next-char! ctx)
        kvs   (read-delimited ctx \} start)
        qualify (fn [k]
                  (let [ctor (if (keyword? k) keyword symbol)]
                    (cond
                      (not (or (keyword? k) (symbol? k))) k
                      (nil? (namespace k))                (ctor ns-str (name k))
                      (= "_" (namespace k))               (ctor (name k))
                      :else                               k)))]
    (when (odd? (count kvs))
      (reader-error ctx "Namespaced map literal must contain an even number of forms"))
    (build-map ctx (map-indexed (fn [i x] (if (even? i) (qualify x) x)) kvs))))

(defn- read-tagged
  "#tag form, resolved through *data-readers* and default-data-readers."
  [ctx]
  (let [tag  (read-required ctx)
        _    (when-not (symbol? tag)
               (reader-error ctx "Reader tag must be a symbol"))
        form (read-required ctx)]
    (if-let [f (or (get *data-readers* tag) (get default-data-readers tag))]
      (f form)
      (if-let [default-fn *default-data-reader-fn*]
        (default-fn tag form)
        (reader-error ctx (str "No reader function for tag " tag))))))

(defn- read-dispatch
  [ctx]
  (let [start (pos ctx)]
    (next-char! ctx)
    (let [c (peek-char ctx)]
      (case c
        nil  (reader-error ctx "EOF while reading character")
        \{   (read-set ctx start)
        \(   (read-fn ctx)
        \"   (read-regex ctx)
        \'   (read-wrapped ctx 'var 1)
        \_   (do (next-char! ctx) (read-required ctx) skip)
        \!   (skip-line! ctx)
        \^   (let [caret (pos ctx)] (next-char! ctx) (read-meta ctx caret))
        \:   (do (next-char! ctx) (read-namespaced-map ctx))
        \#   (do (next-char! ctx)
                 (host-read ctx (str "##" (read-token-text ctx (pos ctx)))))
        \?   (reader-error ctx "Conditional read not allowed")
        \=   (reader-error ctx "#= (read-eval) is not supported in .cljel source")
        \<   (reader-error ctx "Unreadable form")
        (if (re-matches #"\p{L}" (str c))
          (read-tagged ctx)
          (reader-error ctx (str "No dispatch macro for: " c)))))))

;; ----------------------------------------------------------------------------
;; Dispatch on the first character
;; ----------------------------------------------------------------------------

(defn- read-form
  "Read the form starting at char c (not yet consumed). Returns skip for a
   comment or #_ form."
  [ctx c]
  (cond
    (digit? c)
    (read-number ctx)

    (and (contains? #{\+ \-} c) (digit? (peek-char ctx 1)))
    (read-number ctx)

    :else
    (case c
      \(  (read-list ctx)
      \[  (read-vector ctx)
      \{  (read-map ctx)
      \"  (read-string-literal ctx)
      \\  (read-character ctx)
      \;  (skip-line! ctx)
      \'  (read-wrapped ctx 'quote 1)
      \@  (read-wrapped ctx 'clojure.core/deref 1)
      \^  (let [start (pos ctx)] (next-char! ctx) (read-meta ctx start))
      \`  (read-syntax-quote ctx)
      \~  (read-unquote ctx)
      \%  (read-arg ctx)
      \#  (read-dispatch ctx)
      (\) \] \})
      (do (next-char! ctx)
          (reader-error ctx (str "Unmatched delimiter: " c)))
      (read-symbolic ctx))))

;; ============================================================================
;; Public API
;; ============================================================================

(defn- normalize-newlines
  "Line terminators become \\n, as clojure.lang.LineNumberingPushbackReader
   delivers them."
  [s]
  (-> s (str/replace "\r\n" "\n") (str/replace "\r" "\n")))

(defn read-forms
  "Read every form in source text s, in order. Lists carry {:line :column}.
   Throws ex-info on malformed input; its data has :line and :column, and
   :type ::invalid-number for a token that looks like a number but is not
   one (Elisp's 2+, for example)."
  [s]
  (let [ctx (make-ctx (normalize-newlines s))]
    (loop [forms (transient [])]
      (let [form (read-next ctx eof)]
        (if (identical? form eof)
          (persistent! forms)
          (recur (conj! forms form)))))))

(defn read-first
  "Read only the first form in source text s, or nil when there is none.
   Text after the first form is not read, so it may be malformed."
  [s]
  (read-next (make-ctx (normalize-newlines s)) nil))
