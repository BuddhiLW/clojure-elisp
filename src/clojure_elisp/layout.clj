(ns clojure-elisp.layout
  "Line breaking and indentation of emitted Emacs Lisp.

   `layout-code` lays out the text of a whole file form by form, as Emacs
   indents Lisp: a form that fits in `width` columns stays on one line. One
   that does not keeps its distinguished arguments (a defun's name and
   arglist, a let's bindings, an if's test) on its first line and indents its
   body by two, an if's then-branch by four; a call aligns its arguments
   under the first; a keyword and its value share a line. Defining forms
   always put their body under the first line.

   Every token is kept as written, strings (docstrings keep their lines) and
   comments included, so the text reads back as the same forms. Text whose
   parentheses do not balance is returned unchanged."
  (:require [clojure.string :as str]))

(def width
  "Column the layout keeps lines within, where tokens allow."
  80)

;; ============================================================================
;; Tokens
;; ============================================================================

(defn- char-at [s i]
  (when (< i (count s)) (nth s i)))

(defn- delimiter? [c]
  (or (nil? c)
      (contains? #{\space \tab \newline \return \formfeed \( \) \[ \] \" \;} c)))

(defn- string-end
  "Index just past the string literal that opens at i."
  [s i]
  (loop [j (inc i)]
    (let [c (char-at s j)]
      (cond (nil? c) j
            (= c \\) (recur (+ j 2))
            (= c \") (inc j)
            :else (recur (inc j))))))

(defn- atom-end
  "Index just past the symbol, number or character literal that starts at i."
  [s i]
  (let [start (if (= (char-at s i) \?)
                (if (= (char-at s (inc i)) \\) (+ i 3) (+ i 2))
                i)]
    (loop [j start]
      (let [c (char-at s j)]
        (cond (delimiter? c) j
              (= c \\) (recur (+ j 2))
              :else (recur (inc j)))))))

(defn- token-end
  "[type end] of the token that starts with c at i."
  [s i c]
  (let [nxt (char-at s (inc i))]
    (cond
      (contains? #{\( \[} c) [:open (inc i)]
      (contains? #{\) \]} c) [:close (inc i)]
      (= c \") [:string (string-end s i)]
      (= c \;) [:comment (or (str/index-of s "\n" i) (count s))]
      (contains? #{\' \`} c) [:prefix (inc i)]
      (= c \,) [:prefix (if (= nxt \@) (+ i 2) (inc i))]
      (and (= c \#) (contains? #{\' \( \[} nxt)) [:prefix (if (= nxt \') (+ i 2) (inc i))]
      (and (= c \#) (= nxt \s) (= (char-at s (+ i 2)) \()) [:prefix (+ i 2)]
      :else [:atom (atom-end s i)])))

(defn- tokenize
  "Tokens of Elisp text s: {:t type, :s text, :nl newlines before it}, types
   :open :close :prefix :string :comment :atom."
  [s]
  (let [n (count s)]
    (loop [i 0 nl 0 out []]
      (if (>= i n)
        out
        (let [c (nth s i)]
          (cond
            (= c \newline) (recur (inc i) (inc nl) out)
            (contains? #{\space \tab \return \formfeed} c) (recur (inc i) nl out)
            :else (let [[t j] (token-end s i c)]
                    (recur j 0 (conj out {:t t :s (subs s i j) :nl nl})))))))))

(defn- balanced?
  [tokens]
  (loop [ts (seq tokens) depth 0]
    (cond (neg? depth) false
          (empty? ts) (zero? depth)
          :else (recur (rest ts) (case (:t (first ts))
                                   :open (inc depth)
                                   :close (dec depth)
                                   depth)))))

;; ============================================================================
;; Tree
;; ============================================================================

(defn- parse-node
  "[node remaining-tokens] for the node that starts ts. A node is a leaf
   {:k :atom|:string|:comment :s}, {:k :prefixed :prefix :node} or
   {:k :list :open :close :items}; each carries the :nl of its first token."
  [ts]
  (let [{:keys [t s nl]} (first ts)]
    (case t
      :open (loop [ts (rest ts) items []]
              (if (= :close (:t (first ts)))
                [{:k :list :open s :close (:s (first ts)) :items items :nl nl} (rest ts)]
                (let [[x more] (parse-node ts)]
                  (recur more (conj items x)))))
      :prefix (let [[x more] (parse-node (rest ts))]
                [{:k :prefixed :prefix s :node x :nl nl} more])
      [{:k t :s s :nl nl} (rest ts)])))

(defn- parse [tokens]
  (loop [ts (seq tokens) out []]
    (if (empty? ts)
      out
      (let [[x more] (parse-node ts)]
        (recur (seq more) (conj out x))))))

;; ============================================================================
;; Layout
;; ============================================================================

(def ^:private distinguished
  "Arguments a form keeps on its first line, before a body indented by two:
   the `lisp-indent-function' of the Emacs forms emitted code uses."
  {"defun" 2 "defmacro" 2 "defsubst" 2 "cl-defun" 2 "cl-defmacro" 2
   "cl-defmethod" 2 "cl-defgeneric" 2 "define-minor-mode" 1
   "define-derived-mode" 3 "defvar" 2 "defconst" 2 "defcustom" 2
   "defgroup" 2 "defface" 2 "cl-defstruct" 1 "transient-define-prefix" 2
   "lambda" 1 "let" 1 "let*" 1 "when" 1 "unless" 1 "while" 1 "dolist" 1
   "dotimes" 1 "when-let*" 1 "and-let*" 1 "if" 2 "if-let*" 2
   "condition-case" 2 "unwind-protect" 1 "catch" 1 "cl-block" 1
   "progn" 0 "prog1" 1 "save-excursion" 0 "save-restriction" 0
   "save-match-data" 0 "with-temp-buffer" 0 "with-current-buffer" 1
   "with-help-window" 1 "with-output-to-string" 0 "with-eval-after-load" 1
   "eval-and-compile" 0 "eval-when-compile" 0 "cl-labels" 1 "cl-flet" 1
   "cl-letf" 1 "cl-letf*" 1 "pcase" 1 "pcase-let" 1 "pcase-let*" 1
   "pcase-dolist" 1 "cl-case" 1 "cl-ecase" 1 "cl-typecase" 1})

(def ^:private defining
  "Forms whose body always goes under their first line."
  #{"defun" "defmacro" "defsubst" "cl-defun" "cl-defmacro" "cl-defmethod"
    "cl-defgeneric" "define-minor-mode" "define-derived-mode" "defvar"
    "defconst" "defcustom" "defgroup" "defface" "cl-defstruct"
    "transient-define-prefix"})

(defn- keyword-atom? [n]
  (and (= :atom (:k n)) (str/starts-with? (:s n) ":") (> (count (:s n)) 1)))

(defn- head-symbol
  "The name of the symbol list n calls, or nil for a vector or data."
  [{:keys [k open items]}]
  (let [h (first items)]
    (when (and (= k :list) (= open "(") (= :atom (:k h)))
      (let [s (:s h)]
        (when-not (or (str/starts-with? s ":") (str/starts-with? s "?")
                      (re-matches #"[-+]?\.?[0-9].*" s))
          s)))))

(defn- flat
  "n on one line, or nil when it cannot be: it holds a comment or a string
   with a line break."
  [n]
  (case (:k n)
    :atom (:s n)
    :string (when-not (str/includes? (:s n) "\n") (:s n))
    :comment nil
    :prefixed (when-let [f (flat (:node n))] (str (:prefix n) f))
    :list (let [parts (mapv flat (:items n))]
            (when (every? some? parts)
              (str (:open n) (str/join " " parts) (:close n))))))

(defn- always-broken? [n]
  (let [hs (head-symbol n)]
    (and (contains? defining hs)
         (> (count (:items n)) (inc (get distinguished hs 0))))))

(defn- spaces [n]
  (apply str (repeat n " ")))

(defn- end-col
  "Column after text s, written from column col."
  [s col]
  (if-let [i (str/last-index-of s "\n")]
    (- (count s) i 1)
    (+ col (count s))))

(defn- group-pairs
  "items as lines of one item, or two where a keyword precedes its value."
  [items]
  (loop [xs (seq items) out []]
    (if (empty? xs)
      out
      (let [[a b] xs]
        (if (and b (keyword-atom? a) (not (keyword-atom? b)) (not= :comment (:k b)))
          (recur (nnext xs) (conj out [a b]))
          (recur (next xs) (conj out [a])))))))

(declare render)

(defn- render-line
  "items written on one line from column col; trail characters follow the
   last. A comment ends the line, and what follows it goes under it."
  [items col trail]
  (loop [xs (seq items) c col out []]
    (let [x     (first xs)
          last? (nil? (next xs))
          s     (render x c (if last? trail 0))
          out   (conj out s)]
      (cond last?                (apply str out)
            (= :comment (:k x))  (recur (next xs) col (conj out "\n" (spaces col)))
            :else                (recur (next xs) (inc (end-col s c)) (conj out " "))))))

(defn- lines
  "[items indent] lines a broken list n is written as; the first line's
   indent is nil, as it follows the opening bracket.

   A form with distinguished arguments keeps the first on its first line,
   and the others while they fit there flat; the rest go under it by four,
   its body by two. A call aligns its arguments under the first when that
   one fits on the first line and the others either fit under it on one
   line each or start within the first third of `width`; otherwise every
   argument goes on its own line one column in."
  [{:keys [items] :as n} col]
  (let [hs         (head-symbol n)
        head       (first items)
        c1         (inc col)
        on         (fn [indent groups] (map (fn [g] [g indent]) groups))
        flat-width (fn [xs] (when-let [fs (seq (map flat xs))]
                              (when (every? some? fs)
                                (count (str/join " " fs)))))]
    (cond
      (nil? hs)
      (let [[g & gs] (group-pairs items)]
        (cons [g nil] (on c1 gs)))

      (= hs "cond")
      (cons [[head] nil] (on c1 (map vector (rest items))))

      (contains? #{"if" "if-let*"} hs)
      (let [[_ test then & more] items]
        (concat [[(remove nil? [head test]) nil]]
                (when then [[[then] (+ col 4)]])
                (on (+ col 2) (map vector more))))

      (contains? distinguished hs)
      (let [k        (get distinguished hs)
            dist     (take k (rest items))
            body     (drop (inc k) items)
            first-ln (vec (remove nil? [head (first dist)]))
            [on-head below]
            (loop [xs (rest dist) line first-ln]
              (let [w (when (seq xs) (flat-width (conj line (first xs))))]
                (if (and w (<= (+ c1 w 1) width))
                  (recur (rest xs) (conj line (first xs)))
                  [line xs])))]
        (concat [[on-head nil]]
                (on (+ col 4) (map vector below))
                (on (+ col 2) (group-pairs body))))

      :else
      (let [[g & gs] (group-pairs (rest items))
            align    (+ col 2 (count hs))
            fits?    (fn [grp] (when-let [w (flat-width grp)]
                                 (<= (+ align w 1) width)))]
        (if (and g (fits? g) (or (<= align (quot width 3)) (every? fits? gs)))
          (cons [(cons head g) nil] (on align gs))
          (cons [[head] nil] (on c1 (remove nil? (cons g gs)))))))))

(defn- render-broken [{:keys [open close items] :as n} col trail]
  (let [ls            (vec (lines n col))
        last-i        (dec (count ls))
        last-comment? (= :comment (:k (last items)))
        out           (reduce-kv
                       (fn [out i [xs indent]]
                         (let [t (if (and (= i last-i) (not last-comment?)) (inc trail) 0)]
                           (if (nil? indent)
                             (conj out (render-line xs (inc col) t))
                             (conj out "\n" (spaces indent) (render-line xs indent t)))))
                       [open]
                       ls)]
    (apply str (concat out
                       (when last-comment? ["\n" (spaces (inc col))])
                       [close]))))

(defn- render
  "n laid out from column col, followed on its last line by trail more
   characters (closing brackets)."
  [n col trail]
  (let [f (flat n)]
    (if (and f (<= (+ col (count f) trail) width) (not (always-broken? n)))
      f
      (case (:k n)
        :prefixed (let [p (:prefix n)]
                    (str p (render (:node n) (+ col (count p)) trail)))
        :list (if (empty? (:items n)) f (render-broken n col trail))
        (:s n)))))

(defn layout-code
  "Elisp text s laid out form by form (see the namespace doc). Comments and
   blank lines between top-level forms are kept; unbalanced text is
   returned as it is."
  [s]
  (let [tokens (tokenize s)]
    (if-not (balanced? tokens)
      s
      (let [nodes (parse tokens)
            body  (loop [ns (seq nodes) first? true out []]
                    (if (empty? ns)
                      out
                      (let [n   (first ns)
                            sep (cond first?          ""
                                      (zero? (:nl n)) " "
                                      :else           (apply str (repeat (:nl n) "\n")))]
                        (recur (next ns) false (conj out sep (render n 0 0))))))]
        (str (apply str body)
             (when (str/ends-with? s "\n") "\n"))))))
