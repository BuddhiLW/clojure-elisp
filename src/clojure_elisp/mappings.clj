(ns clojure-elisp.mappings
  "Clojure→Elisp function name mappings.

   Maps Clojure core functions and namespaced functions to their
   Elisp equivalents. Organized by category for maintainability.")

;; ============================================================================
;; Arithmetic
;; ============================================================================

(def arithmetic-mappings
  {'+ "+"
   '- "-"
   '* "*"
   '/ "/"
   'mod "mod"
   'rem "%"
   'inc "1+"
   'dec "1-"})

;; ============================================================================
;; Comparison
;; ============================================================================

(def comparison-mappings
  {'= "equal"
   '== "="
   'not= "/="
   '< "<"
   '> ">"
   '<= "<="
   '>= ">="})

;; ============================================================================
;; Logic
;; ============================================================================

(def logic-mappings
  {;; NOTE: 'and and 'or are special forms (for short-circuit evaluation),
   ;; handled in analyzer.clj and emitted via emit-node :and/:or
   'not "not"})

;; ============================================================================
;; Type Predicates
;; ============================================================================

(def type-predicate-mappings
  {'nil? "null"
   'string? "stringp"
   'number? "numberp"
   'symbol? "symbolp"
   'list? "listp"
   'vector? "vectorp"
   'map? "hash-table-p"
   'fn? "functionp"
   'keyword? "keywordp"})

;; ============================================================================
;; Numeric Predicates
;; ============================================================================

(def numeric-predicate-mappings
  {'zero? "zerop"
   'pos? "cl-plusp"
   'neg? "cl-minusp"
   'even? "cl-evenp"
   'odd? "cl-oddp"})

;; ============================================================================
;; Collection Predicates
;; ============================================================================

(def collection-predicate-mappings
  {'coll? "clel-coll-p"
   'sequential? "clel-sequential-p"
   'associative? "clel-associative-p"
   ;; Boolean/nil predicates
   'some? "clel-some-p"
   'true? "clel-true-p"
   'false? "clel-false-p"})

;; ============================================================================
;; Collections
;; ============================================================================

(def collection-mappings
  {'first "clel-first"
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
   'peek "clel-peek"
   'pop "clel-pop"
   'subvec "clel-subvec"})

;; ============================================================================
;; Sequence Functions
;; ============================================================================

(def sequence-mappings
  {;; Lazy
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
   'interpose "clel-interpose"
   'partition "clel-partition"
   'partition-all "clel-partition-all"
   'partition-by "clel-partition-by"
   'distinct "clel-distinct"
   'dedupe "clel-dedupe"
   'split-at "clel-split-at"
   'split-with "clel-split-with"
   ;; Eager
   'reduce "clel-reduce"
   'sort "clel-sort"
   'sort-by "clel-sort-by"
   'group-by "clel-group-by"
   'frequencies "clel-frequencies"
   ;; Additional lazy
   'cycle "clel-cycle"
   'iterate "clel-iterate"
   'reductions "clel-reductions"
   'take-nth "clel-take-nth"
   'take-last "clel-take-last"
   'drop-last "clel-drop-last"
   ;; Generators
   'range "clel-range"
   'repeat "clel-repeat"
   'repeatedly "clel-repeatedly"
   ;; Predicates
   'every? "clel-every-p"
   'some "clel-some"
   'not-every? "clel-not-every-p"
   'not-any? "clel-not-any-p"
   ;; Lazy realization
   'realized? "clel-realized-p"
   'doall "clel-doall"
   'dorun "clel-dorun"})

;; ============================================================================
;; Transducers
;; ============================================================================

(def transducer-mappings
  {'transduce "clel-transduce"
   'eduction "clel-eduction"
   'keep "clel-keep"
   'keep-indexed "clel-keep-indexed"
   'cat "clel-cat-xf"
   'reduced "clel-reduced"
   'reduced? "clel-reduced-p"
   'unreduced "clel-unreduced"})

;; ============================================================================
;; Strings
;; ============================================================================

(def string-mappings
  {;; Core
   'str "clel-str"
   'subs "substring"
   'format "format"
   'pr-str "prin1-to-string"
   'println "message"
   'print "princ"
   ;; clojure.string namespace
   'clojure.string/join "clel-str-join"
   'clojure.string/split "clel-str-split"
   'clojure.string/replace "clel-str-replace"
   'clojure.string/replace-first "clel-str-replace-first"
   'clojure.string/re-quote-replacement "regexp-quote"
   'clojure.string/trim "clel-str-trim"
   'clojure.string/triml "clel-str-triml"
   'clojure.string/trimr "clel-str-trimr"
   'clojure.string/lower-case "clel-str-lower"
   'clojure.string/upper-case "clel-str-upper"
   'clojure.string/capitalize "clel-str-capitalize"
   'clojure.string/blank? "clel-str-blank-p"
   'clojure.string/includes? "clel-str-includes-p"
   'clojure.string/starts-with? "clel-str-starts-with-p"
   'clojure.string/ends-with? "clel-str-ends-with-p"
   'clojure.string/reverse "clel-str-reverse"
   'clojure.string/index-of "clel-str-index-of"
   'clojure.string/last-index-of "clel-str-last-index-of"
   'clojure.string/split-lines "clel-str-split-lines"
   ;; Regex string functions
   're-matches "clel-str-re-matches"
   're-find "clel-str-re-find"
   're-seq "clel-str-re-seq"})

;; ============================================================================
;; Sets
;; ============================================================================

(def set-mappings
  {'set "clel-set-from-coll"
   'hash-set "clel-set"
   'disj "clel-set-remove"
   'set? "clel-set-p"
   ;; clojure.set namespace
   'clojure.set/union "clel-set-union"
   'clojure.set/intersection "clel-set-intersection"
   'clojure.set/difference "clel-set-difference"
   'clojure.set/subset? "clel-set-subset-p"
   'clojure.set/superset? "clel-set-superset-p"
   'clojure.set/select "clel-set-select"
   'clojure.set/project "clel-set-project"
   'clojure.set/rename "clel-set-rename"
   'clojure.set/rename-keys "clel-rename-keys"
   'clojure.set/join "clel-set-join"
   'clojure.set/index "clel-set-index"
   'clojure.set/map-invert "clel-map-invert"})

;; ============================================================================
;; Math
;; ============================================================================

(def math-mappings
  {'min "cl-min"
   'max "cl-max"
   'abs "abs"
   'quot "truncate"
   'rand "clel-rand"
   'rand-int "clel-rand-int"
   'rand-nth "clel-rand-nth"})

;; ============================================================================
;; Functions & Symbols
;; ============================================================================

(def function-mappings
  {'apply "apply"
   'identity "identity"
   'constantly "clel-constantly"
   'partial "apply-partially"
   'comp "clel-comp"
   'name "symbol-name"
   'juxt "clel-juxt"
   'complement "clel-complement"})

;; ============================================================================
;; Atoms
;; ============================================================================

(def atom-mappings
  {'atom "clel-atom"
   'deref "clel-deref"
   'clojure.core/deref "clel-deref"  ; @a reader macro expands to clojure.core/deref
   'reset! "clel-reset!"
   'swap! "clel-swap!"
   'add-watch "clel-add-watch"
   'remove-watch "clel-remove-watch"})

;; ============================================================================
;; Emacs Buffer Operations
;; ============================================================================

(def emacs-buffer-mappings
  {'message "message"
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
   'buffer-narrowed-p "buffer-narrowed-p"})

;; ============================================================================
;; Emacs Text Properties
;; ============================================================================

(def emacs-text-prop-mappings
  {'put-text-property "put-text-property"
   'get-text-property "get-text-property"
   'remove-text-properties "remove-text-properties"
   'propertize "propertize"})

;; ============================================================================
;; Emacs Process Operations
;; ============================================================================

(def emacs-process-mappings
  {'start-process "start-process"
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
   'process-list "process-list"})

;; ============================================================================
;; Emacs File Operations
;; ============================================================================

(def emacs-file-mappings
  {'find-file "find-file"
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
   'file-name-nondirectory "file-name-nondirectory"})

;; ============================================================================
;; Emacs Window Operations
;; ============================================================================

(def emacs-window-mappings
  {'selected-window "selected-window"
   'select-window "select-window"
   'window-buffer "window-buffer"
   'set-window-buffer "set-window-buffer"
   'split-window "split-window"
   'delete-window "delete-window"
   'other-window "other-window"})

;; ============================================================================
;; Utility Functions
;; ============================================================================

(def utility-mappings
  {'zipmap "clel-zipmap"
   'select-keys "clel-select-keys"})

;; ============================================================================
;; I/O Functions
;; ============================================================================

(def io-mappings
  {'slurp "clel-slurp"
   'spit "clel-spit"
   'read-string "clel-read-string"})

;; ============================================================================
;; Elisp Built-in Passthrough (special chars that mangle-name would break)
;; ============================================================================

(def elisp-passthrough-mappings
  "Elisp functions whose names contain characters that mangle-name
   would incorrectly transform (=, <, >, *, +).
   These need explicit mapping to preserve the original elisp name."
  {;; String comparison (= → -eq would be wrong)
   'string= "string="
   'string< "string<"
   'string> "string>"
   'char= "char="
   ;; Note: 1+ and 1- are valid elisp but invalid Clojure symbols.
   ;; Use (inc x) and (dec x) instead — already mapped to "1+" and "1-".
   ;; Common elisp predicates with special chars
   'string-equal "string-equal"  ;; already safe, but explicit for discoverability
   'string-lessp "string-lessp"
   'string-greaterp "string-greaterp"})

;; ============================================================================
;; Merged Mapping
;; ============================================================================

(def core-fn-mapping
  "Merged map of all Clojure→Elisp function translations."
  (merge arithmetic-mappings
         comparison-mappings
         logic-mappings
         type-predicate-mappings
         numeric-predicate-mappings
         collection-predicate-mappings
         collection-mappings
         sequence-mappings
         transducer-mappings
         string-mappings
         set-mappings
         math-mappings
         function-mappings
         atom-mappings
         emacs-buffer-mappings
         emacs-text-prop-mappings
         emacs-process-mappings
         emacs-file-mappings
         emacs-window-mappings
         utility-mappings
         io-mappings
         elisp-passthrough-mappings))
