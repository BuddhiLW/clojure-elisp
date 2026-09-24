;;; clel.el --- Runtime library for ClojureElisp  -*- lexical-binding: t; -*-

;; Copyright (C) 2025-2026 Pedro G. Branquinho

;; Author: Pedro G. Branquinho <pedrogbranquinho@gmail.com>
;; Maintainer: Pedro G. Branquinho <pedrogbranquinho@gmail.com>
;; URL: https://github.com/BuddhiLW/clojure-elisp
;; Version: 0.8.0
;; Package-Requires: ((emacs "28.1"))
;; Keywords: languages, lisp, clojure
;; SPDX-License-Identifier: MIT

;; This file is not part of GNU Emacs.

;;; Commentary:

;; The runtime library of ClojureElisp, a compiler from Clojure syntax
;; (.cljel files) to Emacs Lisp.  Compiled code calls these functions for
;; what Clojure has and Emacs Lisp lacks: Clojure's collection functions
;; over lists, vectors, alists and hash tables, lazy sequences, atoms,
;; transducers, protocols and multimethods, and the clojure.string and
;; clojure.set functions.
;;
;; You do not call it directly.  A package compiled with ClojureElisp
;; lists clel in its Package-Requires, and each of its files requires
;; clel and refuses to load when `clel-runtime-version' is older than
;; the compiler that produced it expects.
;;
;; This file is generated from runtime.cljel in the ClojureElisp
;; repository: edit that, not this file.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)

(defconst clel-runtime-version "0.8.0"
  "Version of the ClojureElisp runtime library.
Compiled files check this to refuse a runtime older than the one they
were emitted against.")

(defun clel-vector (&rest items)
  "Create a vector from ITEMS."
  (clel-apply #'vector items))

(defun clel-hash-map (&rest kvs)
  "Create a hash-table from key-value pairs KVS."
  (let* ((ht (make-hash-table :test 'equal)) (rest kvs))
    (while rest (puthash (car rest) (cadr rest) ht) (setq rest (cddr rest)))
    ht))

(defvar clel--entries (make-hash-table :test 'eq :weakness 'key)
  "Map entries the runtime made, as keys.")

(defun clel--entry (k v)
  "Return a new map entry (K . V)."
  (let* ((e (cons k v))) (puthash e t clel--entries) e))

(defun clel-map-entry-p (x)
  "Return t if X is a map entry: one the runtime made, or a dotted pair."
  (and (consp x) (or (gethash x clel--entries) (not (listp (cdr x)))) t))

(defun clel--alist-p (x)
  "Return t if X is a non-empty list whose first element is a map entry."
  (and (consp x) (clel-map-entry-p (car x))))

(defun clel-map-p (x)
  "Return t if X is a map, as Clojure `map?'.
A map is a hash table that is not a set, or an alist of entries.  The
empty map is nil, which is not a map."
  (cond
   ((hash-table-p x) (not (clel-set-p x)))
   ((consp x) (clel--alist-p x))
   (t nil)))

(defun clel-vector-p (x)
  "Return t if X is a vector, as Clojure `vector?'.
A vector is an Elisp vector, or a non-empty list that is neither a map
nor another runtime value (a lazy seq, an atom, a reduced value)."
  (cond
   ((vectorp x) t)
   ((consp x)
    (not
     (or (clel--alist-p x)
         (memq (car x) '(clel-lazy-seq clel-atom clel-reduced clel-eduction)))))
   (t nil)))

(defun clel--kv-entry (item)
  "Return ITEM, a map entry or a two-item vector [K V], as an entry."
  (cond
   ((clel-map-entry-p item) (clel--entry (car item) (cdr item)))
   ((consp item) (clel--entry (car item) (clel-second item)))
   ((vectorp item) (clel--entry (aref item 0) (aref item 1)))
   (t (error "Not a map entry: %S" item))))

(defun clel--assoc-key (alist key val)
  "Return ALIST with KEY bound to VAL.
An existing key keeps its position; a new one is appended. Untouched
entries are shared, not copied."
  (let* ((found nil)
         (result
          (mapcar
           (lambda (e)
             (if (and (not found) (consp e) (equal (car e) key))
                 (progn (setq found t) (clel--entry key val))
               e))
           alist)))
    (if found result (append result (list (clel--entry key val))))))

(defun clel--assoc-index (coll idx val)
  "Return the list COLL with index IDX set to VAL.
This is Clojure assoc on a vector: IDX may be one past the end, which
appends."
  (let* ((len (length coll)))
    (cond
     ((and (>= idx 0) (< idx len))
      (let* ((new (copy-sequence coll))) (setcar (nthcdr idx new) val) new))
     ((= idx len) (append coll (list val)))
     (t (error "Index %d out of bounds for clel-assoc (length %d)" idx len)))))

(defun clel-array-map (&rest kvs)
  "Return the map of key-value pairs KVS; a later duplicate key wins."
  (let* ((result nil) (rest kvs))
    (while rest
      (setq result (clel--assoc-key result (car rest) (cadr rest)))
      (setq rest (cddr rest)))
    result))

(defun clel-conj (coll item)
  "Add ITEM to collection COLL, returning new collection.
A map takes a [K V] pair or an entry, as `assoc'."
  (let* ((coll (clel-realize coll)))
    (cond
     ((null coll) (list item))
     ((clel--alist-p coll)
      (let* ((e (clel--kv-entry item))) (clel--assoc-key coll (car e) (cdr e))))
     ((listp coll) (append coll (list item)))
     ((vectorp coll) (vconcat coll (vector item)))
     ((hash-table-p coll)
      (let* ((new (copy-hash-table coll)) (e (clel--kv-entry item)))
        (puthash (car e) (cdr e) new)
        new))
     (t (error "Unsupported collection type for clel-conj: %s"
               (type-of coll))))))

(cl-defun clel-get (coll key &optional default)
  "Get KEY from COLL, returning DEFAULT only when KEY is ABSENT.
A present nil or false is returned as itself: `or' against the default
would overwrite it, which is how destructuring :or used to lose a
deliberately falsy value. An integer KEY indexes a list that is not a
map, as Clojure `get' on a vector; on a map it is a key like any other."
  (cond
   ((null coll) default)
   ((listp coll)
    (if (and (numberp key) (not (clel--alist-p coll)))
        (if (and (integerp key) (>= key 0))
            (let* ((cell (nthcdr key coll)))
              (if (consp cell) (car cell) default))
          default)
      (let* ((pair (assoc key coll))) (if pair (cdr pair) default))))
   ((vectorp coll)
    (if (and (integerp key) (>= key 0) (< key (length coll)))
        (aref coll key)
      default))
   ((hash-table-p coll) (gethash key coll default))
   (t default)))

(cl-defun clel-assoc (coll key val &rest kvs)
  "Return COLL with KEY associated to VAL, and each key-value pair in KVS.
A list that is not a map takes an integer KEY as an index, as Clojure
assoc on a vector."
  (let* ((result
          (cond
           ((null coll) (list (clel--entry key val)))
           ((listp coll)
            (if (and (integerp key) (not (clel--alist-p coll)))
                (clel--assoc-index coll key val)
              (clel--assoc-key coll key val)))
           ((vectorp coll)
            (let* ((new (copy-sequence coll))) (aset new key val) new))
           ((hash-table-p coll)
            (let* ((new (copy-hash-table coll))) (puthash key val new) new))
           (t
            (error "Unsupported collection type for clel-assoc: %s"
                   (type-of coll))))))
    (if kvs (clel-apply #'clel-assoc result kvs) result)))

(defun clel-dissoc (coll &rest ks)
  "Remove each of KS from COLL, returning a new collection.
Works with alists and hash-tables."
  (cond
   ((null coll) nil)
   ((listp coll)
    (cl-remove-if (lambda (e) (and (consp e) (member (car e) ks))) coll))
   ((hash-table-p coll)
    (let* ((new (copy-hash-table coll))) (dolist (k ks) (remhash k new)) new))
   (t (error "Unsupported collection type for clel-dissoc: %s"
             (type-of coll)))))

(cl-defun clel-get-in (m ks &optional not-found)
  "Get nested value from M following keys KS.
Returns NOT-FOUND (default nil) if path does not exist."
  (let* ((result m) (keys ks))
    (setq keys (if (vectorp keys) (append keys nil) (clel-realize keys)))
    (while (and keys result)
      (setq result (clel-get result (car keys)))
      (setq keys (cdr keys)))
    (if (null result) (or not-found nil) result)))

(defun clel-assoc-in (m ks v)
  "Associate value V at nested path KS in M.
Creates intermediate maps as needed."
  (let* ((keys ks))
    (setq keys (if (vectorp keys) (append keys nil) (clel-realize keys)))
    (if (null keys)
        m
      (if (= 1 (clel-count keys))
          (clel-assoc m (car keys) v)
        (clel-assoc m
                    (car keys)
                    (clel-assoc-in (clel-get m (car keys)) (cdr keys) v))))))

(defun clel-update (m k f &rest args)
  "Update value at K in M by applying F to old value and ARGS."
  (clel-assoc m k (clel-apply (clel--fn f) (clel-get m k) args)))

(defun clel-update-in (m ks f &rest args)
  "Update value at nested path KS in M by applying F to old value and ARGS."
  (let* ((keys ks))
    (setq keys (if (vectorp keys) (append keys nil) (clel-realize keys)))
    (if (null keys)
        m
      (if (= 1 (clel-count keys))
          (clel-apply #'clel-update m (car keys) f args)
        (clel-assoc m
                    (car keys)
                    (clel-apply #'clel-update-in
                                (clel-get m (car keys))
                                (cdr keys)
                                f
                                args))))))

(defun clel-merge (&rest maps)
  "Merge MAPS left to right.
Later values override earlier. Returns an alist or a hash-table,
depending on the first map."
  (if (null maps)
      nil
    (let* ((first-map (car maps))
           (result
            (cond
             ((null first-map) nil)
             ((hash-table-p first-map) (copy-hash-table first-map))
             ((listp first-map) first-map)
             (t
              (error "Unsupported map type for clel-merge: %s"
                     (type-of first-map))))))
      (dolist (m (cdr maps))
        (when m
          (cond
           ((hash-table-p result)
            (cond
             ((hash-table-p m) (maphash (lambda (k v) (puthash k v result)) m))
             ((listp m)
              (dolist (pair m) (puthash (car pair) (cdr pair) result)))))
           ((listp result)
            (cond
             ((hash-table-p m)
              (maphash
               (lambda (k v) (setq result (clel--assoc-key result k v)))
               m))
             ((listp m)
              (dolist (pair m)
                (setq result
                      (clel--assoc-key result (car pair) (cdr pair))))))))))
      result)))

(defun clel-last (coll)
  "Return the last element of COLL.
Unlike Elisp `last' which returns a cons cell, this returns the element itself."
  (let* ((coll (clel-realize coll)))
    (cond
     ((null coll) nil)
     ((listp coll) (car (last coll)))
     ((vectorp coll)
      (if (> (clel-count coll) 0) (aref coll (1- (clel-count coll))) nil))
     (t nil))))

(cl-defun clel-nth (coll n &optional (not-found nil not-found-p))
  "Clojure-style nth: the N-th element of COLL, coll FIRST and 0-indexed.
Elisp `nth' is (nth N LIST) — index first — so a bare mapping reversed the
args. With NOT-FOUND supplied, return it for an out-of-range index instead
of signalling, matching clojure.core/nth's 3-arity."
  (if (or (vectorp coll) (stringp coll))
      (if (and (integerp n) (>= n 0) (< n (length coll)))
          (aref coll n)
        (if not-found-p
            not-found
          (error "Index %s out of bounds for clel-nth (length %d)"
                 n
                 (length coll))))
    (let* ((s (clel-nthnext coll n)))
      (if (and (integerp n) (>= n 0) (consp s))
          (car s)
        (if not-found-p
            not-found
          (error "Index %s out of bounds for clel-nth" n))))))

(defun clel-nthnext (coll n)
  "Clojure `nthnext': the items of COLL after the first N, or nil.
Walks a lazy seq, a vector or a map entry one cell at a time, which
Elisp `nthcdr' cannot."
  (let* ((s (clel-seq-force (clel--entry-seq coll))) (i n))
    (while (and s (> i 0)) (setq s (clel-rest s)) (setq i (1- i)))
    (if (clel-lazy-seq-p s) (clel-seq-force s) s)))

(defun clel-nthrest (coll n)
  "Clojure `nthrest': the items of COLL after the first N, as a list."
  (clel-nthnext coll n))

(defun clel-contains-p (coll key)
  "Return t if KEY exists in COLL.
For maps/alists, checks if key is present.
For sets (represented as lists), checks if element is present.
For vectors, checks if index is valid."
  (let* ((coll (clel-realize coll)))
    (cond
     ((null coll) nil)
     ((hash-table-p coll)
      (let* ((not-found (gensym)))
        (not (eq (gethash key coll not-found) not-found))))
     ((listp coll)
      (if (clel--alist-p coll)
          (if (assoc key coll) t nil)
        (if (member key coll) t nil)))
     ((vectorp coll) (and (integerp key) (>= key 0) (< key (clel-count coll))))
     (t nil))))

(defun clel-keys (coll)
  "Return keys of COLL as a list."
  (let* ((coll (clel-realize coll)))
    (cond
     ((null coll) nil)
     ((listp coll) (mapcar #'car coll))
     ((hash-table-p coll) (hash-table-keys coll))
     (t nil))))

(defun clel-vals (coll)
  "Return values of COLL as a list."
  (let* ((coll (clel-realize coll)))
    (cond
     ((null coll) nil)
     ((listp coll) (mapcar #'cdr coll))
     ((hash-table-p coll) (hash-table-values coll))
     (t nil))))

(defun clel-seq (coll)
  "Return COLL as a sequence (list), or nil if empty.
A lazy sequence is realized, so the result is always a plain list."
  (let* ((coll (clel-realize coll)))
    (cond
     ((null coll) nil)
     ((listp coll) (if coll coll nil))
     ((vectorp coll) (if (= 0 (clel-count coll)) nil (append coll nil)))
     ((hash-table-p coll)
      (let* ((pairs nil))
        (maphash (lambda (k v) (push (clel--entry k v) pairs)) coll)
        (nreverse pairs)))
     (t nil))))

(defun clel--into-map (to from)
  "Put every item of FROM into the map TO (nil is the empty map).
An item is a map entry or a two-item vector [K V]."
  (let* ((result to))
    (dolist (item (clel-seq from))
      (let* ((e (clel--kv-entry item)))
        (setq result
              (if (hash-table-p result)
                  (clel-assoc result (car e) (cdr e))
                (clel--assoc-key result (car e) (cdr e))))))
    result))

(cl-defun clel-into (to from &optional (coll nil coll-p))
  "Add all items of FROM into the collection TO.
With COLL, FROM is a transducer applied to the items of COLL.  A map
target takes entries or [K V] pairs."
  (if coll-p
      (clel-into-xform to from coll)
    (let* ((from (clel-realize from)))
      (cond
       ((and (hash-table-p to) (clel-set-p to))
        (let* ((new (copy-hash-table to)))
          (dolist (item (clel-seq from)) (puthash item t new))
          new))
       ((or (clel--alist-p to) (hash-table-p to)) (clel--into-map to from))
       ((vectorp to)
        (vconcat to
                 (if (vectorp from)
                     from
                   (clel-apply #'vector (clel-seq from)))))
       ((listp to) (append to (clel-seq from)))
       (t
        (error "Unsupported target collection type for clel-into: %s"
               (type-of to)))))))

(defun clel-coll-p (x)
  "Return t if X is a collection (list, vector, or hash-table)."
  (or (listp x) (vectorp x) (hash-table-p x)))

(defun clel-sequential-p (x)
  "Return t if X is sequential (list or vector)."
  (or (listp x) (vectorp x)))

(defun clel-associative-p (x)
  "Return t if X is associative (list or hash-table)."
  (or (listp x) (hash-table-p x)))

(defun clel--map-equal (a b)
  "Return t if maps A and B hold the same keys with equal values."
  (let* ((ea (clel-seq a)) (eb (clel-seq b)))
    (and (= (length ea) (length eb))
         (cl-every
          (lambda (e)
            (let* ((pair (assoc (car e) eb)))
              (and pair (clel--equal2 (cdr e) (cdr pair)))))
          ea)
         t)))

(defun clel--set-equal (a b)
  "Return t if the hash set A holds exactly the items of B, a set or a list."
  (let* ((items
          (if (hash-table-p b)
              (hash-table-keys b)
            (delete-dups (copy-sequence b)))))
    (and (= (hash-table-count a) (length items))
         (cl-every (lambda (x) (gethash x a)) items)
         t)))

(defun clel--seq-equal (a b)
  "Return t if the proper lists A and B are equal element by element."
  (let* ((x a) (y b) (ok t))
    (while (and ok (consp x) (consp y))
      (setq ok (clel--equal2 (car x) (car y)))
      (setq x (cdr x))
      (setq y (cdr y)))
    (and ok (null x) (null y))))

(defun clel--equal2 (a b)
  "Return t if A and B are equal, as Clojure `='."
  (cond
   ((eq a b) t)
   ((or (clel-lazy-seq-p a) (clel-lazy-seq-p b))
    (clel--equal2 (clel-realize a) (clel-realize b)))
   ((and (hash-table-p a) (clel-set-p a))
    (and (or (listp b) (clel-set-p b)) (clel--set-equal a b)))
   ((and (hash-table-p b) (clel-set-p b)) (and (listp a) (clel--set-equal b a)))
   ((or (hash-table-p a) (hash-table-p b) (clel--alist-p a) (clel--alist-p b))
    (and (clel-map-p a) (clel-map-p b) (clel--map-equal a b)))
   ((or (clel-map-entry-p a) (clel-map-entry-p b))
    (clel--seq-equal (clel--entry-seq a) (clel--entry-seq b)))
   ((and (consp a) (consp b)) (clel--seq-equal a b))
   ((and (vectorp a) (or (vectorp b) (consp b)))
    (clel--seq-equal (append a nil) (append b nil)))
   ((and (consp a) (vectorp b)) (clel--seq-equal a (append b nil)))
   (t (equal a b))))

(cl-defun clel-equal (a &rest more)
  "Clojure `=': t when A and every value of MORE are equal.
Maps are equal when they hold the same entries in any order."
  (let* ((ok t) (prev a))
    (dolist (b more) (when ok (setq ok (clel--equal2 prev b)) (setq prev b)))
    ok))

(cl-defun clel-not-equal (a &rest more)
  "Return t unless A and every value of MORE are equal, as Clojure `not='."
  (not (clel-apply #'clel-equal a more)))

(defun clel-some-p (x)
  "Return t if X is not nil."
  (if x t nil))

(defun clel-true-p (x)
  "Return t if X is exactly t."
  (eq x t))

(defun clel-false-p (x)
  "Return t if X is exactly nil."
  (null x))

(defun clel-str (&rest args)
  "Concatenate ARGS as strings."
  (mapconcat
   (lambda (x)
     (cond
      ((stringp x) x)
      ((null x) "")
      ((symbolp x) (symbol-name x))
      (t (format "%s" x))))
   args
   ""))

(cl-defun clel-subs (s start &optional end)
  "Extract substring from S starting at START to END (optional)."
  (if (null s) "" (substring s start end)))

(defun clel-str-join (sep coll)
  "Join elements of COLL as strings, separated by SEP."
  (if (null coll)
      ""
    (let* ((strings (mapcar #'clel-str (clel-realize coll))))
      (string-join strings sep))))

(defun clel-str-split (s re)
  "Split S by regex RE."
  (if (null s) nil (split-string s re)))

(defun clel-str-replace (s match replacement)
  "Replace all occurrences of MATCH in S with REPLACEMENT.
MATCH is treated as a literal string."
  (if (null s)
      ""
    (replace-regexp-in-string (regexp-quote match) replacement s)))

(defun clel-str-trim (s)
  "Trim whitespace from both ends of S."
  (if (null s) "" (string-trim s)))

(defun clel-str-lower (s)
  "Convert S to lowercase."
  (if (null s) "" (downcase s)))

(defun clel-str-upper (s)
  "Convert S to uppercase."
  (if (null s) "" (upcase s)))

(defun clel-str-capitalize (s)
  "Capitalize S (uppercase first char, lowercase rest)."
  (if (or (null s) (string-empty-p s))
      ""
    (concat (upcase (substring s 0 1)) (downcase (substring s 1)))))

(defun clel-str-triml (s)
  "Trim whitespace from left (start) of S."
  (if (null s) "" (string-trim-left s)))

(defun clel-str-trimr (s)
  "Trim whitespace from right (end) of S."
  (if (null s) "" (string-trim-right s)))

(defun clel-str-blank-p (s)
  "Return t if S is nil, empty, or contains only whitespace."
  (or (null s) (string-empty-p s) (string-match-p "\\`[[:space:]]*\\'" s)))

(defun clel-str-includes-p (s substr)
  "Return t if S contains SUBSTR."
  (if (or (null s) (null substr))
      nil
    (if (string-match-p (regexp-quote substr) s) t nil)))

(defun clel-str-starts-with-p (s prefix)
  "Return t if S starts with PREFIX."
  (if (or (null s) (null prefix)) nil (string-prefix-p prefix s)))

(defun clel-str-ends-with-p (s suffix)
  "Return t if S ends with SUFFIX."
  (if (or (null s) (null suffix)) nil (string-suffix-p suffix s)))

(defun clel-str-reverse (s)
  "Reverse string S."
  (if (null s) "" (concat (nreverse (string-to-list s)))))

(defun clel-str-replace-first (s match replacement)
  "Replace first occurrence of MATCH in S with REPLACEMENT."
  (if (null s)
      ""
    (replace-regexp-in-string (regexp-quote match) replacement s nil nil 1)))

(defun clel-str-re-replace (s pattern replacement)
  "Replace all matches of regex PATTERN in S with REPLACEMENT."
  (if (null s) "" (replace-regexp-in-string pattern replacement s)))

(defun clel-str-re-replace-first (s pattern replacement)
  "Replace first match of regex PATTERN in S with REPLACEMENT."
  (if (null s) "" (replace-regexp-in-string pattern replacement s nil nil 1)))

(defun clel-str-re-matches (re s)
  "Return match data if RE matches entire string S, else nil."
  (if (or (null re) (null s))
      nil
    (when (string-match-p (concat "\\`" re "\\'") s)
      (string-match re s)
      (match-string 0 s))))

(defun clel-str-re-find (re s)
  "Return first match of RE in S, or nil."
  (if (or (null re) (null s))
      nil
    (when (string-match re s) (match-string 0 s))))

(defun clel-str-re-seq (re s)
  "Return list of all matches of RE in S."
  (if (or (null re) (null s))
      nil
    (let* ((matches nil) (start 0))
      (while (string-match re s start)
        (push (match-string 0 s) matches)
        (setq start (match-end 0)))
      (nreverse matches))))

(cl-defun clel-str-index-of (s substr &optional from-index)
  "Return index of first occurrence of SUBSTR in S, or nil.
Optional FROM-INDEX specifies starting position."
  (if (or (null s) (null substr))
      nil
    (let* ((pos (string-match (regexp-quote substr) s (or from-index 0))))
      pos)))

(cl-defun clel-str-last-index-of (s substr &optional from-index)
  "Return index of last occurrence of SUBSTR in S, or nil."
  (if (or (null s) (null substr))
      nil
    (let* ((len (clel-count s))
           (sublen (clel-count substr))
           (limit (or from-index len))
           (result nil))
      (cl-dotimes (i (min (1+ limit) (- len sublen -1)))
                  (when (and (<= (+ i sublen) len)
                             (string= substr (substring s i (+ i sublen))))
                    (setq result i)))
      result)))

(defun clel-constantly (x)
  "Return a function that always returns X."
  (lambda (&rest _) x))

(defun clel-comp (&rest fns)
  "Compose functions FNS right-to-left."
  (let* ((fns (mapcar #'clel--fn fns)))
    (lambda (x)
      (seq-reduce (lambda (v f) (funcall f v)) (clel-reverse fns) x))))

(defun clel--fn (f)
  "Return F as a function, the way Clojure invokes it.
A keyword looks itself up in its map argument, a map looks up its key
argument, and a set literal (a list) answers the member it contains."
  (cond
   ((functionp f) f)
   ((keywordp f) (lambda (m &rest default) (clel-get m f (car default))))
   ((hash-table-p f) (lambda (k &rest default) (clel-get f k (car default))))
   ((and (consp f) (consp (car f)))
    (lambda (k &rest default) (clel-get f k (car default))))
   ((consp f) (lambda (x) (car (member x f))))
   (t f)))

(defun clel-atom (val)
  "Create an atom with initial value VAL."
  (list 'clel-atom val nil))

(defun clel-deref (atom)
  "Get the value of ATOM."
  (nth 1 atom))

(defun clel--notify-watchers (atom old-val new-val)
  "Call all watchers on ATOM with OLD-VAL and NEW-VAL."
  (let* ((watchers (nth 2 atom)))
    (dolist (watcher watchers)
      (let* ((key (car watcher)) (f (cdr watcher)))
        (funcall f key atom old-val new-val)))))

(defun clel-reset-bang (atom val)
  "Reset ATOM to VAL, calling watchers."
  (let* ((old-val (nth 1 atom)))
    (setcar (nthcdr 1 atom) val)
    (clel--notify-watchers atom old-val val)
    val))

(defalias 'clel-reset! #'clel-reset-bang)

(defun clel-swap-bang (atom f &rest args)
  "Swap ATOM by applying F to current value and ARGS, calling watchers."
  (let* ((old-val (clel-deref atom))
         (new-val (clel-apply (clel--fn f) old-val args)))
    (setcar (nthcdr 1 atom) new-val)
    (clel--notify-watchers atom old-val new-val)
    new-val))

(defalias 'clel-swap! #'clel-swap-bang)

(defun clel-add-watch (atom key f)
  "Add watcher F to ATOM under KEY.
F will be called with (key atom old-val new-val) when atom changes.
Returns ATOM."
  (let* ((watchers (nth 2 atom)))
    (setq watchers (cl-remove-if (lambda (w) (equal (car w) key)) watchers))
    (setcar (nthcdr 2 atom) (cons (cons key f) watchers)))
  atom)

(defun clel-remove-watch (atom key)
  "Remove watcher with KEY from ATOM.
Returns ATOM."
  (let* ((watchers (nth 2 atom)))
    (setcar (nthcdr 2 atom)
            (cl-remove-if (lambda (w) (equal (car w) key)) watchers)))
  atom)

(defun clel-lazy-seq-create (thunk)
  "Create a lazy sequence from THUNK."
  (list 'clel-lazy-seq thunk nil nil))

(defun clel-lazy-seq-p (x)
  "Return t if X is a lazy sequence."
  (and (consp x) (eq (car x) 'clel-lazy-seq)))

(defun clel-lazy-seq-force (lseq)
  "Force lazy sequence LSEQ, memoizing the result."
  (if (nth 3 lseq)
      (nth 2 lseq)
    (let* ((result (funcall (nth 1 lseq))))
      (setcar (nthcdr 2 lseq) result)
      (setcar (nthcdr 3 lseq) t)
      result)))

(defun clel-realized-p (x)
  "Return t if X is realized (not a pending lazy seq)."
  (if (clel-lazy-seq-p x) (nth 3 x) t))

(defun clel-doall (seq)
  "Force entire lazy SEQ, returning it."
  (let* ((s seq))
    (while (clel-lazy-seq-p s) (setq s (clel-lazy-seq-force s)))
    (when (listp s)
      (let* ((current s))
        (while current
          (when (clel-lazy-seq-p (car current))
            (setcar current (clel-doall (car current))))
          (when (and (consp current) (clel-lazy-seq-p (cdr current)))
            (setcdr current (clel-doall (cdr current))))
          (setq current (cdr-safe current)))))
    s))

(defun clel-dorun (seq)
  "Force entire lazy SEQ for side effects, returning nil."
  (clel-doall seq)
  nil)

(defun clel-first (s)
  "Return the first element of S, forcing lazy seqs."
  (cond
   ((null s) nil)
   ((clel-lazy-seq-p s) (clel-first (clel-lazy-seq-force s)))
   ((listp s) (car s))
   ((vectorp s) (if (> (clel-count s) 0) (aref s 0) nil))
   (t nil)))

(defun clel-rest (s)
  "Return the rest of S (possibly empty list), forcing lazy seqs.
The tail is forced one cell deep, so a `while' walking with `clel-rest'
terminates on the real end of the sequence rather than on a pending thunk."
  (cond
   ((null s) nil)
   ((clel-lazy-seq-p s) (clel-rest (clel-lazy-seq-force s)))
   ((clel-map-entry-p s) (list (cdr s)))
   ((listp s)
    (let* ((tail (cdr s)))
      (if (clel-lazy-seq-p tail) (clel-seq-force tail) tail)))
   ((vectorp s) (if (> (clel-count s) 1) (cdr (append s nil)) nil))
   (t nil)))

(defun clel-next (s)
  "Return the next of S, or nil if empty. Forces lazy seqs."
  (let* ((r (clel-rest s))) (if r r nil)))

(defun clel-seq-force (s)
  "Ensure S is a realized sequence (list). Forces lazy seqs."
  (cond
   ((null s) nil)
   ((clel-lazy-seq-p s) (clel-seq-force (clel-lazy-seq-force s)))
   ((listp s) s)
   ((vectorp s) (append s nil))
   (t (list s))))

(defun clel-lazy-spine-p (s)
  "Return t if any cell in the spine of S is an unrealized lazy sequence."
  (cond
   ((clel-lazy-seq-p s) t)
   ((consp s)
    (let* ((cur s) (found nil))
      (while (and (consp cur) (not found))
        (if (clel-lazy-seq-p (cdr cur)) (setq found t) (setq cur (cdr cur))))
      found))
   (t nil)))

(defun clel--entry-seq (x)
  "Return X as a sequence: a map entry (K . V) is the two items (K V).
Anything else is returned unchanged."
  (if (clel-map-entry-p x) (list (car x) (cdr x)) x))

(defun clel-realize (s)
  "Return S with every lazy cell of its spine forced into a plain list.
Values with no lazy spine are returned unchanged, vectors included, and a
map entry becomes its two items.  Call it wherever a sequence is about to
reach a raw Elisp primitive such as `length', `apply', `sort' or
`reverse', which cannot force."
  (let* ((s (clel--entry-seq s)))
    (if (clel-lazy-spine-p s)
        (let* ((acc nil) (cur (clel-seq-force s)))
          (while cur (push (clel-first cur) acc) (setq cur (clel-rest cur)))
          (nreverse acc))
      s)))

(defun clel-count (coll)
  "Return the number of items in COLL."
  (let* ((coll (clel-realize coll)))
    (if (hash-table-p coll) (hash-table-count coll) (length coll))))

(defun clel-second (coll)
  "Return the second element of COLL."
  (clel-first (clel-rest coll)))

(defun clel-butlast (coll)
  "Return COLL without its last element."
  (butlast (clel-realize coll)))

(defun clel-reverse (coll)
  "Return COLL in reverse order."
  (reverse (clel-realize coll)))

(defun clel-flatten (coll)
  "Return a flat list of every leaf in the nested collection COLL."
  (flatten-tree (clel-realize coll)))

(defun clel-remove (pred coll)
  "Lazily return the items of COLL for which PRED is false."
  (let* ((pred (clel--fn pred)))
    (clel-filter (lambda (x) (not (funcall pred x))) coll)))

(defun clel-apply (f &rest args)
  "Apply F to ARGS, whose final element is a sequence of trailing arguments."
  (let* ((f (clel--fn f)))
    (if (null args)
        (funcall f)
      (let* ((leading (butlast args)) (trailing (clel-seq (car (last args)))))
        (apply f (append leading trailing))))))

(defun clel-map (f &rest colls)
  "Lazily map F over COLLS. With one coll, returns lazy seq."
  (let* ((f (clel--fn f)))
    (if (= 1 (clel-count colls))
        (let* ((s (clel-seq-force (car colls))))
          (clel-lazy-seq-create
           (lambda ()
             (when s
               (cons (funcall f (clel-first s)) (clel-map f (clel-rest s)))))))
      (let* ((seqs (mapcar #'clel-seq-force colls)))
        (clel-lazy-seq-create
         (lambda ()
           (when (cl-every #'identity seqs)
             (cons (clel-apply f (mapcar #'clel-first seqs))
                   (clel-apply #'clel-map f (mapcar #'clel-rest seqs))))))))))

(defun clel-filter (pred s)
  "Lazily filter S by PRED."
  (let* ((s (clel-seq-force s)) (pred (clel--fn pred)))
    (clel-lazy-seq-create
     (lambda ()
       (let* ((cur s))
         (while (and cur (not (funcall pred (clel-first cur))))
           (setq cur (clel-rest cur)))
         (when cur
           (cons (clel-first cur) (clel-filter pred (clel-rest cur)))))))))

(defun clel-take (n s)
  "Lazily take N elements from S."
  (clel-lazy-seq-create
   (lambda ()
     (when (and (> n 0) s)
       (let* ((forced (clel-seq-force s)))
         (when forced
           (cons (clel-first forced)
                 (clel-take (1- n) (clel-rest forced)))))))))

(defun clel-drop (n s)
  "Drop N elements from S, return rest lazily."
  (clel-lazy-seq-create
   (lambda ()
     (let* ((cur (clel-seq-force s)) (remaining n))
       (while (and (> remaining 0) cur)
         (setq cur (clel-rest cur))
         (setq remaining (1- remaining)))
       cur))))

(defun clel-take-while (pred s)
  "Lazily take elements from S while PRED is true."
  (clel-lazy-seq-create
   (lambda ()
     (let* ((forced (clel-seq-force s)) (pred (clel--fn pred)))
       (when (and forced (funcall pred (clel-first forced)))
         (cons (clel-first forced)
               (clel-take-while pred (clel-rest forced))))))))

(defun clel-drop-while (pred s)
  "Drop elements from S while PRED is true, return rest lazily."
  (clel-lazy-seq-create
   (lambda ()
     (let* ((cur (clel-seq-force s)) (pred (clel--fn pred)))
       (while (and cur (funcall pred (clel-first cur)))
         (setq cur (clel-rest cur)))
       cur))))

(defun clel-concat (&rest colls)
  "Lazily concatenate COLLS."
  (if (null colls)
      nil
    (let* ((first-coll (clel-seq-force (car colls))) (rest-colls (cdr colls)))
      (clel-lazy-seq-create
       (lambda ()
         (if first-coll
             (cons (clel-first first-coll)
                   (clel-apply #'clel-concat
                               (cons (clel-rest first-coll) rest-colls)))
           (when rest-colls
             (clel-seq-force (clel-apply #'clel-concat rest-colls)))))))))

(defun clel-mapcat (f &rest colls)
  "Map F over COLLS and concatenate results lazily."
  (clel-apply #'clel-concat (clel-doall (clel-apply #'clel-map f colls))))

(defun clel-interleave (&rest colls)
  "Lazily interleave COLLS."
  (let* ((seqs (mapcar #'clel-seq-force colls)))
    (clel-lazy-seq-create
     (lambda ()
       (when (cl-every #'identity seqs)
         (let* ((firsts (mapcar #'clel-first seqs))
                (rests (mapcar #'clel-rest seqs)))
           (append firsts
                   (clel-seq-force (clel-apply #'clel-interleave rests)))))))))

(defun clel-partition (n s)
  "Partition S into groups of N elements. Returns lazy seq of lists."
  (clel-lazy-seq-create
   (lambda ()
     (let* ((forced (clel-seq-force s)))
       (when forced
         (let* ((group nil) (cur forced) (count 0))
           (while (and cur (< count n))
             (push (clel-first cur) group)
             (setq cur (clel-rest cur))
             (setq count (1+ count)))
           (when (= count n)
             (cons (nreverse group) (clel-partition n cur)))))))))

(defun clel-partition-by (f s)
  "Partition S into groups by the value of (F elem).
Each group contains consecutive elements with the same (F elem) value."
  (clel-lazy-seq-create
   (lambda ()
     (let* ((forced (clel-seq-force s)) (f (clel--fn f)))
       (when forced
         (let* ((first-elem (clel-first forced))
                (first-val (funcall f first-elem))
                (group (list first-elem))
                (cur (clel-rest forced)))
           (while (and cur (clel-equal (funcall f (clel-first cur)) first-val))
             (push (clel-first cur) group)
             (setq cur (clel-rest cur)))
           (cons (nreverse group) (clel-partition-by f cur))))))))

(defun clel-split-at (n s)
  "Split S at position N. Returns list of (take n s) and (drop n s)."
  (list (clel-doall (clel-take n s)) (clel-doall (clel-drop n s))))

(defun clel-split-with (pred s)
  "Split S at first element where PRED is false.
Returns list of (take-while pred s) and (drop-while pred s)."
  (list (clel-doall (clel-take-while pred s))
        (clel-doall (clel-drop-while pred s))))

(defun clel-reduce (f &rest args)
  "Reduce the collection in ARGS with F, as Clojure's reduce does.
ARGS is COLL, reduced from its first element, or INIT and COLL."
  (let* ((init nil) (s nil))
    (if (= 1 (clel-count args))
        (let* ((coll (clel-seq-force (car args))))
          (setq init (clel-first coll))
          (setq s (clel-rest coll)))
      (progn (setq init (car args)) (setq s (clel-seq-force (cadr args)))))
    (let* ((acc init) (cur s))
      (while cur
        (setq acc (funcall f acc (clel-first cur)))
        (setq cur (clel-rest cur)))
      acc)))

(defun clel-compare (x y)
  "Compare X and Y the way `clojure.core/compare' does: -1, 0 or 1.
nil sorts first; numbers, strings, keywords and symbols compare by value;
lists compare by length, then element by element."
  (cond
   ((equal x y) 0)
   ((null x) -1)
   ((null y) 1)
   ((and (numberp x) (numberp y)) (cond ((< x y) -1) ((> x y) 1) (t 0)))
   ((and (stringp x) (stringp y)) (if (string< x y) -1 1))
   ((and (symbolp x) (symbolp y))
    (let* ((a (symbol-name x)) (b (symbol-name y)))
      (cond ((string< a b) -1) ((string= a b) 0) (t 1))))
   ((and (clel-sequential-p x) (clel-sequential-p y))
    (let* ((a (clel-seq x)) (b (clel-seq y)) (la (length a)) (lb (length b)))
      (if (not (= la lb))
          (if (< la lb) -1 1)
        (let* ((result 0))
          (while (and a (= 0 result))
            (setq result (clel-compare (car a) (car b)))
            (setq a (cdr a) b (cdr b)))
          result))))
   (t (error "Cannot compare %S with %S" x y))))

(defun clel--sort-pred (cmp)
  "Return an Elisp sort predicate for the Clojure comparator CMP.
A nil CMP is `clel-compare'. A comparator may answer a boolean (x before
y) or a number (negative when x is before y), as in Clojure."
  (if (null cmp)
      (lambda (a b) (< (clel-compare a b) 0))
    (let* ((cmp (clel--fn cmp)))
      (lambda (a b)
        (let* ((r (funcall cmp a b))) (if (numberp r) (< r 0) r))))))

(cl-defun clel-sort (a &optional (b nil b-p))
  "Return a new sorted list, as Clojure `sort'.
With A alone, sort the collection A.  With B, A is the comparator and B
the collection.  The sort is stable."
  (let* ((cmp (if b-p a nil))
         (coll (if b-p b a))
         (lst (copy-sequence (clel-seq coll))))
    (sort lst (clel--sort-pred cmp))))

(cl-defun clel-sort-by (keyfn a &optional (b nil b-p))
  "Return a new list sorted by KEYFN, as Clojure `sort-by'.
With A alone, sort the collection A.  With B, A is the comparator and B
the collection.  KEYFN may be a keyword.  The sort is stable."
  (let* ((keyfn (clel--fn keyfn))
         (pred (clel--sort-pred (if b-p a nil)))
         (coll (if b-p b a))
         (lst (copy-sequence (clel-seq coll))))
    (sort lst
          (lambda (x y) (funcall pred (funcall keyfn x) (funcall keyfn y))))))

(defun clel-group-by (f coll)
  "Group elements of COLL by the result of F. Returns alist."
  (let* ((result nil) (f (clel--fn f)) (cur (clel-seq-force coll)))
    (while cur
      (let* ((item (clel-first cur))
             (key (funcall f item))
             (existing (assoc key result)))
        (if existing
            (setcdr existing (append (cdr existing) (list item)))
          (push (clel--entry key (list item)) result)))
      (setq cur (clel-rest cur)))
    (nreverse result)))

(defun clel-frequencies (coll)
  "Return alist of (element . count) for elements in COLL."
  (let* ((result nil) (cur (clel-seq-force coll)))
    (while cur
      (let* ((item (clel-first cur)) (existing (assoc item result)))
        (if existing
            (setcdr existing (1+ (cdr existing)))
          (push (clel--entry item 1) result)))
      (setq cur (clel-rest cur)))
    (nreverse result)))

(defun clel-every-p (pred coll)
  "Return t if PRED is true for every element in COLL."
  (let* ((cur (clel-seq-force coll)) (pred (clel--fn pred)) (result t))
    (while (and cur result)
      (unless (funcall pred (clel-first cur)) (setq result nil))
      (setq cur (clel-rest cur)))
    result))

(defun clel-some (pred coll)
  "Return the first truthy value of (PRED item) for items in COLL, or nil."
  (let* ((cur (clel-seq-force coll)) (pred (clel--fn pred)) (result nil))
    (while (and cur (not result))
      (setq result (funcall pred (clel-first cur)))
      (setq cur (clel-rest cur)))
    result))

(defun clel-not-every-p (pred coll)
  "Return t if PRED is not true for every element in COLL."
  (not (clel-every-p pred coll)))

(defun clel-not-any-p (pred coll)
  "Return t if PRED is not true for any element in COLL."
  (not (clel-some pred coll)))

(defun clel-empty-p (coll)
  "Return t if COLL is empty or nil.
A lazy seq is realized as far as its first item; a string, a vector and
a hash table are empty when they have no elements."
  (cond
   ((stringp coll) (= 0 (length coll)))
   ((vectorp coll) (= 0 (length coll)))
   ((hash-table-p coll) (= 0 (hash-table-count coll)))
   (t (null (clel-seq-force coll)))))

(defun clel-not-empty (coll)
  "Return COLL, or nil when it is empty."
  (if (clel-empty-p coll) nil coll))

(defun clel-range (&rest args)
  "Return a range of numbers; ARGS is END, START END, or START END STEP.
The range runs from START (default 0) by STEP (default 1) up to but not
including END.  With no ARGS, return the empty list: an infinite range
is not supported."
  (let* ((start 0) (end nil) (step 1))
    (pcase (clel-count args)
      (0 nil)
      (1 (setq end (car args)))
      (2 (setq start (car args) end (cadr args)))
      (_ (setq start (car args) end (cadr args) step (caddr args))))
    (when end
      (let* ((result nil) (i start))
        (if (> step 0)
            (while (< i end) (push i result) (setq i (+ i step)))
          (when (< step 0)
            (while (> i end) (push i result) (setq i (+ i step)))))
        (nreverse result)))))

(defun clel-repeat (n x)
  "Return a list of N copies of X."
  (let* ((result nil)) (cl-dotimes (_ n) (push x result)) result))

(defun clel-repeatedly (n f)
  "Call F N times with no arguments, returning a list of results."
  (let* ((result nil))
    (cl-dotimes (_ n) (push (funcall f) result))
    (nreverse result)))

(defun clel-set (&rest items)
  "Create a set from ITEMS.
Returns a hash-table where each item is a key with value t."
  (let* ((s (make-hash-table :test 'equal)))
    (dolist (item items) (puthash item t s))
    s))

(defun clel-set-from-coll (coll)
  "Create a set from collection COLL."
  (let* ((s (make-hash-table :test 'equal)))
    (dolist (item (clel-realize coll)) (puthash item t s))
    s))

(defun clel-set-p (x)
  "Return t if X is a set (hash-table with all values t)."
  (and (hash-table-p x)
       (let* ((is-set t))
         (maphash (lambda (_k v) (unless (eq v t) (setq is-set nil))) x)
         is-set)))

(defun clel-set-contains-p (s item)
  "Return t if set S contains ITEM."
  (if (hash-table-p s) (gethash item s nil) (if (member item s) t nil)))

(defun clel-set-add (s item)
  "Add ITEM to set S, returning new set."
  (let* ((new (copy-hash-table s))) (puthash item t new) new))

(defun clel-set-remove (s item)
  "Remove ITEM from set S, returning new set."
  (let* ((new (copy-hash-table s))) (remhash item new) new))

(defun clel-set-union (&rest sets)
  "Return the union of SETS."
  (let* ((result (make-hash-table :test 'equal)))
    (dolist (s sets)
      (if (hash-table-p s)
          (maphash (lambda (k _v) (puthash k t result)) s)
        (dolist (item (clel-seq-force s)) (puthash item t result))))
    result))

(defun clel-set-intersection (&rest sets)
  "Return the intersection of SETS."
  (if (null sets)
      (make-hash-table :test 'equal)
    (let* ((first-set (car sets))
           (rest-sets (cdr sets))
           (result (make-hash-table :test 'equal)))
      (if (hash-table-p first-set)
          (maphash
           (lambda (k _v)
             (when (cl-every
                    (lambda (s)
                      (if (hash-table-p s) (gethash k s) (member k s)))
                    rest-sets)
               (puthash k t result)))
           first-set)
        (dolist (item (clel-seq-force first-set))
          (when (cl-every
                 (lambda (s)
                   (if (hash-table-p s) (gethash item s) (member item s)))
                 rest-sets)
            (puthash item t result))))
      result)))

(defun clel-set-difference (s1 &rest sets)
  "Return items in S1 not in any of SETS."
  (let* ((result (make-hash-table :test 'equal)))
    (if (hash-table-p s1)
        (maphash
         (lambda (k _v)
           (unless (cl-some
                    (lambda (s)
                      (if (hash-table-p s) (gethash k s) (member k s)))
                    sets)
             (puthash k t result)))
         s1)
      (dolist (item (clel-seq-force s1))
        (unless (cl-some
                 (lambda (s)
                   (if (hash-table-p s) (gethash item s) (member item s)))
                 sets)
          (puthash item t result))))
    result))

(defun clel-set-subset-p (s1 s2)
  "Return t if S1 is a subset of S2."
  (let* ((result t))
    (if (hash-table-p s1)
        (maphash
         (lambda (k _v)
           (unless (if (hash-table-p s2) (gethash k s2) (member k s2))
             (setq result nil)))
         s1)
      (dolist (item (clel-seq-force s1))
        (unless (if (hash-table-p s2) (gethash item s2) (member item s2))
          (setq result nil))))
    result))

(defun clel-set-superset-p (s1 s2)
  "Return t if S1 is a superset of S2."
  (clel-set-subset-p s2 s1))

(defun clel-set-select (pred s)
  "Return a set of items in S for which PRED returns true."
  (let* ((result (make-hash-table :test 'equal)) (pred (clel--fn pred)))
    (if (hash-table-p s)
        (maphash (lambda (k _v) (when (funcall pred k) (puthash k t result))) s)
      (dolist (item (clel-seq-force s))
        (when (funcall pred item) (puthash item t result))))
    result))

(defun clel-set-project (xrel ks)
  "Project a relation XREL (set of maps) onto the keys in KS."
  (let* ((result (make-hash-table :test 'equal)) (key-list (clel-realize ks)))
    (if (hash-table-p xrel)
        (maphash
         (lambda (m _v)
           (let* ((projected nil))
             (dolist (k key-list)
               (let* ((val (clel-get m k)))
                 (when val (push (clel--entry k val) projected))))
             (puthash (nreverse projected) t result)))
         xrel)
      (dolist (m (clel-seq-force xrel))
        (let* ((projected nil))
          (dolist (k key-list)
            (let* ((val (clel-get m k)))
              (when val (push (clel--entry k val) projected))))
          (puthash (nreverse projected) t result))))
    result))

(defun clel-set-rename (xrel kmap)
  "Rename keys in relation XREL according to KMAP (old-key . new-key) pairs."
  (let* ((result (make-hash-table :test 'equal))
         (rename-map
          (if (hash-table-p kmap)
              kmap
            (let* ((ht (make-hash-table :test 'equal)))
              (dolist (pair kmap) (puthash (car pair) (cdr pair) ht))
              ht))))
    (if (hash-table-p xrel)
        (maphash
         (lambda (m _v)
           (let* ((renamed nil))
             (cond
              ((hash-table-p m)
               (maphash
                (lambda (k val)
                  (let* ((new-key (or (gethash k rename-map) k)))
                    (push (clel--entry new-key val) renamed)))
                m))
              ((listp m)
               (dolist (pair m)
                 (let* ((new-key
                         (or (gethash (car pair) rename-map) (car pair))))
                   (push (clel--entry new-key (cdr pair)) renamed)))))
             (puthash (nreverse renamed) t result)))
         xrel)
      (dolist (m (clel-seq-force xrel))
        (let* ((renamed nil))
          (cond
           ((hash-table-p m)
            (maphash
             (lambda (k val)
               (let* ((new-key (or (gethash k rename-map) k)))
                 (push (clel--entry new-key val) renamed)))
             m))
           ((listp m)
            (dolist (pair m)
              (let* ((new-key (or (gethash (car pair) rename-map) (car pair))))
                (push (clel--entry new-key (cdr pair)) renamed)))))
          (puthash (nreverse renamed) t result))))
    result))

(defun clel-rename-keys (m kmap)
  "Rename keys in map M according to KMAP (old-key . new-key) pairs."
  (let* ((rename-map
          (if (hash-table-p kmap)
              kmap
            (let* ((ht (make-hash-table :test 'equal)))
              (dolist (pair kmap) (puthash (car pair) (cdr pair) ht))
              ht)))
         (result nil))
    (cond
     ((hash-table-p m)
      (let* ((new-ht (make-hash-table :test 'equal)))
        (maphash
         (lambda (k v)
           (let* ((new-key (or (gethash k rename-map) k)))
             (puthash new-key v new-ht)))
         m)
        new-ht))
     ((listp m)
      (progn
        (dolist (pair m)
          (let* ((new-key (or (gethash (car pair) rename-map) (car pair))))
            (push (clel--entry new-key (cdr pair)) result)))
        (nreverse result)))
     (t m))))

(cl-defun clel-set-join (xrel yrel &optional km)
  "Natural join of relations XREL and YREL.
If KM is provided, it maps keys from XREL to keys in YREL."
  (let* ((result (make-hash-table :test 'equal))
         (x-list
          (if (hash-table-p xrel)
              (let* ((items nil))
                (maphash (lambda (k _v) (push k items)) xrel)
                items)
            (clel-seq-force xrel)))
         (y-list
          (if (hash-table-p yrel)
              (let* ((items nil))
                (maphash (lambda (k _v) (push k items)) yrel)
                items)
            (clel-seq-force yrel))))
    (dolist (xm x-list)
      (dolist (ym y-list)
        (let* ((xm-keys (clel-keys xm)) (ym-keys (clel-keys ym)) (match t))
          (let* ((common-keys
                  (if km
                      (let* ((mapped nil))
                        (dolist (k xm-keys)
                          (let* ((yk (clel-get km k)))
                            (when (and yk (member yk ym-keys))
                              (push k mapped))))
                        mapped)
                    (cl-remove-if-not (lambda (k) (member k ym-keys))
                                      xm-keys))))
            (dolist (xk common-keys)
              (let* ((yk (if km (clel-get km xk) xk)))
                (unless (clel-equal (clel-get xm xk) (clel-get ym yk))
                  (setq match nil))))
            (when match
              (let* ((merged (clel-merge xm ym)))
                (puthash merged t result)))))))
    result))

(defun clel-set-index (xrel ks)
  "Index relation XREL on keys KS.
Returns a map from key-values to sets of matching maps."
  (let* ((result nil)
         (key-list (clel-realize ks))
         (x-list
          (if (hash-table-p xrel)
              (let* ((items nil))
                (maphash (lambda (k _v) (push k items)) xrel)
                items)
            (clel-realize xrel))))
    (dolist (m x-list)
      (let* ((key-vals nil))
        (dolist (k key-list) (push (clel--entry k (clel-get m k)) key-vals))
        (setq key-vals (nreverse key-vals))
        (let* ((existing (clel-get result key-vals)))
          (if existing
              (puthash m t existing)
            (let* ((new-set (make-hash-table :test 'equal)))
              (puthash m t new-set)
              (setq result (clel-assoc result key-vals new-set)))))))
    result))

(defun clel-map-invert (m)
  "Invert map M, swapping keys and values.
Values must be unique, or later entries will overwrite earlier ones."
  (cond
   ((hash-table-p m)
    (let* ((result (make-hash-table :test 'equal)))
      (maphash (lambda (k v) (puthash v k result)) m)
      result))
   ((listp m)
    (let* ((result nil))
      (dolist (pair m) (push (clel--entry (cdr pair) (car pair)) result))
      (nreverse result)))
   (t nil)))

(defun clel--reducing-fn-init (rf)
  "Call RF with 0 arguments for init value."
  (condition-case nil (funcall rf) (error nil)))

(defun clel--reducing-fn-complete (rf result)
  "Call RF with RESULT alone, its completion arity.
Return RESULT unchanged when RF has no such arity."
  (condition-case nil (funcall rf result) (error result)))

(defun clel-reduced (val)
  "Wrap VAL to signal early termination in reduce."
  (list 'clel-reduced val))

(defun clel-reduced-p (x)
  "Return t if X is a reduced value."
  (and (consp x) (eq (car x) 'clel-reduced)))

(defun clel-deref-reduced (x)
  "Unwrap a reduced value, or return X if not reduced."
  (if (clel-reduced-p x) (cadr x) x))

(defun clel-ensure-reduced (x)
  "Ensure X is reduced. If already reduced, return as-is."
  (if (clel-reduced-p x) x (clel-reduced x)))

(defun clel-unreduced (x)
  "Unwrap reduced value if reduced, else return X."
  (if (clel-reduced-p x) (cadr x) x))

(defun clel-transduce (xform f &rest args)
  "Transduce a collection with transducer XFORM and reducing function F.
ARGS is COLL, reduced from the init value F returns when called with no
arguments, or INIT and COLL."
  (let* ((init nil) (coll nil))
    (if (= 1 (clel-count args))
        (progn
          (setq coll (clel-seq-force (car args)))
          (setq init (clel--reducing-fn-init f)))
      (progn (setq init (car args)) (setq coll (clel-seq-force (cadr args)))))
    (let* ((xf (funcall xform f)) (result init) (cur coll))
      (while (and cur (not (clel-reduced-p result)))
        (setq result (funcall xf result (clel-first cur)))
        (setq cur (clel-rest cur)))
      (clel--reducing-fn-complete xf (clel-unreduced result)))))

(defun clel-into-xform (to xform from)
  "Add all items FROM into TO, transformed by XFORM."
  (let* ((rf
          (cond
           ((vectorp to)
            (lambda (&rest args)
              (pcase (clel-count args)
                (0 (vector))
                (1 (car args))
                (2 (vconcat (car args) (vector (cadr args)))))))
           ((listp to)
            (lambda (&rest args)
              (pcase (clel-count args)
                (0 nil)
                (1 (nreverse (car args)))
                (2 (cons (cadr args) (car args))))))
           ((hash-table-p to)
            (lambda (&rest args)
              (pcase (clel-count args)
                (0 (make-hash-table :test 'equal))
                (1 (car args))
                (2
                 (let* ((ht (car args)) (pair (cadr args)))
                   (puthash (car pair) (cdr pair) ht)
                   ht))))))))
    (let* ((result (clel-transduce xform rf to from)))
      (cond
       ((vectorp to) result)
       ((listp to) result)
       ((hash-table-p to) result)
       (t result)))))

(defun clel-sequence-xform (xform coll)
  "Apply transducer XFORM to COLL, returning a lazy sequence."
  (clel-transduce xform
                  (lambda (&rest args)
                    (pcase (clel-count args)
                      (0 nil)
                      (1 (nreverse (car args)))
                      (2 (cons (cadr args) (car args)))))
                  nil
                  coll))

(defun clel-eduction (xform coll)
  "Return a reducible/iterable application of XFORM to COLL."
  (list 'clel-eduction xform coll))

(defun clel-eduction-p (x)
  "Return t if X is an eduction."
  (and (consp x) (eq (car x) 'clel-eduction)))

(defun clel-map-xf (f)
  "Return a mapping transducer that applies F to each element."
  (let* ((f (clel--fn f)))
    (lambda (rf)
      (lambda (&rest args)
        (pcase (clel-count args)
          (0 (funcall rf))
          (1 (funcall rf (car args)))
          (2 (funcall rf (car args) (funcall f (cadr args)))))))))

(defun clel-filter-xf (pred)
  "Return a filtering transducer that keeps elements where PRED is true."
  (let* ((pred (clel--fn pred)))
    (lambda (rf)
      (lambda (&rest args)
        (pcase (clel-count args)
          (0 (funcall rf))
          (1 (funcall rf (car args)))
          (2
           (if (funcall pred (cadr args))
               (funcall rf (car args) (cadr args))
             (car args))))))))

(defun clel-remove-xf (pred)
  "Return a transducer that removes elements where PRED is true."
  (let* ((pred (clel--fn pred)))
    (clel-filter-xf (lambda (x) (not (funcall pred x))))))

(defun clel-keep-xf (f)
  "Return a transducer that keeps non-nil results of (F item)."
  (let* ((f (clel--fn f)))
    (lambda (rf)
      (lambda (&rest args)
        (pcase (clel-count args)
          (0 (funcall rf))
          (1 (funcall rf (car args)))
          (2
           (let* ((v (funcall f (cadr args))))
             (if v (funcall rf (car args) v) (car args)))))))))

(defun clel-keep-indexed-xf (f)
  "Return a transducer that keeps non-nil results of (F index item)."
  (lambda (rf)
    (let* ((idx -1))
      (lambda (&rest args)
        (pcase (clel-count args)
          (0 (funcall rf))
          (1 (funcall rf (car args)))
          (2
           (setq idx (1+ idx))
           (let* ((v (funcall f idx (cadr args))))
             (if v (funcall rf (car args) v) (car args)))))))))

(defun clel-take-xf (n)
  "Return a transducer that takes first N elements."
  (lambda (rf)
    (let* ((remaining n))
      (lambda (&rest args)
        (pcase (clel-count args)
          (0 (funcall rf))
          (1 (funcall rf (car args)))
          (2
           (if (> remaining 0)
               (progn
                 (setq remaining (1- remaining))
                 (if (= remaining 0)
                     (clel-ensure-reduced (funcall rf (car args) (cadr args)))
                   (funcall rf (car args) (cadr args))))
             (car args))))))))

(defun clel-drop-xf (n)
  "Return a transducer that drops first N elements."
  (lambda (rf)
    (let* ((remaining n))
      (lambda (&rest args)
        (pcase (clel-count args)
          (0 (funcall rf))
          (1 (funcall rf (car args)))
          (2
           (if (> remaining 0)
               (progn (setq remaining (1- remaining)) (car args))
             (funcall rf (car args) (cadr args)))))))))

(defun clel-take-while-xf (pred)
  "Return a transducer that takes elements while PRED is true."
  (lambda (rf)
    (let* ((taking t))
      (lambda (&rest args)
        (pcase (clel-count args)
          (0 (funcall rf))
          (1 (funcall rf (car args)))
          (2
           (if taking
               (if (funcall pred (cadr args))
                   (funcall rf (car args) (cadr args))
                 (progn (setq taking nil) (clel-reduced (car args))))
             (car args))))))))

(defun clel-drop-while-xf (pred)
  "Return a transducer that drops elements while PRED is true."
  (lambda (rf)
    (let* ((dropping t))
      (lambda (&rest args)
        (pcase (clel-count args)
          (0 (funcall rf))
          (1 (funcall rf (car args)))
          (2
           (if dropping
               (if (funcall pred (cadr args))
                   (car args)
                 (progn
                   (setq dropping nil)
                   (funcall rf (car args) (cadr args))))
             (funcall rf (car args) (cadr args)))))))))

(defun clel-partition-all-xf (n)
  "Return a transducer that partitions into groups of N elements."
  (lambda (rf)
    (let* ((buffer nil))
      (lambda (&rest args)
        (pcase (clel-count args)
          (0 (funcall rf))
          (1
           (let* ((result (car args)))
             (when buffer (setq result (funcall rf result (nreverse buffer))))
             (funcall rf (clel-unreduced result))))
          (2
           (push (cadr args) buffer)
           (if (= (clel-count buffer) n)
               (let* ((group (nreverse buffer)))
                 (setq buffer nil)
                 (funcall rf (car args) group))
             (car args))))))))

(defun clel-partition-by-xf (f)
  "Return a transducer that partitions by changes in (F item)."
  (lambda (rf)
    (let* ((buffer nil) (prev-val 'clel--none))
      (lambda (&rest args)
        (pcase (clel-count args)
          (0 (funcall rf))
          (1
           (let* ((result (car args)))
             (when buffer (setq result (funcall rf result (nreverse buffer))))
             (funcall rf (clel-unreduced result))))
          (2
           (let* ((val (funcall f (cadr args))))
             (if (or (eq prev-val 'clel--none) (clel-equal val prev-val))
                 (progn
                   (push (cadr args) buffer)
                   (setq prev-val val)
                   (car args))
               (let* ((group (nreverse buffer)))
                 (setq buffer (list (cadr args)))
                 (setq prev-val val)
                 (funcall rf (car args) group))))))))))

(defun clel-dedupe-xf ()
  "Return a transducer that removes consecutive duplicates."
  (lambda (rf)
    (let* ((prev 'clel--none))
      (lambda (&rest args)
        (pcase (clel-count args)
          (0 (funcall rf))
          (1 (funcall rf (car args)))
          (2
           (let* ((item (cadr args)))
             (if (clel-equal item prev)
                 (car args)
               (progn (setq prev item) (funcall rf (car args) item))))))))))

(defun clel-distinct-xf ()
  "Return a transducer that removes all duplicates (not just consecutive)."
  (lambda (rf)
    (let* ((seen (make-hash-table :test 'equal)))
      (lambda (&rest args)
        (pcase (clel-count args)
          (0 (funcall rf))
          (1 (funcall rf (car args)))
          (2
           (let* ((item (cadr args)))
             (if (gethash item seen)
                 (car args)
               (progn
                 (puthash item t seen)
                 (funcall rf (car args) item))))))))))

(defun clel-interpose-xf (sep)
  "Return a transducer that interposes SEP between elements."
  (lambda (rf)
    (let* ((started nil))
      (lambda (&rest args)
        (pcase (clel-count args)
          (0 (funcall rf))
          (1 (funcall rf (car args)))
          (2
           (if started
               (let* ((result (funcall rf (car args) sep)))
                 (if (clel-reduced-p result)
                     result
                   (funcall rf result (cadr args))))
             (progn (setq started t) (funcall rf (car args) (cadr args))))))))))

(defun clel-cat-xf ()
  "Return a transducer that concatenates nested collections."
  (lambda (rf)
    (lambda (&rest args)
      (pcase (clel-count args)
        (0 (funcall rf))
        (1 (funcall rf (car args)))
        (2
         (let* ((result (car args)) (coll (clel-seq-force (cadr args))))
           (while (and coll (not (clel-reduced-p result)))
             (setq result (funcall rf result (clel-first coll)))
             (setq coll (clel-rest coll)))
           result))))))

(defun clel-mapcat-xf (f)
  "Return a transducer that maps F then concatenates results."
  (clel-comp (clel-map-xf f) (clel-cat-xf)))

(cl-defun clel-partition-all (n &optional step coll)
  "Partition COLL into groups of N elements, including final partial group.
With STEP, each group starts STEP elements apart.
With one arg, returns a transducer."
  (let* ((actual-step n) (actual-coll nil))
    (cond
     ((null step) (clel-partition-all-xf n))
     ((null coll) (progn (setq actual-coll step) (setq actual-step n)))
     (t (progn (setq actual-step step) (setq actual-coll coll))))
    (when actual-coll
      (clel-lazy-seq-create
       (lambda ()
         (let* ((forced (clel-seq-force actual-coll)))
           (when forced
             (let* ((group nil) (cur forced) (count 0))
               (while (and cur (< count n))
                 (push (clel-first cur) group)
                 (setq cur (clel-rest cur))
                 (setq count (1+ count)))
               (cons (nreverse group)
                     (clel-partition-all n
                                         actual-step
                                         (nthcdr actual-step forced)))))))))))

(cl-defun clel-keep (f &optional (coll nil coll-p))
  "Return lazy seq of non-nil results of (F item) for items in COLL.
With one argument, returns a transducer."
  (if (not coll-p)
      (clel-keep-xf f)
    (clel-lazy-seq-create
     (lambda ()
       (let* ((cur (clel-seq-force coll)) (f (clel--fn f)) (result nil))
         (while (and cur (not result))
           (setq result (funcall f (clel-first cur)))
           (unless result (setq cur (clel-rest cur))))
         (when result (cons result (clel-keep f (clel-rest cur)))))))))

(cl-defun clel-keep-indexed (f &optional coll)
  "Return lazy seq of non-nil results of (F index item) for items in COLL.
With one argument, returns a transducer."
  (if (null coll)
      (clel-keep-indexed-xf f)
    (let* ((idx -1))
      (clel-keep (lambda (item) (setq idx (1+ idx)) (funcall f idx item))
                 coll))))

(cl-defun clel-dedupe (&optional (coll nil coll-p))
  "Remove consecutive duplicates from COLL.
With no arguments, returns a transducer."
  (if (not coll-p)
      (clel-dedupe-xf)
    (clel-lazy-seq-create
     (lambda ()
       (let* ((forced (clel-seq-force coll)))
         (when forced
           (let* ((first-item (clel-first forced))
                  (rest-items (clel-rest forced)))
             (while (and rest-items
                         (clel-equal (clel-first rest-items) first-item))
               (setq rest-items (clel-rest rest-items)))
             (cons first-item (clel-dedupe rest-items)))))))))

(defun clel--distinct-from (seen coll)
  "Lazy seq of the items of COLL that are not already keys of the SEEN table.
SEEN is carried across the whole sequence, so a duplicate is dropped however
far apart its occurrences are."
  (clel-lazy-seq-create
   (lambda ()
     (let* ((cur (clel-seq-force coll)) (item nil))
       (while (and cur (not item))
         (let* ((candidate (clel-first cur)))
           (if (gethash candidate seen)
               (setq cur (clel-rest cur))
             (progn (puthash candidate t seen) (setq item candidate)))))
       (when item (cons item (clel--distinct-from seen (clel-rest cur))))))))

(cl-defun clel-distinct (&optional (coll nil coll-p))
  "Remove all duplicates from COLL (not just consecutive).
With no arguments, returns a transducer."
  (if (not coll-p)
      (clel-distinct-xf)
    (clel--distinct-from (make-hash-table :test 'equal) coll)))

(cl-defun clel-interpose (sep &optional (coll nil coll-p))
  "Interpose SEP between elements of COLL.
With one argument, returns a transducer."
  (if (not coll-p)
      (clel-interpose-xf sep)
    (clel-lazy-seq-create
     (lambda ()
       (let* ((forced (clel-seq-force coll)))
         (when forced
           (let* ((first-item (clel-first forced))
                  (rest-items (clel-rest forced)))
             (if rest-items
                 (cons first-item (cons sep (clel-interpose sep rest-items)))
               (list first-item)))))))))

(defun clel-zipmap (keys vals)
  "Create a map from the parallel sequences KEYS and VALS.
A later duplicate key wins."
  (let* ((ks (clel-seq keys)) (vs (clel-seq vals)) (result nil))
    (while (and ks vs)
      (setq result (clel--assoc-key result (car ks) (car vs)))
      (setq ks (cdr ks))
      (setq vs (cdr vs)))
    result))

(defun clel-select-keys (m ks)
  "Return a subset of map M containing only keys in KS."
  (let* ((key-list (clel-seq ks)) (result nil))
    (dolist (k key-list)
      (let* ((v (clel-get m k 'clel--not-found)))
        (unless (eq v 'clel--not-found) (push (clel--entry k v) result))))
    (nreverse result)))

(defun clel-complement (f)
  "Return a function that is the boolean complement of F."
  (lambda (&rest args) (not (clel-apply f args))))

(defun clel-juxt (&rest fns)
  "Return a function applying each of FNS to its args, collecting the results."
  (lambda (&rest args) (mapcar (lambda (f) (clel-apply f args)) fns)))

(cl-defun clel-rand (&optional n)
  "Return a random float between 0 (inclusive) and N (default 1, exclusive)."
  (let* ((r
          (/ (float (random most-positive-fixnum))
             (float most-positive-fixnum))))
    (if n (* n r) r)))

(defun clel-rand-int (n)
  "Return a random integer between 0 (inclusive) and N (exclusive)."
  (random n))

(defun clel-rand-nth (coll)
  "Return a random element from COLL."
  (let* ((s (clel-realize coll))) (nth (random (clel-count s)) s)))

(defun clel-slurp (path)
  "Read the entire contents of file at PATH as a string."
  (with-temp-buffer (insert-file-contents path) (buffer-string)))

(defun clel-spit (path content)
  "Write CONTENT to file at PATH."
  (with-temp-buffer
    (insert content)
    (write-region (point-min) (point-max) path)))

(defun clel-read-string (s)
  "Read a Clojure-like data structure from string S.
Returns the Elisp equivalent."
  (car (read-from-string s)))

(defun clel-str-split-lines (s)
  "Split string S into a list of lines."
  (if (null s) nil (split-string s "\n")))

(defun clel-peek (coll)
  "Return the element of COLL that `clel-pop' removes.
That is the last element of a vector and the first element of a list."
  (let* ((coll (clel-realize coll)))
    (cond
     ((null coll) nil)
     ((vectorp coll)
      (if (> (clel-count coll) 0) (aref coll (1- (clel-count coll))) nil))
     ((listp coll) (car coll))
     (t nil))))

(defun clel-pop (coll)
  "Return COLL without the element `clel-peek' returns.
That is all but the last element of a vector, and the rest of a list."
  (let* ((coll (clel-realize coll)))
    (cond
     ((null coll) nil)
     ((vectorp coll)
      (if (> (clel-count coll) 0)
          (cl-subseq coll 0 (1- (clel-count coll)))
        (vector)))
     ((listp coll) (cdr coll))
     (t nil))))

(cl-defun clel-subvec (v start &optional end)
  "Return a subvector of V from START to END (exclusive).
If END is not provided, uses the length of V."
  (let* ((e (or end (clel-count v)))) (cl-subseq v start e)))

(defun clel--cycle-helper (cur s)
  "Recursive helper for `clel-cycle'.
CUR is the current position in S, the original forced sequence."
  (clel-lazy-seq-create
   (lambda ()
     (if cur
         (cons (car cur) (clel--cycle-helper (cdr cur) s))
       (clel-seq-force (clel--cycle-helper s s))))))

(defun clel-cycle (coll)
  "Return a lazy infinite cycle of elements in COLL."
  (let* ((s (clel-realize coll))) (when s (clel--cycle-helper s s))))

(defun clel-iterate (f x)
  "Return the lazy sequence X, (F X), (F (F X)), and so on."
  (let* ((f (clel--fn f)))
    (clel-lazy-seq-create (lambda () (cons x (clel-iterate f (funcall f x)))))))

(defun clel--reductions-helper (f acc s)
  "Recursive helper for `clel-reductions'.
F is the reducing function, ACC the accumulator, S the remaining sequence."
  (clel-lazy-seq-create
   (lambda ()
     (if s
         (let* ((new-acc (funcall f acc (clel-first s))))
           (cons new-acc (clel--reductions-helper f new-acc (clel-rest s))))
       nil))))

(defun clel-reductions (f &rest args)
  "Return a lazy seq of the intermediate values of reducing with F.
ARGS is COLL, reduced from its first element, or INIT and COLL."
  (let* ((init nil) (coll nil))
    (if (= 1 (clel-count args))
        (let* ((s (clel-seq-force (car args))))
          (setq init (clel-first s))
          (setq coll (clel-rest s)))
      (progn (setq init (car args)) (setq coll (clel-seq-force (cadr args)))))
    (clel-lazy-seq-create
     (lambda () (cons init (clel--reductions-helper f init coll))))))

(defun clel-take-nth (n coll)
  "Return a lazy seq of every Nth element in COLL."
  (clel-lazy-seq-create
   (lambda ()
     (let* ((s (clel-seq-force coll)))
       (when s (cons (clel-first s) (clel-take-nth n (clel-drop n s))))))))

(defun clel-take-last (n coll)
  "Return the last N elements of COLL as a list."
  (let* ((s (clel-realize coll)))
    (let* ((len (clel-count s))) (if (<= len n) s (nthcdr (- len n) s)))))

(defun clel-drop-last (&rest args)
  "Return all but the last N elements of COLL.
ARGS is COLL, dropping one element, or N and COLL."
  (let* ((n nil) (coll nil))
    (if (= 1 (clel-count args))
        (progn (setq n 1) (setq coll (car args)))
      (progn (setq n (car args)) (setq coll (cadr args))))
    (let* ((s (clel-realize coll)) (len (clel-count s)))
      (if (<= len n) nil (cl-subseq s 0 (- len n))))))

(defun clel-vec (coll)
  "Return the items of COLL as a new realized list, as Clojure `vec'.
A map gives its entries, an entry its two items, a string its
characters."
  (if (stringp coll) (append coll nil) (copy-sequence (clel-seq coll))))

(defun clel-mapv (f &rest colls)
  "Return the realized list of F applied across COLLS, as Clojure `mapv'."
  (clel-realize (clel-apply #'clel-map f colls)))

(defun clel-filterv (pred coll)
  "Return the realized list of the items of COLL satisfying PRED.
This is Clojure `filterv'."
  (clel-realize (clel-filter pred coll)))

(defun clel-map-indexed (f coll)
  "Clojure `map-indexed': (F index item) for each item of COLL."
  (let* ((f (clel--fn f)) (i -1))
    (mapcar (lambda (x) (setq i (1+ i)) (funcall f i x)) (clel-seq coll))))

(defun clel-min-key (k x &rest more)
  "Return whichever of X and MORE has the least (K arg), as Clojure `min-key'.
The last one wins a tie."
  (let* ((k (clel--fn k)) (best x) (best-k (funcall k x)))
    (dolist (y more)
      (let* ((ky (funcall k y)))
        (when (<= ky best-k) (setq best y) (setq best-k ky))))
    best))

(defun clel-max-key (k x &rest more)
  "Return whichever of X and MORE has the greatest (K arg).
This is Clojure `max-key'; the last one wins a tie."
  (let* ((k (clel--fn k)) (best x) (best-k (funcall k x)))
    (dolist (y more)
      (let* ((ky (funcall k y)))
        (when (>= ky best-k) (setq best y) (setq best-k ky))))
    best))

(defun clel-name (x)
  "Return the name of X, as Clojure `name'.
That is a keyword or symbol's name without its namespace or colon; a
string is its own name."
  (if (stringp x)
      x
    (let* ((s (symbol-name x))
           (s (if (and (keywordp x) (string-prefix-p ":" s)) (substring s 1) s))
           (slash (string-search "/" s)))
      (if (and slash (> (length s) 1)) (substring s (1+ slash)) s))))

(defun clel-namespace (x)
  "Return the namespace of the keyword or symbol X, or nil."
  (let* ((s (symbol-name x))
         (s (if (and (keywordp x) (string-prefix-p ":" s)) (substring s 1) s))
         (slash (string-search "/" s)))
    (when (and slash (> (length s) 1)) (substring s 0 slash))))

(cl-defun clel-keyword (a &optional (b nil b-p))
  "Return a keyword, as Clojure `keyword'.
With A alone, A is the name.  With B, A is the namespace and B the name."
  (cond
   (b-p
    (intern (concat ":" (if a (concat (clel-name a) "/") "") (clel-name b))))
   ((keywordp a) a)
   ((null a) nil)
   (t (intern (concat ":" (if (symbolp a) (symbol-name a) a))))))

(cl-defun clel-symbol (a &optional (b nil b-p))
  "Return a symbol, as Clojure `symbol'.
With A alone, A is the name.  With B, A is the namespace and B the name."
  (cond
   (b-p (intern (concat (if a (concat a "/") "") b)))
   ((symbolp a) (if (keywordp a) (intern (clel-name a)) a))
   (t (intern a))))

(defun clel-boolean (x)
  "Return t for any X but nil, as Clojure `boolean'."
  (if x t nil))

(defun clel-ex-message (e)
  "Return the message of the condition E, as Clojure `ex-message'.
E is caught by `condition-case'.  That is the message of an `ex-info' or
`error', else the condition's printed message."
  (cond
   ((stringp e) e)
   ((and (consp e) (stringp (cadr e))) (cadr e))
   ((consp e) (error-message-string e))
   (t nil)))

(defun clel-ex-data (e)
  "Return the data map of E, a condition raised by `ex-info'."
  (when (and (consp e) (eq (car e) 'error) (stringp (cadr e))) (caddr e)))

(defun clel-parse-long (s)
  "Clojure `parse-long': the integer S spells, or nil."
  (when (and (stringp s) (string-match-p "\\`[+-]?[0-9]+\\'" s))
    (string-to-number s)))

(defun clel-parse-double (s)
  "Clojure `parse-double': the number S spells, as a float, or nil."
  (when (and (stringp s)
             (string-match-p
              "\\`[+-]?\\([0-9]+\\.?[0-9]*\\|\\.[0-9]+\\)\\([eE][+-]?[0-9]+\\)?\\'"
              s))
    (float (string-to-number s))))

(defun clel-math-floor (x)
  "Math/floor: the largest integral value not above X, as a float."
  (ffloor (float x)))

(defun clel-math-ceil (x)
  "Math/ceil: the smallest integral value not below X, as a float."
  (fceiling (float x)))

(defun clel-math-rint (x)
  "Math/rint: X rounded to the nearest integral value, ties to even, as a float."
  (fround (float x)))

(defun clel-math-round (x)
  "Math/round: X rounded to the nearest integer, ties toward positive infinity."
  (floor (+ (float x) 0.5)))

(defun clel-math-pow (x y)
  "Math/pow: X raised to Y, as a float."
  (expt (float x) y))

(defun clel-math-log10 (x)
  "Math/log10: the base-10 logarithm of X."
  (log x 10))

(defun clel-math-signum (x)
  "Math/signum: -1.0, 0.0 or 1.0 by the sign of X."
  (cond ((> x 0) 1.0) ((< x 0) -1.0) (t 0.0)))

(defun clel-math-hypot (x y)
  "Math/hypot: the square root of X squared plus Y squared."
  (sqrt (+ (* (float x) x) (* (float y) y))))

(defun clel-math-cbrt (x)
  "Math/cbrt: the cube root of X."
  (if (< x 0) (- (expt (float (- x)) (/ 1.0 3))) (expt (float x) (/ 1.0 3))))

(defun clel-current-time-millis ()
  "System/currentTimeMillis: the current time in milliseconds."
  (truncate (* 1000 (float-time))))

(defvar clel--protocol-registry (make-hash-table :test 'equal)
  "Registry mapping protocol names to their method lists.")

(defvar clel--protocol-impl-registry (make-hash-table :test 'equal)
  "Registry mapping (protocol . type) pairs to t if implemented.")

(defun clel--register-protocol (protocol-name methods)
  "Register PROTOCOL-NAME with METHODS, the list of its method names."
  (puthash protocol-name methods clel--protocol-registry))

(defun clel--register-impl (protocol-name type-name)
  "Register that TYPE-NAME implements PROTOCOL-NAME."
  (puthash (cons protocol-name type-name) t clel--protocol-impl-registry))

(defun clel--type-of (value)
  "Get the type of VALUE for protocol dispatch."
  (cond
   ((null value) 'null)
   ((stringp value) 'string)
   ((integerp value) 'integer)
   ((floatp value) 'float)
   ((symbolp value) 'symbol)
   ((vectorp value) 'vector)
   ((hash-table-p value) 'hash-table)
   ((listp value)
    (if (and (symbolp (car value)) (get (car value) 'cl-struct-type))
        (car value)
      'cons))
   (t (type-of value))))

(defun clel-satisfies-p (protocol-name value)
  "Return t if VALUE satisfies PROTOCOL-NAME."
  (let* ((value-type (clel--type-of value))
         (key (cons protocol-name value-type)))
    (or (gethash key clel--protocol-impl-registry)
        (let* ((methods (gethash protocol-name clel--protocol-registry)))
          (and methods
               (cl-some
                (lambda (method) (and (fboundp method) (cl-generic-p method)))
                methods))))))

(provide 'clel)
;;; clel.el ends here
