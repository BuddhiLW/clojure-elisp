;;; clojure-elisp-runtime.el --- Runtime library for ClojureElisp -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Pedro G. Branquinho
;; Author: Pedro G. Branquinho <pedrogbranquinho@gmail.com>
;; Maintainer: Pedro G. Branquinho <pedrogbranquinho@gmail.com>
;; URL: https://github.com/BuddhiLW/clojure-elisp
;; Version: 0.3.2
;; Package-Requires: ((emacs "28.1"))
;; Keywords: languages, lisp, clojure
;; SPDX-License-Identifier: MIT

;;; Commentary:
;;
;; Runtime support library for ClojureElisp compiled code.
;; Provides Clojure-like functions that don't have direct Elisp equivalents.
;; Auto-generated from runtime.cljel — do not edit by hand.

;;; Code:

(require 'cl-lib)
(require 'seq)

(defun clel-vector (&rest args)
  (let ((items (nthcdr 0 args)))
    "Create a vector from ITEMS."
  (apply #'vector items)))

(defun clel-hash-map (&rest args)
  (let ((kvs (nthcdr 0 args)))
    "Create a hash-table from key-value pairs KVS."
  (let* ((ht (make-hash-table :test 'equal))
        (rest kvs))
    (while rest
    (puthash (car rest) (cadr rest) ht)
    (setq rest (cddr rest)))
    ht)))

(defun clel-conj (coll item)
  "Add ITEM to collection COLL, returning new collection."
  (cond
  ((null coll) (list item))
  ((listp coll) (append coll (list item)))
  ((vectorp coll) (vconcat coll (vector item)))
  ((hash-table-p coll) (let* ((new (copy-hash-table coll)))
    (puthash (car item) (cdr item) new)
    new))
  (t (error "clel-conj: unsupported collection type"))))

(cl-defun clel-get (coll key &optional default)
  "Get KEY from COLL, returning DEFAULT if not found."
  (cond
  ((null coll) default)
  ((listp coll) (if (numberp key) (or (nth key coll) default) (or (alist-get key coll nil nil 'equal) default)))
  ((vectorp coll) (if (and (numberp key) (< key (length coll))) (aref coll key) default))
  ((hash-table-p coll) (gethash key coll default))
  (t default)))

(defun clel-assoc (coll key val)
  "Associate KEY with VAL in COLL, returning new collection."
  (cond
  ((null coll) (list (cons key val)))
  ((listp coll) (let* ((new (copy-alist coll)))
    (setf (alist-get key new nil nil 'equal) val)
    new))
  ((hash-table-p coll) (let* ((new (copy-hash-table coll)))
    (puthash key val new)
    new))
  (t (error "clel-assoc: unsupported collection type"))))

(defun clel-dissoc (coll key)
  "Remove KEY from COLL, returning new collection.\nWorks with alists and hash-tables."
  (cond
  ((null coll) nil)
  ((listp coll) (cl-remove-if (lambda (pair)
    (equal (car pair) key)) coll))
  ((hash-table-p coll) (let* ((new (copy-hash-table coll)))
    (remhash key new)
    new))
  (t (error "clel-dissoc: unsupported collection type"))))

(cl-defun clel-get-in (m ks &optional not-found)
  "Get nested value from M following keys KS.\nReturns NOT-FOUND (default nil) if path does not exist."
  (let* ((result m)
        (keys ks))
    (when (or (vectorp keys) (and (listp keys) (eq (car-safe keys) 'clel-lazy-seq)))
    (setq keys (if (vectorp keys) (append keys nil) (clel-seq-force keys))))
    (while (and keys result)
    (setq result (clel-get result (car keys)))
    (setq keys (cdr keys)))
    (if (null result) (or not-found nil) result)))

(defun clel-assoc-in (m ks v)
  "Associate value V at nested path KS in M.\nCreates intermediate maps as needed."
  (let* ((keys ks))
    (when (or (vectorp keys) (and (listp keys) (eq (car-safe keys) 'clel-lazy-seq)))
    (setq keys (if (vectorp keys) (append keys nil) (clel-seq-force keys))))
    (if (null keys) m (if (= 1 (length keys)) (clel-assoc m (car keys) v) (clel-assoc m (car keys) (clel-assoc-in (clel-get m (car keys)) (cdr keys) v))))))

(defun clel-update (&rest args)
  (let ((m (nth 0 args)) (k (nth 1 args)) (f (nth 2 args)) (args (nthcdr 3 args)))
    "Update value at K in M by applying F to old value and ARGS."
  (clel-assoc m k (apply f (clel-get m k) args))))

(defun clel-update-in (&rest args)
  (let ((m (nth 0 args)) (ks (nth 1 args)) (f (nth 2 args)) (args (nthcdr 3 args)))
    "Update value at nested path KS in M by applying F to old value and ARGS."
  (let* ((keys ks))
    (when (or (vectorp keys) (and (listp keys) (eq (car-safe keys) 'clel-lazy-seq)))
    (setq keys (if (vectorp keys) (append keys nil) (clel-seq-force keys))))
    (if (null keys) m (if (= 1 (length keys)) (apply #'clel-update m (car keys) f args) (clel-assoc m (car keys) (apply #'clel-update-in (clel-get m (car keys)) (cdr keys) f args)))))))

(defun clel-merge (&rest args)
  (let ((maps (nthcdr 0 args)))
    "Merge MAPS left to right.\nLater values override earlier. Returns alist or hash-table depending on first map."
  (if (null maps) nil (let* ((first-map (car maps))
        (result (cond
  ((null first-map) nil)
  ((hash-table-p first-map) (copy-hash-table first-map))
  ((listp first-map) (copy-alist first-map))
  (t (error "clel-merge: unsupported type")))))
    (dolist (m (cdr maps))
    (when m
    (cond
  ((hash-table-p result) (cond
  ((hash-table-p m) (maphash (lambda (k v)
    (puthash k v result)) m))
  ((listp m) (dolist (pair m)
    (puthash (car pair) (cdr pair) result)))))
  ((listp result) (cond
  ((hash-table-p m) (maphash (lambda (k v)
    (setf (alist-get k result nil nil 'equal) v)) m))
  ((listp m) (dolist (pair m)
    (setf (alist-get (car pair) result nil nil 'equal) (cdr pair)))))))))
    result))))

(defun clel-last (coll)
  "Return the last element of COLL.\nUnlike Elisp `last' which returns a cons cell, this returns the element itself."
  (cond
  ((null coll) nil)
  ((listp coll) (car (clel-last coll)))
  ((vectorp coll) (when (> (length coll) 0) (aref coll (1- (length coll)))))
  (t nil)))

(defun clel-contains-p (coll key)
  "Return t if KEY exists in COLL.\nFor maps/alists, checks if key is present.\nFor sets (represented as lists), checks if element is present.\nFor vectors, checks if index is valid."
  (cond
  ((null coll) nil)
  ((hash-table-p coll) (let* ((not-found (gensym)))
    (not (eq (gethash key coll not-found) not-found))))
  ((listp coll) (if (and (consp (car coll)) (not (listp (cdr (car coll))))) (not (null (assoc key coll))) (not (null (member key coll)))))
  ((vectorp coll) (and (integerp key) (>= key 0) (< key (length coll))))
  (t nil)))

(defun clel-keys (coll)
  "Return keys of COLL as a list."
  (cond
  ((null coll) nil)
  ((listp coll) (mapcar #'car coll))
  ((hash-table-p coll) (hash-table-keys coll))
  (t nil)))

(defun clel-vals (coll)
  "Return values of COLL as a list."
  (cond
  ((null coll) nil)
  ((listp coll) (mapcar #'cdr coll))
  ((hash-table-p coll) (hash-table-values coll))
  (t nil)))

(defun clel-seq (coll)
  "Return COLL as a sequence (list), or nil if empty."
  (cond
  ((null coll) nil)
  ((listp coll) (when coll coll))
  ((vectorp coll) (if (= 0 (length coll)) nil (append coll nil)))
  ((hash-table-p coll) (let* ((pairs nil))
    (maphash (lambda (k v)
    (push (cons k v) pairs)) coll)
    pairs))
  (t nil)))

(defun clel-into (to from)
  "Add all items FROM collection into TO collection.\nSupports vectors, lists, and hash-tables."
  (cond
  ((vectorp to) (vconcat to (if (vectorp from) from (apply #'vector (clel-seq from)))))
  ((listp to) (append to (if (listp from) from (append (clel-seq from) nil))))
  ((hash-table-p to) (let* ((new (copy-hash-table to)))
    (dolist (pair (clel-seq from))
    (puthash (car pair) (cdr pair) new))
    new))
  (t (error "clel-into: unsupported target collection type: %s" (type-of to)))))

(defun clel-coll-p (x)
  "Return t if X is a collection (list, vector, or hash-table)."
  (or (listp x) (vectorp x) (hash-table-p x)))

(defun clel-sequential-p (x)
  "Return t if X is sequential (list or vector)."
  (or (listp x) (vectorp x)))

(defun clel-associative-p (x)
  "Return t if X is associative (list or hash-table)."
  (or (listp x) (hash-table-p x)))

(defun clel-some-p (x)
  "Return t if X is not nil."
  (not (null x)))

(defun clel-true-p (x)
  "Return t if X is exactly t."
  (eq x t))

(defun clel-false-p (x)
  "Return t if X is exactly nil."
  (null x))

(defun clel-str (&rest args)
  (let ((args (nthcdr 0 args)))
    "Concatenate ARGS as strings."
  (mapconcat (lambda (x)
    (cond
  ((stringp x) x)
  ((null x) "")
  ((symbolp x) (symbol-name x))
  (t (format "%s" x)))) args "")))

(cl-defun clel-subs (s start &optional end)
  "Extract substring from S starting at START to END (optional)."
  (if (null s) "" (substring s start end)))

(defun clel-str-join (sep coll)
  "Join elements of COLL as strings, separated by SEP."
  (if (null coll) "" (let* ((strings (mapcar #'clel-str (clel-seq-force coll))))
    (string-join strings sep))))

(defun clel-str-split (s re)
  "Split S by regex RE."
  (if (null s) nil (split-string s re)))

(defun clel-str-replace (s match replacement)
  "Replace all occurrences of MATCH in S with REPLACEMENT.\nMATCH is treated as a literal string."
  (if (null s) "" (replace-regexp-in-string (regexp-quote match) replacement s)))

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
  (if (or (null s) (string-empty-p s)) "" (concat (upcase (substring s 0 1)) (downcase (substring s 1)))))

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
  (if (or (null s) (null substr)) nil (not (null (string-match-p (regexp-quote substr) s)))))

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
  (if (null s) "" (replace-regexp-in-string (regexp-quote match) replacement s nil nil 1)))

(defun clel-str-re-replace (s pattern replacement)
  "Replace all matches of regex PATTERN in S with REPLACEMENT."
  (if (null s) "" (replace-regexp-in-string pattern replacement s)))

(defun clel-str-re-replace-first (s pattern replacement)
  "Replace first match of regex PATTERN in S with REPLACEMENT."
  (if (null s) "" (replace-regexp-in-string pattern replacement s nil nil 1)))

(defun clel-str-re-matches (re s)
  "Return match data if RE matches entire string S, else nil."
  (if (or (null re) (null s)) nil (when (string-match-p (concat "\\`" re "\\'") s)
    (string-match re s)
    (match-string 0 s))))

(defun clel-str-re-find (re s)
  "Return first match of RE in S, or nil."
  (if (or (null re) (null s)) nil (when (string-match re s)
    (match-string 0 s))))

(defun clel-str-re-seq (re s)
  "Return list of all matches of RE in S."
  (if (or (null re) (null s)) nil (let* ((matches nil)
        (start 0))
    (while (string-match re s start)
    (push (match-string 0 s) matches)
    (setq start (match-end 0)))
    (nreverse matches))))

(cl-defun clel-str-index-of (s substr &optional from-index)
  "Return index of first occurrence of SUBSTR in S, or nil.\nOptional FROM-INDEX specifies starting position."
  (if (or (null s) (null substr)) nil (let* ((pos (string-match (regexp-quote substr) s (or from-index 0))))
    pos)))

(cl-defun clel-str-last-index-of (s substr &optional from-index)
  "Return index of last occurrence of SUBSTR in S, or nil."
  (if (or (null s) (null substr)) nil (let* ((len (length s))
        (sublen (length substr))
        (limit (or from-index len))
        (result nil))
    (cl-dotimes (i (cl-min (1+ limit) (- len sublen -1)))
    (when (and (<= (+ i sublen) len) (string= substr (substring s i (+ i sublen))))
    (setq result i)))
    result)))

(defun clel-constantly (x)
  "Return a function that always returns X."
  (lambda (& _)
    x))

(defun clel-comp (&rest args)
  (let ((fns (nthcdr 0 args)))
    "Compose functions FNS right-to-left."
  (lambda (x)
    (seq-reduce (lambda (v f)
    (funcall f v)) (reverse fns) x))))

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
    (let* ((key (car watcher))
        (f (cdr watcher)))
    (funcall f key atom old-val new-val)))))

(defun clel-reset-bang (atom val)
  "Reset ATOM to VAL, calling watchers."
  (let* ((old-val (nth 1 atom)))
    (setcar (nthcdr 1 atom) val)
    (clel--notify-watchers atom old-val val)
    val))

(defalias 'clel-reset! #'clel-reset-bang)

(defun clel-swap-bang (&rest args)
  (let ((atom (nth 0 args)) (f (nth 1 args)) (args (nthcdr 2 args)))
    "Swap ATOM by applying F to current value and ARGS, calling watchers."
  (let* ((old-val (clel-deref atom))
        (new-val (apply f old-val args)))
    (setcar (nthcdr 1 atom) new-val)
    (clel--notify-watchers atom old-val new-val)
    new-val)))

(defalias 'clel-swap! #'clel-swap-bang)

(defun clel-add-watch (atom key f)
  "Add watcher F to ATOM under KEY.\nF will be called with (key atom old-val new-val) when atom changes.\nReturns ATOM."
  (let* ((watchers (nth 2 atom)))
    (setq watchers (cl-remove-if (lambda (w)
    (equal (car w) key)) watchers))
    (setcar (nthcdr 2 atom) (cons (cons key f) watchers)))
  atom)

(defun clel-remove-watch (atom key)
  "Remove watcher with KEY from ATOM.\nReturns ATOM."
  (let* ((watchers (nth 2 atom)))
    (setcar (nthcdr 2 atom) (cl-remove-if (lambda (w)
    (equal (car w) key)) watchers)))
  atom)

(defun clel-lazy-seq-create (thunk)
  "Create a lazy sequence from THUNK."
  (list 'clel-lazy-seq thunk nil nil))

(defun clel-lazy-seq-p (x)
  "Return t if X is a lazy sequence."
  (and (consp x) (eq (car x) 'clel-lazy-seq)))

(defun clel-lazy-seq-force (lseq)
  "Force lazy sequence LSEQ, memoizing the result."
  (if (nth 3 lseq) (nth 2 lseq) (let* ((result (funcall (nth 1 lseq))))
    (setcar (nthcdr 2 lseq) result)
    (setcar (nthcdr 3 lseq) t)
    result)))

(defun clel-realized-p (x)
  "Return t if X is realized (not a pending lazy seq)."
  (if (clel-lazy-seq-p x) (nth 3 x) t))

(defun clel-doall (seq)
  "Force entire lazy SEQ, returning it."
  (let* ((s seq))
    (while (clel-lazy-seq-p s)
    (setq s (clel-lazy-seq-force s)))
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
  ((vectorp s) (when (> (length s) 0) (aref s 0)))
  (t nil)))

(defun clel-rest (s)
  "Return the rest of S (possibly empty list), forcing lazy seqs."
  (cond
  ((null s) nil)
  ((clel-lazy-seq-p s) (clel-rest (clel-lazy-seq-force s)))
  ((listp s) (cdr s))
  ((vectorp s) (when (> (length s) 1) (cdr (append s nil))))
  (t nil)))

(defun clel-next (s)
  "Return the next of S, or nil if empty. Forces lazy seqs."
  (let* ((r (clel-rest s)))
    (when (and r (not (equal r nil))) r)))

(defun clel-seq-force (s)
  "Ensure S is a realized sequence (list). Forces lazy seqs."
  (cond
  ((null s) nil)
  ((clel-lazy-seq-p s) (clel-seq-force (clel-lazy-seq-force s)))
  ((listp s) s)
  ((vectorp s) (append s nil))
  (t (list s))))

(defun clel-map (&rest args)
  (let ((f (nth 0 args)) (colls (nthcdr 1 args)))
    "Lazily map F over COLLS. With one coll, returns lazy seq."
  (if (= 1 (length colls)) (let* ((s (clel-seq-force (car colls))))
    (clel-lazy-seq-create (lambda ()
    (when s
    (cons (funcall f (clel-first s)) (clel-map f (clel-rest s))))))) (let* ((seqs (mapcar #'clel-seq-force colls)))
    (clel-lazy-seq-create (lambda ()
    (when (cl-every #'identity seqs)
    (cons (apply f (mapcar #'clel-first seqs)) (apply #'clel-map f (mapcar #'clel-rest seqs))))))))))

(defun clel-filter (pred s)
  "Lazily filter S by PRED."
  (let* ((s (clel-seq-force s)))
    (clel-lazy-seq-create (lambda ()
    (let* ((cur s))
    (while (and cur (not (funcall pred (clel-first cur))))
    (setq cur (clel-rest cur)))
    (when cur
    (cons (clel-first cur) (clel-filter pred (clel-rest cur)))))))))

(defun clel-take (n s)
  "Lazily take N elements from S."
  (clel-lazy-seq-create (lambda ()
    (when (and (> n 0) s)
    (let* ((forced (clel-seq-force s)))
    (when forced
    (cons (clel-first forced) (clel-take (1- n) (clel-rest forced)))))))))

(defun clel-drop (n s)
  "Drop N elements from S, return rest lazily."
  (clel-lazy-seq-create (lambda ()
    (let* ((cur (clel-seq-force s))
        (remaining n))
    (while (and (> remaining 0) cur)
    (setq cur (clel-rest cur))
    (setq remaining (1- remaining)))
    cur))))

(defun clel-take-while (pred s)
  "Lazily take elements from S while PRED is true."
  (clel-lazy-seq-create (lambda ()
    (let* ((forced (clel-seq-force s)))
    (when (and forced (funcall pred (clel-first forced)))
    (cons (clel-first forced) (clel-take-while pred (clel-rest forced))))))))

(defun clel-drop-while (pred s)
  "Drop elements from S while PRED is true, return rest lazily."
  (clel-lazy-seq-create (lambda ()
    (let* ((cur (clel-seq-force s)))
    (while (and cur (funcall pred (clel-first cur)))
    (setq cur (clel-rest cur)))
    cur))))

(defun clel-concat (&rest args)
  (let ((colls (nthcdr 0 args)))
    "Lazily concatenate COLLS."
  (if (null colls) nil (let* ((first-coll (clel-seq-force (car colls)))
        (rest-colls (cdr colls)))
    (clel-lazy-seq-create (lambda ()
    (if first-coll (cons (clel-first first-coll) (apply #'clel-concat (cons (clel-rest first-coll) rest-colls))) (when rest-colls
    (clel-seq-force (apply #'clel-concat rest-colls))))))))))

(defun clel-mapcat (&rest args)
  (let ((f (nth 0 args)) (colls (nthcdr 1 args)))
    "Map F over COLLS and concatenate results lazily."
  (apply #'clel-concat (clel-doall (apply #'clel-map f colls)))))

(defun clel-interleave (&rest args)
  (let ((colls (nthcdr 0 args)))
    "Lazily interleave COLLS."
  (let* ((seqs (mapcar #'clel-seq-force colls)))
    (clel-lazy-seq-create (lambda ()
    (when (cl-every #'identity seqs)
    (let* ((firsts (mapcar #'clel-first seqs))
        (rests (mapcar #'clel-rest seqs)))
    (append firsts (clel-seq-force (apply #'clel-interleave rests))))))))))

(defun clel-partition (n s)
  "Partition S into groups of N elements. Returns lazy seq of lists."
  (clel-lazy-seq-create (lambda ()
    (let* ((forced (clel-seq-force s)))
    (when forced
    (let* ((group nil)
        (cur forced)
        (count 0))
    (while (and cur (< count n))
    (push (clel-first cur) group)
    (setq cur (clel-rest cur))
    (setq count (1+ count)))
    (when (= count n)
    (cons (nreverse group) (clel-partition n cur)))))))))

(defun clel-partition-by (f s)
  "Partition S into groups by the value of (F elem).\nEach group contains consecutive elements with the same (F elem) value."
  (clel-lazy-seq-create (lambda ()
    (let* ((forced (clel-seq-force s)))
    (when forced
    (let* ((first-elem (clel-first forced))
        (first-val (funcall f first-elem))
        (group (list first-elem))
        (cur (clel-rest forced)))
    (while (and cur (equal (funcall f (clel-first cur)) first-val))
    (push (clel-first cur) group)
    (setq cur (clel-rest cur)))
    (cons (nreverse group) (clel-partition-by f cur))))))))

(defun clel-split-at (n s)
  "Split S at position N. Returns list of (take n s) and (drop n s)."
  (list (clel-doall (clel-take n s)) (clel-doall (clel-drop n s))))

(defun clel-split-with (pred s)
  "Split S at first element where PRED is false.\nReturns list of (take-while pred s) and (drop-while pred s)."
  (list (clel-doall (clel-take-while pred s)) (clel-doall (clel-drop-while pred s))))

(defun clel-reduce (&rest args)
  (let ((f (nth 0 args)) (args (nthcdr 1 args)))
    "Reduce S with F. (clel-reduce f coll) or (clel-reduce f init coll)."
  (let* ((init nil)
        (s nil))
    (if (= 1 (length args)) (let* ((coll (clel-seq-force (car args))))
    (setq init (clel-first coll))
    (setq s (clel-rest coll))) (progn
  (setq init (car args))
  (setq s (clel-seq-force (cadr args)))))
    (let* ((acc init)
        (cur s))
    (while cur
    (setq acc (funcall f acc (clel-first cur)))
    (setq cur (clel-rest cur)))
    acc))))

(defun clel-sort (cmp coll)
  "Sort COLL using comparator CMP. Returns a new list."
  (let* ((lst (copy-sequence (clel-seq-force coll))))
    (sort lst cmp)))

(defun clel-sort-by (keyfn coll)
  "Sort COLL by KEYFN. Uses < for comparison on key values."
  (let* ((lst (copy-sequence (clel-seq-force coll))))
    (sort lst (lambda (a b)
    (let* ((ka (funcall keyfn a))
        (kb (funcall keyfn b)))
    (cond
  ((and (numberp ka) (numberp kb)) (< ka kb))
  ((and (stringp ka) (stringp kb)) (string< ka kb))
  (t (string< (format "%s" ka) (format "%s" kb)))))))))

(defun clel-group-by (f coll)
  "Group elements of COLL by the result of F. Returns alist."
  (let* ((result nil)
        (cur (clel-seq-force coll)))
    (while cur
    (let* ((item (clel-first cur))
        (key (funcall f item))
        (existing (assoc key result)))
    (if existing (setcdr existing (append (cdr existing) (list item))) (push (cons key (list item)) result)))
    (setq cur (clel-rest cur)))
    (nreverse result)))

(defun clel-frequencies (coll)
  "Return alist of (element . count) for elements in COLL."
  (let* ((result nil)
        (cur (clel-seq-force coll)))
    (while cur
    (let* ((item (clel-first cur))
        (existing (assoc item result)))
    (if existing (setcdr existing (1+ (cdr existing))) (push (cons item 1) result)))
    (setq cur (clel-rest cur)))
    (nreverse result)))

(defun clel-every-p (pred coll)
  "Return t if PRED is true for every element in COLL."
  (let* ((cur (clel-seq-force coll))
        (result t))
    (while (and cur result)
    (unless (funcall pred (clel-first cur))
    (setq result nil))
    (setq cur (clel-rest cur)))
    result))

(defun clel-some (pred coll)
  "Return the first truthy value of (PRED item) for items in COLL, or nil."
  (let* ((cur (clel-seq-force coll))
        (result nil))
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
  "Return t if COLL is empty or nil. Lazy-seq aware."
  (null (clel-seq-force coll)))

(defun clel-range (&rest args)
  (let ((args (nthcdr 0 args)))
    "Generate a range of numbers.\n(range) - returns empty list (infinite range not supported)\n(range end) - returns (0 1 ... end-1)\n(range start end) - returns (start start+1 ... end-1)\n(range start end step) - returns (start start+step ...) up to but not including end"
  (let* ((start 0)
        (end nil)
        (step 1))
    (pcase (length args)
  (0 nil)
  (1 (setq end (car args)))
  (2 (setq start (car args) end (cadr args)))
  ('_ (setq start (car args) end (cadr args) step (caddr args))))
    (when end
    (let* ((result nil)
        (i start))
    (if (> step 0) (while (< i end)
    (push i result)
    (setq i (+ i step))) (when (< step 0)
    (while (> i end)
    (push i result)
    (setq i (+ i step)))))
    (nreverse result))))))

(defun clel-repeat (n x)
  "Return a list of N copies of X."
  (let* ((result nil))
    (cl-dotimes (_ n)
    (push x result))
    result))

(defun clel-repeatedly (n f)
  "Call F N times with no arguments, returning a list of results."
  (let* ((result nil))
    (cl-dotimes (_ n)
    (push (funcall f) result))
    (nreverse result)))

(defun clel-set (&rest args)
  (let ((items (nthcdr 0 args)))
    "Create a set from ITEMS.\nReturns a hash-table where each item is a key with value t."
  (let* ((s (make-hash-table :test 'equal)))
    (dolist (item items)
    (puthash item t s))
    s)))

(defun clel-set-from-coll (coll)
  "Create a set from collection COLL."
  (let* ((s (make-hash-table :test 'equal)))
    (dolist (item (clel-seq-force coll))
    (puthash item t s))
    s))

(defun clel-set-p (x)
  "Return t if X is a set (hash-table with all values t)."
  (and (hash-table-p x) (let* ((is-set t))
    (maphash (lambda (k v)
    (unless (eq v t)
    (setq is-set nil))) x)
    is-set)))

(defun clel-set-contains-p (s item)
  "Return t if set S contains ITEM."
  (if (hash-table-p s) (gethash item s nil) (not (null (member item s)))))

(defun clel-set-add (s item)
  "Add ITEM to set S, returning new set."
  (let* ((new (copy-hash-table s)))
    (puthash item t new)
    new))

(defun clel-set-remove (s item)
  "Remove ITEM from set S, returning new set."
  (let* ((new (copy-hash-table s)))
    (remhash item new)
    new))

(defun clel-set-union (&rest args)
  (let ((sets (nthcdr 0 args)))
    "Return the union of SETS."
  (let* ((result (make-hash-table :test 'equal)))
    (dolist (s sets)
    (if (hash-table-p s) (maphash (lambda (k v)
    (puthash k t result)) s) (dolist (item (clel-seq-force s))
    (puthash item t result))))
    result)))

(defun clel-set-intersection (&rest args)
  (let ((sets (nthcdr 0 args)))
    "Return the intersection of SETS."
  (if (null sets) (make-hash-table :test 'equal) (let* ((first-set (car sets))
        (rest-sets (cdr sets))
        (result (make-hash-table :test 'equal)))
    (if (hash-table-p first-set) (maphash (lambda (k v)
    (when (cl-every (lambda (s)
    (if (hash-table-p s) (gethash k s) (member k s))) rest-sets)
    (puthash k t result))) first-set) (dolist (item (clel-seq-force first-set))
    (when (cl-every (lambda (s)
    (if (hash-table-p s) (gethash item s) (member item s))) rest-sets)
    (puthash item t result))))
    result))))

(defun clel-set-difference (&rest args)
  (let ((s1 (nth 0 args)) (sets (nthcdr 1 args)))
    "Return items in S1 not in any of SETS."
  (let* ((result (make-hash-table :test 'equal)))
    (if (hash-table-p s1) (maphash (lambda (k v)
    (unless (cl-some (lambda (s)
    (if (hash-table-p s) (gethash k s) (member k s))) sets)
    (puthash k t result))) s1) (dolist (item (clel-seq-force s1))
    (unless (cl-some (lambda (s)
    (if (hash-table-p s) (gethash item s) (member item s))) sets)
    (puthash item t result))))
    result)))

(defun clel-set-subset-p (s1 s2)
  "Return t if S1 is a subset of S2."
  (let* ((result t))
    (if (hash-table-p s1) (maphash (lambda (k v)
    (unless (if (hash-table-p s2) (gethash k s2) (member k s2))
    (setq result nil))) s1) (dolist (item (clel-seq-force s1))
    (unless (if (hash-table-p s2) (gethash item s2) (member item s2))
    (setq result nil))))
    result))

(defun clel-set-superset-p (s1 s2)
  "Return t if S1 is a superset of S2."
  (clel-set-subset-p s2 s1))

(defun clel-set-select (pred s)
  "Return a set of items in S for which PRED returns true."
  (let* ((result (make-hash-table :test 'equal)))
    (if (hash-table-p s) (maphash (lambda (k v)
    (when (funcall pred k)
    (puthash k t result))) s) (dolist (item (clel-seq-force s))
    (when (funcall pred item)
    (puthash item t result))))
    result))

(defun clel-set-project (xrel ks)
  "Project a relation XREL (set of maps) onto the keys in KS."
  (let* ((result (make-hash-table :test 'equal))
        (key-list (clel-seq-force ks)))
    (if (hash-table-p xrel) (maphash (lambda (m v)
    (let* ((projected nil))
    (dolist (k key-list)
    (let* ((val (clel-get m k)))
    (when val
    (push (cons k val) projected))))
    (puthash (nreverse projected) t result))) xrel) (dolist (m (clel-seq-force xrel))
    (let* ((projected nil))
    (dolist (k key-list)
    (let* ((val (clel-get m k)))
    (when val
    (push (cons k val) projected))))
    (puthash (nreverse projected) t result))))
    result))

(defun clel-set-rename (xrel kmap)
  "Rename keys in relation XREL according to KMAP (old-key . new-key) pairs."
  (let* ((result (make-hash-table :test 'equal))
        (rename-map (if (hash-table-p kmap) kmap (let* ((ht (make-hash-table :test 'equal)))
    (dolist (pair kmap)
    (puthash (car pair) (cdr pair) ht))
    ht))))
    (if (hash-table-p xrel) (maphash (lambda (m v)
    (let* ((renamed nil))
    (cond
  ((hash-table-p m) (maphash (lambda (k val)
    (let* ((new-key (or (gethash k rename-map) k)))
    (push (cons new-key val) renamed))) m))
  ((listp m) (dolist (pair m)
    (let* ((new-key (or (gethash (car pair) rename-map) (car pair))))
    (push (cons new-key (cdr pair)) renamed)))))
    (puthash (nreverse renamed) t result))) xrel) (dolist (m (clel-seq-force xrel))
    (let* ((renamed nil))
    (cond
  ((hash-table-p m) (maphash (lambda (k val)
    (let* ((new-key (or (gethash k rename-map) k)))
    (push (cons new-key val) renamed))) m))
  ((listp m) (dolist (pair m)
    (let* ((new-key (or (gethash (car pair) rename-map) (car pair))))
    (push (cons new-key (cdr pair)) renamed)))))
    (puthash (nreverse renamed) t result))))
    result))

(defun clel-rename-keys (m kmap)
  "Rename keys in map M according to KMAP (old-key . new-key) pairs."
  (let* ((rename-map (if (hash-table-p kmap) kmap (let* ((ht (make-hash-table :test 'equal)))
    (dolist (pair kmap)
    (puthash (car pair) (cdr pair) ht))
    ht)))
        (result nil))
    (cond
  ((hash-table-p m) (let* ((new-ht (make-hash-table :test 'equal)))
    (maphash (lambda (k v)
    (let* ((new-key (or (gethash k rename-map) k)))
    (puthash new-key v new-ht))) m)
    new-ht))
  ((listp m) (progn
  (dolist (pair m)
    (let* ((new-key (or (gethash (car pair) rename-map) (car pair))))
    (push (cons new-key (cdr pair)) result)))
  (nreverse result)))
  (t m))))

(cl-defun clel-set-join (xrel yrel &optional km)
  "Natural join of relations XREL and YREL.\nIf KM is provided, it maps keys from XREL to keys in YREL."
  (let* ((result (make-hash-table :test 'equal))
        (x-list (if (hash-table-p xrel) (let* ((items nil))
    (maphash (lambda (k v)
    (push k items)) xrel)
    items) (clel-seq-force xrel)))
        (y-list (if (hash-table-p yrel) (let* ((items nil))
    (maphash (lambda (k v)
    (push k items)) yrel)
    items) (clel-seq-force yrel))))
    (dolist (xm x-list)
    (dolist (ym y-list)
    (let* ((xm-keys (clel-keys xm))
        (ym-keys (clel-keys ym))
        (match t))
    (let* ((common-keys (if km (let* ((mapped nil))
    (dolist (k xm-keys)
    (let* ((yk (clel-get km k)))
    (when (and yk (member yk ym-keys))
    (push k mapped))))
    mapped) (cl-remove-if-not (lambda (k)
    (member k ym-keys)) xm-keys))))
    (dolist (xk common-keys)
    (let* ((yk (if km (clel-get km xk) xk)))
    (unless (equal (clel-get xm xk) (clel-get ym yk))
    (setq match nil))))
    (when match
    (let* ((merged (clel-merge xm ym)))
    (puthash merged t result)))))))
    result))

(defun clel-set-index (xrel ks)
  "Index relation XREL on keys KS.\nReturns a map from key-values to sets of matching maps."
  (let* ((result nil)
        (key-list (clel-seq-force ks))
        (x-list (if (hash-table-p xrel) (let* ((items nil))
    (maphash (lambda (k v)
    (push k items)) xrel)
    items) (clel-seq-force xrel))))
    (dolist (m x-list)
    (let* ((key-vals nil))
    (dolist (k key-list)
    (push (cons k (clel-get m k)) key-vals))
    (setq key-vals (nreverse key-vals))
    (let* ((existing (clel-get result key-vals)))
    (if existing (puthash m t existing) (let* ((new-set (make-hash-table :test 'equal)))
    (puthash m t new-set)
    (setq result (clel-assoc result key-vals new-set)))))))
    result))

(defun clel-map-invert (m)
  "Invert map M, swapping keys and values.\nValues must be unique, or later entries will overwrite earlier ones."
  (cond
  ((hash-table-p m) (let* ((result (make-hash-table :test 'equal)))
    (maphash (lambda (k v)
    (puthash v k result)) m)
    result))
  ((listp m) (let* ((result nil))
    (dolist (pair m)
    (push (cons (cdr pair) (car pair)) result))
    (nreverse result)))
  (t nil)))

(defun clel--reducing-fn-init (rf)
  "Call RF with 0 arguments for init value."
  (condition-case nil
    (funcall rf)
  (error nil)))

(defun clel--reducing-fn-complete (rf result)
  "Call RF with 1 argument for completion."
  (condition-case nil
    (funcall rf result)
  (error result)))

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

(defun clel-transduce (&rest args)
  (let ((xform (nth 0 args)) (f (nth 1 args)) (args (nthcdr 2 args)))
    "Transduce COLL with transducer XFORM and reducing function F.\nUsage: (clel-transduce xform f coll) or (clel-transduce xform f init coll)"
  (let* ((init nil)
        (coll nil))
    (if (= 1 (length args)) (progn
  (setq coll (clel-seq-force (car args)))
  (setq init (clel--reducing-fn-init f))) (progn
  (setq init (car args))
  (setq coll (clel-seq-force (cadr args)))))
    (let* ((xf (funcall xform f))
        (result init)
        (cur coll))
    (while (and cur (not (clel-reduced-p result)))
    (setq result (funcall xf result (clel-first cur)))
    (setq cur (clel-rest cur)))
    (clel--reducing-fn-complete xf (clel-unreduced result))))))

(defun clel-into-xform (to xform from)
  "Add all items FROM into TO, transformed by XFORM."
  (let* ((rf (cond
  ((vectorp to) (lambda (& args)
    (pcase (length args)
  (0 (vector))
  (1 (car args))
  (2 (vconcat (car args) (vector (cadr args)))))))
  ((listp to) (lambda (& args)
    (pcase (length args)
  (0 nil)
  (1 (nreverse (car args)))
  (2 (cons (cadr args) (car args))))))
  ((hash-table-p to) (lambda (& args)
    (pcase (length args)
  (0 (make-hash-table :test 'equal))
  (1 (car args))
  (2 (let* ((ht (car args))
        (pair (cadr args)))
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
  (clel-transduce xform (lambda (& args)
    (pcase (length args)
  (0 nil)
  (1 (nreverse (car args)))
  (2 (cons (cadr args) (car args))))) nil coll))

(defun clel-eduction (xform coll)
  "Return a reducible/iterable application of XFORM to COLL."
  (list 'clel-eduction xform coll))

(defun clel-eduction-p (x)
  "Return t if X is an eduction."
  (and (consp x) (eq (car x) 'clel-eduction)))

(defun clel-map-xf (f)
  "Return a mapping transducer that applies F to each element."
  (lambda (rf)
    (lambda (& args)
    (pcase (length args)
  (0 (funcall rf))
  (1 (funcall rf (car args)))
  (2 (funcall rf (car args) (funcall f (cadr args))))))))

(defun clel-filter-xf (pred)
  "Return a filtering transducer that keeps elements where PRED is true."
  (lambda (rf)
    (lambda (& args)
    (pcase (length args)
  (0 (funcall rf))
  (1 (funcall rf (car args)))
  (2 (if (funcall pred (cadr args)) (funcall rf (car args) (cadr args)) (car args)))))))

(defun clel-remove-xf (pred)
  "Return a transducer that removes elements where PRED is true."
  (clel-filter-xf (lambda (x)
    (not (funcall pred x)))))

(defun clel-keep-xf (f)
  "Return a transducer that keeps non-nil results of (F item)."
  (lambda (rf)
    (lambda (& args)
    (pcase (length args)
  (0 (funcall rf))
  (1 (funcall rf (car args)))
  (2 (let* ((v (funcall f (cadr args))))
    (if v (funcall rf (car args) v) (car args))))))))

(defun clel-keep-indexed-xf (f)
  "Return a transducer that keeps non-nil results of (F index item)."
  (lambda (rf)
    (let* ((idx -1))
    (lambda (& args)
    (pcase (length args)
  (0 (funcall rf))
  (1 (funcall rf (car args)))
  (2 (setq idx (1+ idx)) (let* ((v (funcall f idx (cadr args))))
    (if v (funcall rf (car args) v) (car args)))))))))

(defun clel-take-xf (n)
  "Return a transducer that takes first N elements."
  (lambda (rf)
    (let* ((remaining n))
    (lambda (& args)
    (pcase (length args)
  (0 (funcall rf))
  (1 (funcall rf (car args)))
  (2 (if (> remaining 0) (progn
  (setq remaining (1- remaining))
  (if (= remaining 0) (clel-ensure-reduced (funcall rf (car args) (cadr args))) (funcall rf (car args) (cadr args)))) (car args))))))))

(defun clel-drop-xf (n)
  "Return a transducer that drops first N elements."
  (lambda (rf)
    (let* ((remaining n))
    (lambda (& args)
    (pcase (length args)
  (0 (funcall rf))
  (1 (funcall rf (car args)))
  (2 (if (> remaining 0) (progn
  (setq remaining (1- remaining))
  (car args)) (funcall rf (car args) (cadr args)))))))))

(defun clel-take-while-xf (pred)
  "Return a transducer that takes elements while PRED is true."
  (lambda (rf)
    (let* ((taking t))
    (lambda (& args)
    (pcase (length args)
  (0 (funcall rf))
  (1 (funcall rf (car args)))
  (2 (if taking (if (funcall pred (cadr args)) (funcall rf (car args) (cadr args)) (progn
  (setq taking nil)
  (clel-reduced (car args)))) (car args))))))))

(defun clel-drop-while-xf (pred)
  "Return a transducer that drops elements while PRED is true."
  (lambda (rf)
    (let* ((dropping t))
    (lambda (& args)
    (pcase (length args)
  (0 (funcall rf))
  (1 (funcall rf (car args)))
  (2 (if dropping (if (funcall pred (cadr args)) (car args) (progn
  (setq dropping nil)
  (funcall rf (car args) (cadr args)))) (funcall rf (car args) (cadr args)))))))))

(defun clel-partition-all-xf (n)
  "Return a transducer that partitions into groups of N elements."
  (lambda (rf)
    (let* ((buffer nil))
    (lambda (& args)
    (pcase (length args)
  (0 (funcall rf))
  (1 (let* ((result (car args)))
    (when buffer
    (setq result (funcall rf result (nreverse buffer))))
    (funcall rf (clel-unreduced result))))
  (2 (push (cadr args) buffer) (if (= (length buffer) n) (let* ((group (nreverse buffer)))
    (setq buffer nil)
    (funcall rf (car args) group)) (car args))))))))

(defun clel-partition-by-xf (f)
  "Return a transducer that partitions by changes in (F item)."
  (lambda (rf)
    (let* ((buffer nil)
        (prev-val 'clel--none))
    (lambda (& args)
    (pcase (length args)
  (0 (funcall rf))
  (1 (let* ((result (car args)))
    (when buffer
    (setq result (funcall rf result (nreverse buffer))))
    (funcall rf (clel-unreduced result))))
  (2 (let* ((val (funcall f (cadr args))))
    (if (or (eq prev-val 'clel--none) (equal val prev-val)) (progn
  (push (cadr args) buffer)
  (setq prev-val val)
  (car args)) (let* ((group (nreverse buffer)))
    (setq buffer (list (cadr args)))
    (setq prev-val val)
    (funcall rf (car args) group))))))))))

(defun clel-dedupe-xf ()
  "Return a transducer that removes consecutive duplicates."
  (lambda (rf)
    (let* ((prev 'clel--none))
    (lambda (& args)
    (pcase (length args)
  (0 (funcall rf))
  (1 (funcall rf (car args)))
  (2 (let* ((item (cadr args)))
    (if (equal item prev) (car args) (progn
  (setq prev item)
  (funcall rf (car args) item))))))))))

(defun clel-distinct-xf ()
  "Return a transducer that removes all duplicates (not just consecutive)."
  (lambda (rf)
    (let* ((seen (make-hash-table :test 'equal)))
    (lambda (& args)
    (pcase (length args)
  (0 (funcall rf))
  (1 (funcall rf (car args)))
  (2 (let* ((item (cadr args)))
    (if (gethash item seen) (car args) (progn
  (puthash item t seen)
  (funcall rf (car args) item))))))))))

(defun clel-interpose-xf (sep)
  "Return a transducer that interposes SEP between elements."
  (lambda (rf)
    (let* ((started nil))
    (lambda (& args)
    (pcase (length args)
  (0 (funcall rf))
  (1 (funcall rf (car args)))
  (2 (if started (let* ((result (funcall rf (car args) sep)))
    (if (clel-reduced-p result) result (funcall rf result (cadr args)))) (progn
  (setq started t)
  (funcall rf (car args) (cadr args))))))))))

(defun clel-cat-xf ()
  "Return a transducer that concatenates nested collections."
  (lambda (rf)
    (lambda (& args)
    (pcase (length args)
  (0 (funcall rf))
  (1 (funcall rf (car args)))
  (2 (let* ((result (car args))
        (coll (clel-seq-force (cadr args))))
    (while (and coll (not (clel-reduced-p result)))
    (setq result (funcall rf result (clel-first coll)))
    (setq coll (clel-rest coll)))
    result))))))

(defun clel-mapcat-xf (f)
  "Return a transducer that maps F then concatenates results."
  (clel-comp (clel-map-xf f) (clel-cat-xf)))

(cl-defun clel-partition-all (n &optional step coll)
  "Partition COLL into groups of N elements, including final partial group.\nWith STEP, each group starts STEP elements apart.\nWith one arg, returns a transducer."
  (let* ((actual-step n)
        (actual-coll nil))
    (cond
  ((null step) (clel-partition-all-xf n))
  ((null coll) (progn
  (setq actual-coll step)
  (setq actual-step n)))
  (t (progn
  (setq actual-step step)
  (setq actual-coll coll))))
    (when actual-coll
    (clel-lazy-seq-create (lambda ()
    (let* ((forced (clel-seq-force actual-coll)))
    (when forced
    (let* ((group nil)
        (cur forced)
        (count 0))
    (while (and cur (< count n))
    (push (clel-first cur) group)
    (setq cur (clel-rest cur))
    (setq count (1+ count)))
    (cons (nreverse group) (clel-partition-all n actual-step (nthcdr actual-step forced)))))))))))

(cl-defun clel-keep (f &optional coll)
  "Return lazy seq of non-nil results of (F item) for items in COLL.\nWith one argument, returns a transducer."
  (if (null coll) (clel-keep-xf f) (clel-lazy-seq-create (lambda ()
    (let* ((cur (clel-seq-force coll))
        (result nil))
    (while (and cur (not result))
    (setq result (funcall f (clel-first cur)))
    (unless result
    (setq cur (clel-rest cur))))
    (when result
    (cons result (clel-keep f (clel-rest cur)))))))))

(cl-defun clel-keep-indexed (f &optional coll)
  "Return lazy seq of non-nil results of (F index item) for items in COLL.\nWith one argument, returns a transducer."
  (if (null coll) (clel-keep-indexed-xf f) (let* ((idx -1))
    (clel-keep (lambda (item)
    (setq idx (1+ idx))
    (funcall f idx item)) coll))))

(cl-defun clel-dedupe (&optional coll)
  "Remove consecutive duplicates from COLL.\nWith no arguments, returns a transducer."
  (if (null coll) (clel-dedupe-xf) (clel-lazy-seq-create (lambda ()
    (let* ((forced (clel-seq-force coll)))
    (when forced
    (let* ((first-item (clel-first forced))
        (rest-items (clel-rest forced)))
    (while (and rest-items (equal (clel-first rest-items) first-item))
    (setq rest-items (clel-rest rest-items)))
    (cons first-item (clel-dedupe rest-items)))))))))

(cl-defun clel-distinct (&optional coll)
  "Remove all duplicates from COLL (not just consecutive).\nWith no arguments, returns a transducer."
  (if (null coll) (clel-distinct-xf) (let* ((seen (make-hash-table :test 'equal)))
    (clel-lazy-seq-create (lambda ()
    (let* ((cur (clel-seq-force coll))
        (item nil))
    (while (and cur (not item))
    (let* ((candidate (clel-first cur)))
    (if (gethash candidate seen) (setq cur (clel-rest cur)) (progn
  (puthash candidate t seen)
  (setq item candidate)))))
    (when item
    (cons item (clel-distinct (clel-rest cur))))))))))

(cl-defun clel-interpose (sep &optional coll)
  "Interpose SEP between elements of COLL.\nWith one argument, returns a transducer."
  (if (null coll) (clel-interpose-xf sep) (clel-lazy-seq-create (lambda ()
    (let* ((forced (clel-seq-force coll)))
    (when forced
    (let* ((first-item (clel-first forced))
        (rest-items (clel-rest forced)))
    (if rest-items (cons first-item (cons sep (clel-interpose sep rest-items))) (list first-item)))))))))

(defun clel-zipmap (keys vals)
  "Create an alist from parallel sequences KEYS and VALS."
  (let* ((ks (clel-seq-force keys))
        (vs (clel-seq-force vals))
        (result nil))
    (while (and ks vs)
    (push (cons (car ks) (car vs)) result)
    (setq ks (cdr ks))
    (setq vs (cdr vs)))
    (nreverse result)))

(defun clel-select-keys (m ks)
  "Return a subset of map M containing only keys in KS."
  (let* ((key-list (clel-seq-force ks))
        (result nil))
    (dolist (k key-list)
    (let* ((v (clel-get m k 'clel--not-found)))
    (unless (eq v 'clel--not-found)
    (push (cons k v) result))))
    (nreverse result)))

(defun clel-complement (f)
  "Return a function that is the boolean complement of F."
  (lambda (& args)
    (not (apply f args))))

(defun clel-juxt (&rest args)
  (let ((fns (nthcdr 0 args)))
    "Return a function that applies each of FNS to its args, returning a list of results."
  (lambda (& args)
    (mapcar (lambda (f)
    (apply f args)) fns))))

(cl-defun clel-rand (&optional n)
  "Return a random float between 0 (inclusive) and N (default 1, exclusive)."
  (let* ((r (/ (float (random most-positive-fixnum)) (float most-positive-fixnum))))
    (if n (* n r) r)))

(defun clel-rand-int (n)
  "Return a random integer between 0 (inclusive) and N (exclusive)."
  (random n))

(defun clel-rand-nth (coll)
  "Return a random element from COLL."
  (let* ((s (clel-seq-force coll)))
    (nth (random (length s)) s)))

(defun clel-slurp (path)
  "Read the entire contents of file at PATH as a string."
  (with-temp-buffer
    (insert-file-contents path)
    (buffer-string)))

(defun clel-spit (path content)
  "Write CONTENT to file at PATH."
  (with-temp-buffer
    (insert content)
    (write-region (point-min) (point-max) path)))

(defun clel-read-string (s)
  "Read a Clojure-like data structure from string S.\nReturns the Elisp equivalent."
  (car (read-from-string s)))

(defun clel-str-split-lines (s)
  "Split string S into a list of lines."
  (if (null s) nil (split-string s "\n")))

(defun clel-peek (coll)
  "Return the last element of a vector, or first element of a list.\nFor vectors (represented as lists in ClojureElisp), returns last element.\nFor lists, returns first element."
  (cond
  ((null coll) nil)
  ((vectorp coll) (when (> (length coll) 0) (aref coll (1- (length coll)))))
  ((listp coll) (car coll))
  (t nil)))

(defun clel-pop (coll)
  "Return collection without the peek element.\nFor vectors, returns all but last. For lists, returns rest."
  (cond
  ((null coll) nil)
  ((vectorp coll) (if (> (length coll) 0) (cl-subseq coll 0 (1- (length coll))) (vector)))
  ((listp coll) (cdr coll))
  (t nil)))

(cl-defun clel-subvec (v start &optional end)
  "Return a subvector of V from START to END (exclusive).\nIf END is not provided, uses the length of V."
  (let* ((e (or end (length v))))
    (cl-subseq v start e)))

(defun clel--cycle-helper (cur s)
  "Recursive helper for `clel-cycle'.\nCUR is the current position in S, the original forced sequence."
  (clel-lazy-seq-create (lambda ()
    (if cur (cons (car cur) (clel--cycle-helper (cdr cur) s)) (clel-seq-force (clel--cycle-helper s s))))))

(defun clel-cycle (coll)
  "Return a lazy infinite cycle of elements in COLL."
  (let* ((s (clel-seq-force coll)))
    (when s
    (clel--cycle-helper s s))))

(defun clel-iterate (f x)
  "Return a lazy sequence of x, (f x), (f (f x)), etc."
  (clel-lazy-seq-create (lambda ()
    (cons x (clel-iterate f (funcall f x))))))

(defun clel--reductions-helper (f acc s)
  "Recursive helper for `clel-reductions'.\nF is the reducing function, ACC the accumulator, S the remaining sequence."
  (clel-lazy-seq-create (lambda ()
    (when s (let* ((new-acc (funcall f acc (clel-first s))))
    (cons new-acc (clel--reductions-helper f new-acc (clel-rest s))))))))

(defun clel-reductions (&rest args)
  (let ((f (nth 0 args)) (args (nthcdr 1 args)))
    "Return a lazy seq of intermediate reduce values.\nUsage: (clel-reductions f coll) or (clel-reductions f init coll)."
  (let* ((init nil)
        (coll nil))
    (if (= 1 (length args)) (let* ((s (clel-seq-force (car args))))
    (setq init (clel-first s))
    (setq coll (clel-rest s))) (progn
  (setq init (car args))
  (setq coll (clel-seq-force (cadr args)))))
    (clel-lazy-seq-create (lambda ()
    (cons init (clel--reductions-helper f init coll)))))))

(defun clel-take-nth (n coll)
  "Return a lazy seq of every Nth element in COLL."
  (clel-lazy-seq-create (lambda ()
    (let* ((s (clel-seq-force coll)))
    (when s
    (cons (clel-first s) (clel-take-nth n (clel-drop n s))))))))

(defun clel-take-last (n coll)
  "Return the last N elements of COLL as a list."
  (let* ((s (clel-seq-force coll)))
    (let* ((len (length s)))
    (if (<= len n) s (nthcdr (- len n) s)))))

(defun clel-drop-last (&rest args)
  (let ((args (nthcdr 0 args)))
    "Return all but the last N elements of COLL.\nUsage: (clel-drop-last coll) or (clel-drop-last n coll)."
  (let* ((n nil)
        (coll nil))
    (if (= 1 (length args)) (progn
  (setq n 1)
  (setq coll (car args))) (progn
  (setq n (car args))
  (setq coll (cadr args))))
    (let* ((s (clel-seq-force coll))
        (len (length s)))
    (if (<= len n) nil (cl-subseq s 0 (- len n)))))))

(defvar clel--protocol-registry (make-hash-table :test 'equal)
  "Registry mapping protocol names to their method lists.")

(defvar clel--protocol-impl-registry (make-hash-table :test 'equal)
  "Registry mapping (protocol . type) pairs to t if implemented.")

(defun clel--register-protocol (protocol-name methods)
  "Register PROTOCOL-NAME with its METHOD names."
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
  ((listp value) (if (and (symbolp (car value)) (get (car value) 'cl-struct-type)) (car value) 'cons))
  (t (type-of value))))

(defun clel-satisfies-p (protocol-name value)
  "Return t if VALUE satisfies PROTOCOL-NAME."
  (let* ((value-type (clel--type-of value))
        (key (cons protocol-name value-type)))
    (or (gethash key clel--protocol-impl-registry) (let* ((methods (gethash protocol-name clel--protocol-registry)))
    (and methods (cl-some (lambda (method)
    (and (fboundp method) (cl-generic-p method))) methods))))))

(provide 'clojure-elisp-runtime)
;;; clojure-elisp-runtime.el ends here
