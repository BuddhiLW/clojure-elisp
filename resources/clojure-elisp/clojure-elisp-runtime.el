;;; clojure-elisp-runtime.el --- Runtime library for ClojureElisp -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Pedro G. Branquinho
;; Author: Pedro G. Branquinho <pedrogbranquinho@gmail.com>
;; Version: 0.1.0
;; Package-Requires: ((emacs "28.1"))
;; Keywords: languages, lisp, clojure
;; SPDX-License-Identifier: MIT

;;; Commentary:
;;
;; Runtime support library for ClojureElisp compiled code.
;; Provides Clojure-like functions that don't have direct Elisp equivalents.

;;; Code:

(require 'cl-lib)
(require 'seq)

;;; Persistent-ish Data Structures

(defun clel-vector (&rest items)
  "Create a vector from ITEMS."
  (apply #'vector items))

(defun clel-hash-map (&rest kvs)
  "Create a hash-table from key-value pairs KVS."
  (let ((ht (make-hash-table :test 'equal)))
    (cl-loop for (k v) on kvs by #'cddr
             do (puthash k v ht))
    ht))

;;; Collection Operations

(defun clel-conj (coll item)
  "Add ITEM to collection COLL, returning new collection."
  (cond
   ((null coll) (list item))
   ((listp coll) (append coll (list item)))
   ((vectorp coll) (vconcat coll (vector item)))
   ((hash-table-p coll)
    (let ((new (copy-hash-table coll)))
      (puthash (car item) (cdr item) new)
      new))
   (t (error "clel-conj: unsupported collection type"))))

(defun clel-get (coll key &optional default)
  "Get KEY from COLL, returning DEFAULT if not found."
  (cond
   ((null coll) default)
   ((listp coll)
    (if (numberp key)
        (or (nth key coll) default)
      (or (alist-get key coll nil nil 'equal) default)))
   ((vectorp coll)
    (if (and (numberp key) (< key (length coll)))
        (aref coll key)
      default))
   ((hash-table-p coll)
    (gethash key coll default))
   (t default)))

(defun clel-assoc (coll key val)
  "Associate KEY with VAL in COLL, returning new collection."
  (cond
   ((null coll) (list (cons key val)))
   ((listp coll)
    (let ((new (copy-alist coll)))
      (setf (alist-get key new nil nil 'equal) val)
      new))
   ((hash-table-p coll)
    (let ((new (copy-hash-table coll)))
      (puthash key val new)
      new))
   (t (error "clel-assoc: unsupported collection type"))))

(defun clel-dissoc (coll key)
  "Remove KEY from COLL, returning new collection.
Works with alists and hash-tables."
  (cond
   ((null coll) nil)
   ((listp coll)
    (cl-remove-if (lambda (pair) (equal (car pair) key)) coll))
   ((hash-table-p coll)
    (let ((new (copy-hash-table coll)))
      (remhash key new)
      new))
   (t (error "clel-dissoc: unsupported collection type"))))

(defun clel-get-in (m ks &optional not-found)
  "Get nested value from M following keys KS.
Returns NOT-FOUND (default nil) if path does not exist."
  (let ((result m)
        (keys ks))
    ;; Force keys if it's a lazy seq or vector
    (when (or (vectorp keys) (and (listp keys) (eq (car-safe keys) 'clel-lazy-seq)))
      (setq keys (if (vectorp keys) (append keys nil) (clel-seq-force keys))))
    (while (and keys result)
      (setq result (clel-get result (car keys)))
      (setq keys (cdr keys)))
    (if (null result)
        (or not-found nil)
      result)))

(defun clel-assoc-in (m ks v)
  "Associate value V at nested path KS in M.
Creates intermediate maps as needed."
  (let ((keys ks))
    ;; Force keys if it's a lazy seq or vector
    (when (or (vectorp keys) (and (listp keys) (eq (car-safe keys) 'clel-lazy-seq)))
      (setq keys (if (vectorp keys) (append keys nil) (clel-seq-force keys))))
    (if (null keys)
        m
      (if (= 1 (length keys))
          (clel-assoc m (car keys) v)
        (clel-assoc m (car keys)
                    (clel-assoc-in (clel-get m (car keys)) (cdr keys) v))))))

(defun clel-update (m k f &rest args)
  "Update value at K in M by applying F to old value and ARGS."
  (clel-assoc m k (apply f (clel-get m k) args)))

(defun clel-update-in (m ks f &rest args)
  "Update value at nested path KS in M by applying F to old value and ARGS."
  (let ((keys ks))
    ;; Force keys if it's a lazy seq or vector
    (when (or (vectorp keys) (and (listp keys) (eq (car-safe keys) 'clel-lazy-seq)))
      (setq keys (if (vectorp keys) (append keys nil) (clel-seq-force keys))))
    (if (null keys)
        m
      (if (= 1 (length keys))
          (apply #'clel-update m (car keys) f args)
        (clel-assoc m (car keys)
                    (apply #'clel-update-in (clel-get m (car keys)) (cdr keys) f args))))))

(defun clel-merge (&rest maps)
  "Merge MAPS left to right.
Later values override earlier. Returns alist or hash-table depending on first map."
  (if (null maps)
      nil
    (let* ((first-map (car maps))
           (result (cond
                    ((null first-map) nil)
                    ((hash-table-p first-map) (copy-hash-table first-map))
                    ((listp first-map) (copy-alist first-map))
                    (t (error "clel-merge: unsupported type")))))
      (dolist (m (cdr maps))
        (when m
          (cond
           ((hash-table-p result)
            (cond
             ((hash-table-p m)
              (maphash (lambda (k v) (puthash k v result)) m))
             ((listp m)
              (dolist (pair m)
                (puthash (car pair) (cdr pair) result)))))
           ((listp result)
            (cond
             ((hash-table-p m)
              (maphash (lambda (k v)
                         (setf (alist-get k result nil nil 'equal) v))
                       m))
             ((listp m)
              (dolist (pair m)
                (setf (alist-get (car pair) result nil nil 'equal) (cdr pair)))))))))
      result)))

(defun clel-last (coll)
  "Return the last element of COLL.
Unlike Elisp `last' which returns a cons cell, this returns the element itself."
  (cond
   ((null coll) nil)
   ((listp coll) (car (last coll)))
   ((vectorp coll) (if (> (length coll) 0)
                       (aref coll (1- (length coll)))
                     nil))
   (t nil)))

(defun clel-contains-p (coll key)
  "Return t if KEY exists in COLL.
For maps/alists, checks if key is present.
For sets (represented as lists), checks if element is present.
For vectors, checks if index is valid."
  (cond
   ((null coll) nil)
   ((hash-table-p coll)
    (let ((not-found (gensym)))
      (not (eq (gethash key coll not-found) not-found))))
   ((listp coll)
    ;; Check if alist (pairs) or set (elements)
    (if (and (consp (car coll)) (not (listp (cdr (car coll)))))
        ;; Alist - check keys
        (not (null (assoc key coll)))
      ;; Set or list - check membership
      (not (null (member key coll)))))
   ((vectorp coll)
    ;; For vectors, check if index is valid
    (and (integerp key)
         (>= key 0)
         (< key (length coll))))
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
   ((listp coll) (if coll coll nil))
   ((vectorp coll) (if (= 0 (length coll)) nil (append coll nil)))
   ((hash-table-p coll)
    (let ((pairs nil))
      (maphash (lambda (k v) (push (cons k v) pairs)) coll)
      pairs))
   (t nil)))

(defun clel-into (to from)
  "Add all items FROM collection into TO collection.
Supports vectors, lists, and hash-tables.
Examples:
  (clel-into [] '(1 2 3)) => [1 2 3]
  (clel-into '() [1 2 3]) => (1 2 3)
  (clel-into {} '((:a . 1) (:b . 2))) => hash-table"
  (cond
   ;; into vector
   ((vectorp to)
    (vconcat to (if (vectorp from) from (apply #'vector (clel-seq from)))))
   ;; into list
   ((listp to)
    (append to (if (listp from) from (append (clel-seq from) nil))))
   ;; into hash-table
   ((hash-table-p to)
    (let ((new (copy-hash-table to)))
      (dolist (pair (clel-seq from))
        (puthash (car pair) (cdr pair) new))
      new))
   (t (error "clel-into: unsupported target collection type: %s" (type-of to)))))

;;; Collection Predicates

(defun clel-coll-p (x)
  "Return t if X is a collection (list, vector, or hash-table)."
  (or (listp x) (vectorp x) (hash-table-p x)))

(defun clel-sequential-p (x)
  "Return t if X is sequential (list or vector)."
  (or (listp x) (vectorp x)))

(defun clel-associative-p (x)
  "Return t if X is associative (list or hash-table)."
  (or (listp x) (hash-table-p x)))

;;; Boolean/Nil Predicates

(defun clel-some-p (x)
  "Return t if X is not nil."
  (not (null x)))

(defun clel-true-p (x)
  "Return t if X is exactly t."
  (eq x t))

(defun clel-false-p (x)
  "Return t if X is exactly nil."
  (null x))

;;; String Operations

(defun clel-str (&rest args)
  "Concatenate ARGS as strings."
  (mapconcat (lambda (x)
               (cond
                ((stringp x) x)
                ((null x) "")
                ((symbolp x) (symbol-name x))
                (t (format "%s" x))))
             args ""))

(defun clel-subs (s start &optional end)
  "Extract substring from S starting at START to END (optional)."
  (if (null s) ""
      (substring s start end)))

(defun clel-str-join (sep coll)
  "Join elements of COLL as strings, separated by SEP."
  (if (null coll) ""
      (let ((strings (mapcar #'clel-str (clel-seq-force coll))))
        (string-join strings sep))))

(defun clel-str-split (s re)
  "Split S by regex RE."
  (if (null s) nil
      (split-string s re)))

(defun clel-str-replace (s match replacement)
  "Replace all occurrences of MATCH in S with REPLACEMENT.
MATCH is treated as a literal string."
  (if (null s) ""
      (replace-regexp-in-string (regexp-quote match) replacement s)))

(defun clel-str-trim (s)
  "Trim whitespace from both ends of S."
  (if (null s) ""
      (string-trim s)))

(defun clel-str-lower (s)
  "Convert S to lowercase."
  (if (null s) ""
      (downcase s)))

(defun clel-str-upper (s)
  "Convert S to uppercase."
  (if (null s) ""
      (upcase s)))

;;; Function Utilities

(defun clel-constantly (x)
  "Return a function that always returns X."
  (lambda (&rest _) x))

(defun clel-comp (&rest fns)
  "Compose functions FNS right-to-left."
  (lambda (x)
    (seq-reduce (lambda (v f) (funcall f v))
                (reverse fns)
                x)))

;;; Atoms
;; Structure: (list 'clel-atom value watchers-alist)
;; - (nth 1 atom) = value
;; - (nth 2 atom) = watchers alist ((key1 . fn1) (key2 . fn2) ...)

(defun clel-atom (val)
  "Create an atom with initial value VAL."
  (list 'clel-atom val nil))

(defun clel-deref (atom)
  "Get the value of ATOM."
  (nth 1 atom))

(defun clel--notify-watchers (atom old-val new-val)
  "Call all watchers on ATOM with OLD-VAL and NEW-VAL."
  (let ((watchers (nth 2 atom)))
    (dolist (watcher watchers)
      (let ((key (car watcher))
            (fn (cdr watcher)))
        (funcall fn key atom old-val new-val)))))

(defun clel-reset! (atom val)
  "Reset ATOM to VAL, calling watchers."
  (let ((old-val (nth 1 atom)))
    (setcar (nthcdr 1 atom) val)
    (clel--notify-watchers atom old-val val)
    val))

(defun clel-swap! (atom fn &rest args)
  "Swap ATOM by applying FN to current value and ARGS, calling watchers."
  (let* ((old-val (clel-deref atom))
         (new-val (apply fn old-val args)))
    (setcar (nthcdr 1 atom) new-val)
    (clel--notify-watchers atom old-val new-val)
    new-val))

(defun clel-add-watch (atom key fn)
  "Add watcher FN to ATOM under KEY.
FN will be called with (key atom old-val new-val) when atom changes.
Returns ATOM."
  (let ((watchers (nth 2 atom)))
    ;; Remove existing watcher with same key if present
    (setq watchers (cl-remove-if (lambda (w) (equal (car w) key)) watchers))
    ;; Add new watcher
    (setcar (nthcdr 2 atom) (cons (cons key fn) watchers)))
  atom)

(defun clel-remove-watch (atom key)
  "Remove watcher with KEY from ATOM.
Returns ATOM."
  (let ((watchers (nth 2 atom)))
    (setcar (nthcdr 2 atom)
            (cl-remove-if (lambda (w) (equal (car w) key)) watchers)))
  atom)

;;; Lazy Sequences
;; Structure: (list 'clel-lazy-seq thunk result realized-p)
;; - (nth 1 lseq) = thunk (lambda producing the sequence)
;; - (nth 2 lseq) = memoized result
;; - (nth 3 lseq) = realized flag (t or nil)

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
    (let ((result (funcall (nth 1 lseq))))
      (setcar (nthcdr 2 lseq) result)
      (setcar (nthcdr 3 lseq) t)
      result)))

(defun clel-realized-p (x)
  "Return t if X is realized (not a pending lazy seq)."
  (if (clel-lazy-seq-p x) (nth 3 x) t))

(defun clel-doall (seq)
  "Force entire lazy SEQ, returning it."
  (let ((s seq))
    (while (clel-lazy-seq-p s)
      (setq s (clel-lazy-seq-force s)))
    ;; Walk the realized list to force any nested lazy seqs
    (when (listp s)
      (let ((current s))
        (while current
          (when (clel-lazy-seq-p (car current))
            (setcar current (clel-doall (car current))))
          (setq current (cdr current)))))
    s))

(defun clel-dorun (seq)
  "Force entire lazy SEQ for side effects, returning nil."
  (clel-doall seq)
  nil)

;;; Sequence Abstraction (clel-029)
;; Lazy-seq aware first/rest/next — foundation for all seq functions.

(defun clel-first (s)
  "Return the first element of S, forcing lazy seqs."
  (cond
   ((null s) nil)
   ((clel-lazy-seq-p s) (clel-first (clel-lazy-seq-force s)))
   ((listp s) (car s))
   ((vectorp s) (if (> (length s) 0) (aref s 0) nil))
   (t nil)))

(defun clel-rest (s)
  "Return the rest of S (possibly empty list), forcing lazy seqs."
  (cond
   ((null s) nil)
   ((clel-lazy-seq-p s) (clel-rest (clel-lazy-seq-force s)))
   ((listp s) (cdr s))
   ((vectorp s) (if (> (length s) 1)
                    (cdr (append s nil))
                  nil))
   (t nil)))

(defun clel-next (s)
  "Return the next of S, or nil if empty. Forces lazy seqs."
  (let ((r (clel-rest s)))
    (if (and r (not (equal r nil)))
        r
      nil)))

(defun clel-seq-force (s)
  "Ensure S is a realized sequence (list). Forces lazy seqs."
  (cond
   ((null s) nil)
   ((clel-lazy-seq-p s) (clel-seq-force (clel-lazy-seq-force s)))
   ((listp s) s)
   ((vectorp s) (append s nil))
   (t (list s))))

;;; Lazy Sequence Functions

(defun clel-map (f &rest colls)
  "Lazily map F over COLLS. With one coll, returns lazy seq."
  (if (= 1 (length colls))
      (let ((s (clel-seq-force (car colls))))
        (clel-lazy-seq-create
         (lambda ()
           (when s
             (cons (funcall f (clel-first s))
                   (clel-map f (clel-rest s)))))))
    ;; Multi-coll: zip-map
    (let ((seqs (mapcar #'clel-seq-force colls)))
      (clel-lazy-seq-create
       (lambda ()
         (when (cl-every #'identity seqs)
           (cons (apply f (mapcar #'clel-first seqs))
                 (apply #'clel-map f (mapcar #'clel-rest seqs)))))))))

(defun clel-filter (pred s)
  "Lazily filter S by PRED."
  (let ((s (clel-seq-force s)))
    (clel-lazy-seq-create
     (lambda ()
       (let ((cur s))
         (while (and cur (not (funcall pred (clel-first cur))))
           (setq cur (clel-rest cur)))
         (when cur
           (cons (clel-first cur)
                 (clel-filter pred (clel-rest cur)))))))))

(defun clel-take (n s)
  "Lazily take N elements from S."
  (clel-lazy-seq-create
   (lambda ()
     (when (and (> n 0) s)
       (let ((forced (clel-seq-force s)))
         (when forced
           (cons (clel-first forced)
                 (clel-take (1- n) (clel-rest forced)))))))))

(defun clel-drop (n s)
  "Drop N elements from S, return rest lazily."
  (clel-lazy-seq-create
   (lambda ()
     (let ((cur (clel-seq-force s))
           (remaining n))
       (while (and (> remaining 0) cur)
         (setq cur (clel-rest cur))
         (setq remaining (1- remaining)))
       cur))))

(defun clel-take-while (pred s)
  "Lazily take elements from S while PRED is true."
  (clel-lazy-seq-create
   (lambda ()
     (let ((forced (clel-seq-force s)))
       (when (and forced (funcall pred (clel-first forced)))
         (cons (clel-first forced)
               (clel-take-while pred (clel-rest forced))))))))

(defun clel-drop-while (pred s)
  "Drop elements from S while PRED is true, return rest lazily."
  (clel-lazy-seq-create
   (lambda ()
     (let ((cur (clel-seq-force s)))
       (while (and cur (funcall pred (clel-first cur)))
         (setq cur (clel-rest cur)))
       cur))))

(defun clel-concat (&rest colls)
  "Lazily concatenate COLLS."
  (if (null colls)
      nil
    (let ((first-coll (clel-seq-force (car colls)))
          (rest-colls (cdr colls)))
      (clel-lazy-seq-create
       (lambda ()
         (if first-coll
             (cons (clel-first first-coll)
                   (apply #'clel-concat (cons (clel-rest first-coll) rest-colls)))
           (when rest-colls
             (clel-seq-force (apply #'clel-concat rest-colls)))))))))

(defun clel-mapcat (f &rest colls)
  "Map F over COLLS and concatenate results lazily."
  (apply #'clel-concat (clel-doall (apply #'clel-map f colls))))

(defun clel-interleave (&rest colls)
  "Lazily interleave COLLS."
  (let ((seqs (mapcar #'clel-seq-force colls)))
    (clel-lazy-seq-create
     (lambda ()
       (when (cl-every #'identity seqs)
         (let ((firsts (mapcar #'clel-first seqs))
               (rests (mapcar #'clel-rest seqs)))
           (append firsts (clel-seq-force (apply #'clel-interleave rests)))))))))

(defun clel-partition (n s)
  "Partition S into groups of N elements. Returns lazy seq of lists."
  (clel-lazy-seq-create
   (lambda ()
     (let ((forced (clel-seq-force s)))
       (when forced
         (let ((group nil)
               (cur forced)
               (count 0))
           (while (and cur (< count n))
             (push (clel-first cur) group)
             (setq cur (clel-rest cur))
             (setq count (1+ count)))
           (when (= count n)
             (cons (nreverse group)
                   (clel-partition n cur)))))))))

;;; Eager Sequence Functions

(defun clel-reduce (f &rest args)
  "Reduce S with F. (clel-reduce f coll) or (clel-reduce f init coll)."
  (let (init s)
    (if (= 1 (length args))
        ;; (reduce f coll) — no init value
        (let ((coll (clel-seq-force (car args))))
          (setq init (clel-first coll))
          (setq s (clel-rest coll)))
      ;; (reduce f init coll)
      (setq init (car args))
      (setq s (clel-seq-force (cadr args))))
    (let ((acc init)
          (cur s))
      (while cur
        (setq acc (funcall f acc (clel-first cur)))
        (setq cur (clel-rest cur)))
      acc)))

(defun clel-sort (cmp coll)
  "Sort COLL using comparator CMP. Returns a new list."
  (let ((lst (copy-sequence (clel-seq-force coll))))
    (sort lst cmp)))

(defun clel-sort-by (keyfn coll)
  "Sort COLL by KEYFN. Uses < for comparison on key values."
  (let ((lst (copy-sequence (clel-seq-force coll))))
    (sort lst (lambda (a b)
                (let ((ka (funcall keyfn a))
                      (kb (funcall keyfn b)))
                  (cond
                   ((and (numberp ka) (numberp kb)) (< ka kb))
                   ((and (stringp ka) (stringp kb)) (string< ka kb))
                   (t (string< (format "%s" ka) (format "%s" kb)))))))))

(defun clel-group-by (f coll)
  "Group elements of COLL by the result of F. Returns alist."
  (let ((result nil)
        (cur (clel-seq-force coll)))
    (while cur
      (let* ((item (clel-first cur))
             (key (funcall f item))
             (existing (assoc key result)))
        (if existing
            (setcdr existing (append (cdr existing) (list item)))
          (push (cons key (list item)) result)))
      (setq cur (clel-rest cur)))
    (nreverse result)))

(defun clel-frequencies (coll)
  "Return alist of (element . count) for elements in COLL."
  (let ((result nil)
        (cur (clel-seq-force coll)))
    (while cur
      (let* ((item (clel-first cur))
             (existing (assoc item result)))
        (if existing
            (setcdr existing (1+ (cdr existing)))
          (push (cons item 1) result)))
      (setq cur (clel-rest cur)))
    (nreverse result)))

;;; Sequence Predicates

(defun clel-every-p (pred coll)
  "Return t if PRED is true for every element in COLL."
  (let ((cur (clel-seq-force coll))
        (result t))
    (while (and cur result)
      (unless (funcall pred (clel-first cur))
        (setq result nil))
      (setq cur (clel-rest cur)))
    result))

(defun clel-some (pred coll)
  "Return the first truthy value of (PRED item) for items in COLL, or nil."
  (let ((cur (clel-seq-force coll))
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

;;; Sequence Generators

(defun clel-range (&rest args)
  "Generate a range of numbers.
\(range) - returns empty list (infinite range not supported)
\(range end) - returns (0 1 ... end-1)
\(range start end) - returns (start start+1 ... end-1)
\(range start end step) - returns (start start+step ... ) up to but not including end"
  (let ((start 0)
        (end nil)
        (step 1))
    (pcase (length args)
      (0 nil)  ; (range) - would be infinite, return empty
      (1 (setq end (car args)))
      (2 (setq start (car args)
               end (cadr args)))
      (_ (setq start (car args)
               end (cadr args)
               step (caddr args))))
    (when end
      (let ((result nil)
            (i start))
        (if (> step 0)
            (while (< i end)
              (push i result)
              (setq i (+ i step)))
          (when (< step 0)
            (while (> i end)
              (push i result)
              (setq i (+ i step)))))
        (nreverse result)))))

(defun clel-repeat (n x)
  "Return a list of N copies of X."
  (let ((result nil))
    (dotimes (_ n)
      (push x result))
    result))

(defun clel-repeatedly (n f)
  "Call F N times with no arguments, returning a list of results."
  (let ((result nil))
    (dotimes (_ n)
      (push (funcall f) result))
    (nreverse result)))

;;; Protocol Support

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
   ((listp value)
    ;; Check for struct types (cl-defstruct creates lists starting with type name)
    (if (and (symbolp (car value))
             (get (car value) 'cl-struct-type))
        (car value)
      'cons))
   ;; For cl-defstruct types, check type-of
   (t (type-of value))))

(defun clel-satisfies-p (protocol-name value)
  "Return t if VALUE satisfies PROTOCOL-NAME."
  (let* ((value-type (clel--type-of value))
         (key (cons protocol-name value-type)))
    (or (gethash key clel--protocol-impl-registry)
        ;; Also check if any cl-defmethod exists for the protocol's methods
        (let ((methods (gethash protocol-name clel--protocol-registry)))
          (and methods
               (cl-some (lambda (method)
                          (and (fboundp method)
                               (cl-generic-p method)))
                        methods))))))

(provide 'clojure-elisp-runtime)
;;; clojure-elisp-runtime.el ends here
