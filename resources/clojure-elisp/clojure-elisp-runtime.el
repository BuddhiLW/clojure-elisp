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

(provide 'clojure-elisp-runtime)
;;; clojure-elisp-runtime.el ends here
