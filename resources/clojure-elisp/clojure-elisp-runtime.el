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

(defun clel-atom (val)
  "Create an atom with initial value VAL."
  (cons 'clel-atom val))

(defun clel-deref (atom)
  "Get the value of ATOM."
  (cdr atom))

(defun clel-reset! (atom val)
  "Reset ATOM to VAL."
  (setcdr atom val) val)

(defun clel-swap! (atom fn &rest args)
  "Swap ATOM by applying FN to current value and ARGS."
  (clel-reset! atom (apply fn (clel-deref atom) args)))

(provide 'clojure-elisp-runtime)
;;; clojure-elisp-runtime.el ends here
