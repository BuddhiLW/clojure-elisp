;;; cider-clojure-elisp.el --- CIDER integration for ClojureElisp -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Pedro G. Branquinho
;; Author: Pedro G. Branquinho <pedrogbranquinho@gmail.com>
;; Maintainer: Pedro G. Branquinho <pedrogbranquinho@gmail.com>
;; URL: https://github.com/BuddhiLW/clojure-elisp
;; Version: 0.7.2
;; Package-Requires: ((emacs "28.1") (cider "1.0"))
;; Keywords: languages, lisp, clojure
;; SPDX-License-Identifier: MIT

;;; Commentary:
;;
;; CIDER integration for ClojureElisp (.cljel) files.
;;
;; Connects to an nREPL server with the `wrap-cljel` middleware,
;; sends ClojureElisp forms for compilation, and evaluates the
;; compiled Elisp locally in Emacs.
;;
;; Architecture:
;;   CIDER sends form via nREPL
;;     → wrap-cljel middleware compiles ClojureElisp → Elisp string
;;     → returns to CIDER with :cljel-compiled-elisp key
;;     → this extension evals Elisp locally in Emacs
;;     → displays result
;;
;; Setup:
;;   1. Ensure .nrepl.edn includes the middleware:
;;      {:middleware [clojure-elisp.nrepl/wrap-cljel]}
;;
;;   2. Start nREPL via CIDER:
;;      M-x cider-jack-in (with :dev alias for deps.edn projects)
;;
;;   3. Activate CLJEL session:
;;      M-x cider-cljel-start
;;
;;   4. Evaluate ClojureElisp forms:
;;      C-c C-e  — eval last sexp
;;      C-c C-c  — eval defun at point
;;      C-c C-k  — compile/eval entire buffer

;;; Code:

(require 'cider)
(require 'nrepl-client)

(defgroup cider-clojure-elisp nil
  "CIDER integration for ClojureElisp."
  :group 'cider
  :prefix "cider-cljel-")

(defcustom cider-cljel-runtime-file nil
  "Path to `clojure-elisp-runtime.el', used when it is not on `load-path'.
Compiled ClojureElisp calls runtime functions such as `clel-str', so the
runtime must be loaded before the first evaluation."
  :type '(choice (const :tag "Rely on load-path" nil) file)
  :group 'cider-clojure-elisp)

(defvar-local cider-cljel-active nil
  "Non-nil when a CLJEL compilation session is active.")

;;; --- Runtime ---

(defun cider-cljel-ensure-runtime ()
  "Ensure `clojure-elisp-runtime' is loaded.
Returns non-nil on success.  Tries `load-path' first, then
`cider-cljel-runtime-file'."
  (or (featurep 'clojure-elisp-runtime)
      (require 'clojure-elisp-runtime nil t)
      (and cider-cljel-runtime-file
           (file-readable-p cider-cljel-runtime-file)
           (progn (load cider-cljel-runtime-file nil t)
                  (featurep 'clojure-elisp-runtime)))))

;;; --- Namespace Context ---

(defun cider-cljel-buffer-ns-form ()
  "Return the source text of this buffer's leading (ns ...) form, or nil."
  (save-excursion
    (save-restriction
      (widen)
      (goto-char (point-min))
      (when (re-search-forward "^(ns\\_>" nil t)
        (let ((start (match-beginning 0)))
          (goto-char start)
          (ignore-errors
            (forward-sexp)
            (buffer-substring-no-properties start (point))))))))

(defun cider-cljel-buffer-context ()
  "Return this buffer's whole source, the compilation context for an eval.
Sending the buffer rather than its (ns ...) form alone is what resolves
calls to sibling definitions."
  (save-restriction
    (widen)
    (buffer-substring-no-properties (point-min) (point-max))))

;;; --- Session Management ---

(defun cider-cljel-start ()
  "Start a ClojureElisp compilation session on the current nREPL connection.
Subsequent eval operations will compile ClojureElisp to Elisp
and evaluate the result locally in Emacs."
  (interactive)
  (cider-ensure-connected)
  (unless (cider-cljel-ensure-runtime)
    (message "ClojureElisp: clojure-elisp-runtime.el not found. \
Add it to load-path or set `cider-cljel-runtime-file'; \
compiled code calling clel-str, clel-conj etc. will fail without it."))
  (let ((buf (current-buffer)))
    (cider-nrepl-send-request
     '("op" "cljel-start")
     (lambda (response)
       (let ((value (nrepl-dict-get response "value")))
         (when value
           (with-current-buffer buf
             (setq cider-cljel-active t))
           (message "%s" value)))))))

(defun cider-cljel-stop ()
  "Stop the ClojureElisp compilation session.
Eval operations return to normal Clojure evaluation."
  (interactive)
  (when cider-cljel-active
    (cider-nrepl-send-request
     '("op" "cljel-stop")
     (lambda (response)
       (let ((value (nrepl-dict-get response "value")))
         (when value
           (setq cider-cljel-active nil)
           (message "%s" value)))))))

;;; --- Eval Helpers ---

(defun cider-cljel--read-forms (elisp-string)
  "Read every top-level form in ELISP-STRING and return them in source order.
Signals on malformed input; a trailing run of whitespace or comments is not
malformed and terminates the scan."
  (let ((pos 0)
        (len (length elisp-string))
        (forms nil))
    (condition-case nil
        (while (< pos len)
          (let ((res (read-from-string elisp-string pos)))
            (setq forms (cons (car res) forms))
            (setq pos (cdr res))))
      (end-of-file nil))
    (nreverse forms)))

(defun cider-cljel--eval-elisp-string (elisp-string)
  "Evaluate every top-level form in ELISP-STRING locally in Emacs.
Returns the printed value of the last form as a string, or a description of
the first error raised.  A single-form ELISP-STRING evaluates to that form's
value, so expression-level eval is unchanged."
  (condition-case err
      (let ((result nil))
        (dolist (form (cider-cljel--read-forms elisp-string))
          (setq result (eval form t)))
        (format "%S" result))
    (error (format "Elisp eval error: %S" err))))

(defun cider-cljel--display (buffer point text)
  "Display TEXT for an evaluation in BUFFER at POINT.
Uses a CIDER inline overlay when available, echo area otherwise."
  (with-current-buffer buffer
    (or (and point
             (fboundp 'cider--display-interactive-eval-result)
             (ignore-errors
               (cider--display-interactive-eval-result text point)
               t))
        (message "=> %s" text))))

(defun cider-cljel--make-handler (buffer &optional point)
  "Make a response handler for CLJEL eval results.
BUFFER is the source buffer.  Optional POINT is the source location
for overlay display.

When the response contains compiled Elisp (`:cljel-compiled-elisp'),
evaluates it locally in Emacs and displays the result."
  (lambda (response)
    (let ((compiled (nrepl-dict-get response "cljel-compiled-elisp"))
          (err (nrepl-dict-get response "err")))
      (cond
       ;; Compiled Elisp received, eval locally
       (compiled
        (cider-cljel--display buffer point
                              (cider-cljel--eval-elisp-string compiled)))
       ;; Compilation error
       (err
        (with-current-buffer buffer
          (message "CLJEL error: %s" err)))))))

;;; --- Eval Functions ---

(defun cider-cljel-eval (code)
  "Evaluate ClojureElisp CODE via nREPL and eval the compiled Elisp locally.
The buffer travels with the request as the compilation context, so a
definition evaluated here gets the same Elisp name the compiled buffer would
give it, and its calls to sibling definitions get the same prefix."
  (interactive "sClojureElisp: ")
  (cider-ensure-connected)
  (cider-nrepl-send-request
   (append (list "op" "eval"
                 "code" code
                 "ns" "user")
           (let ((context (cider-cljel-buffer-context)))
             (when (and context (not (string-empty-p context)))
               (list "cljel-context" context))))
   (cider-cljel--make-handler (current-buffer) (point))))

(defun cider-cljel-eval-last-sexp ()
  "Evaluate the ClojureElisp sexp before point."
  (interactive)
  (let ((code (cider-last-sexp)))
    (cider-cljel-eval code)))

(defun cider-cljel-eval-defun-at-point ()
  "Evaluate the top-level ClojureElisp form at point."
  (interactive)
  (let ((code (cider-defun-at-point)))
    (cider-cljel-eval code)))

(defun cider-cljel-load-buffer ()
  "Compile and evaluate the current buffer as ClojureElisp."
  (interactive)
  (cider-ensure-connected)
  (let ((code (buffer-substring-no-properties (point-min) (point-max))))
    (cider-nrepl-send-request
     (list "op" "load-file"
           "file" code
           "file-name" (or (buffer-name) "unknown.cljel")
           "file-path" (or (buffer-file-name) ""))
     (cider-cljel--make-handler (current-buffer)))))

;;; --- Minor Mode ---

(defvar cider-cljel-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-e") #'cider-cljel-eval-last-sexp)
    (define-key map (kbd "C-c C-c") #'cider-cljel-eval-defun-at-point)
    (define-key map (kbd "C-c C-k") #'cider-cljel-load-buffer)
    map)
  "Keymap for `cider-cljel-mode'.")

;;;###autoload
(define-minor-mode cider-cljel-mode
  "Minor mode for ClojureElisp CIDER integration.
Provides keybindings for evaluating ClojureElisp via nREPL
with local Elisp evaluation in Emacs."
  :lighter " CLJEL"
  :keymap cider-cljel-mode-map
  (if cider-cljel-mode
      (cider-cljel-start)
    (cider-cljel-stop)))

(provide 'cider-clojure-elisp)

;;; cider-clojure-elisp.el ends here
