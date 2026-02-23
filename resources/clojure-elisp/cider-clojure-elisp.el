;;; cider-clojure-elisp.el --- CIDER integration for ClojureElisp -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Pedro G. Branquinho
;; Author: Pedro G. Branquinho <pedrogbranquinho@gmail.com>
;; Maintainer: Pedro G. Branquinho <pedrogbranquinho@gmail.com>
;; URL: https://github.com/BuddhiLW/clojure-elisp
;; Version: 0.3.1
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

(defvar-local cider-cljel-active nil
  "Non-nil when a CLJEL compilation session is active.")

;;; --- Session Management ---

(defun cider-cljel-start ()
  "Start a ClojureElisp compilation session on the current nREPL connection.
Subsequent eval operations will compile ClojureElisp to Elisp
and evaluate the result locally in Emacs."
  (interactive)
  (cider-ensure-connected)
  (cider-nrepl-send-request
   '("op" "cljel-start")
   (lambda (response)
     (let ((value (nrepl-dict-get response "value")))
       (when value
         (setq cider-cljel-active t)
         (message "%s" value))))))

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

(defun cider-cljel--eval-elisp-string (elisp-string)
  "Evaluate ELISP-STRING locally in Emacs and return the result as a string."
  (condition-case err
      (let ((result (eval (car (read-from-string elisp-string)) t)))
        (format "%S" result))
    (error (format "Elisp eval error: %S" err))))

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
       ;; Compiled Elisp received — eval locally
       (compiled
        (let ((result (cider-cljel--eval-elisp-string compiled)))
          (with-current-buffer buffer
            (if (fboundp 'cider--display-interactive-eval-result)
                (cider--display-interactive-eval-result result point)
              (message "=> %s" result)))))
       ;; Compilation error
       (err
        (with-current-buffer buffer
          (message "%s" err)))))))

;;; --- Eval Functions ---

(defun cider-cljel-eval (code)
  "Evaluate ClojureElisp CODE via nREPL and eval the compiled Elisp locally."
  (interactive "sClojureElisp: ")
  (cider-ensure-connected)
  (cider-nrepl-send-request
   (list "op" "eval"
         "code" code
         "ns" "user")
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
