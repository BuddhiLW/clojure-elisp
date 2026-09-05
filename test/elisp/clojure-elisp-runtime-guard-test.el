;;; clojure-elisp-runtime-guard-test.el --- ERT tests for the version guard -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; The Clojure suite asserts that the guard is PRESENT in emitted output. That
;; a present guard actually refuses to load is a different claim, and only
;; Emacs can settle it. These build three runtimes in a temp directory - the
;; current one, an older versioned one, and a pre-guard one that defines no
;; constant at all - and load the same compiled file against each.
;;
;; The pre-guard case is the one that matters: an unguarded read of a constant
;; that does not exist signals void-variable, which reads as a broken package
;; rather than a version mismatch.
;;
;; Run:
;;   emacs -Q -batch -l ert \
;;         -l test/elisp/clojure-elisp-runtime-guard-test.el \
;;         -f ert-run-tests-batch-and-exit

;;; Code:

(require 'ert)

(defvar clel-guard-test--root
  (expand-file-name "../.." (file-name-directory (or load-file-name buffer-file-name)))
  "Repository root, so the runtime and a compiled fixture can be found.")

(defvar clel-guard-test--runtime
  (expand-file-name "resources/clojure-elisp/clojure-elisp-runtime.el"
                    clel-guard-test--root))

(defvar clel-guard-test--fixture
  (expand-file-name "test/elisp/fixtures/guarded.el" clel-guard-test--root)
  "A real compiled file, written by the compiler via `make test-elisp'.
Generated rather than committed: a hand-written copy of emitter output would
be a second source of truth for the guard's shape, free to drift from the
emitter that actually produces it.")

(defun clel-guard-test--load-against (runtime-version expr)
  "Load the compiled fixture against a runtime stamped RUNTIME-VERSION.
RUNTIME-VERSION nil builds a pre-guard runtime that defines no constant.
EXPR is Elisp evaluated after a successful load; its value is printed.

Runs in a FRESH Emacs: `require' is a no-op once a feature is loaded, and
`clel-runtime-version' would stay bound from an earlier case, so every check
here would silently pass in a shared process.

Returns (EXIT-CODE . OUTPUT). Success is read from the exit code, not from the
output: a batch backtrace echoes the --eval string, so any marker printed on
success also appears in the text of a failure."
  (let* ((dir (make-temp-file "clel-guard" t))
         (runtime (expand-file-name "clojure-elisp-runtime.el" dir))
         (target (expand-file-name "guarded.el" dir)))
    (unwind-protect
        (progn
          (with-temp-buffer
            (insert-file-contents clel-guard-test--runtime)
            (goto-char (point-min))
            (when (re-search-forward
                   "^(defconst clel-runtime-version \"[^\"]+\"" nil t)
              (replace-match
               (if runtime-version
                   (format "(defconst clel-runtime-version \"%s\"" runtime-version)
                 ;; Pre-guard runtime: the constant does not exist at all.
                 "(defconst clel-guard-test--absent nil")
               t t))
            (write-region (point-min) (point-max) runtime nil 'silent))
          (copy-file clel-guard-test--fixture target t)
          (with-temp-buffer
            ;; '(t t) mixes stderr into the buffer: the guard signals, so its
            ;; message arrives on stderr and stdout alone would look silent.
            (let ((exit (call-process
                         (expand-file-name invocation-name invocation-directory)
                         nil '(t t) nil
                         "-Q" "--batch" "-L" dir "-l" target
                         "--eval" (format "(princ (format \"=>%%S\" %s))" expr))))
              (cons exit (buffer-string)))))
      (delete-directory dir t))))

(ert-deftest clel-guard-accepts-a-current-runtime ()
  "A runtime at or above the minimum loads, and the compiled file works."
  (pcase-let ((`(,exit . ,out) (clel-guard-test--load-against "9.9.9" "(guarded-ok)")))
    (should (equal 0 exit))
    (should (string-match-p "=>:ok" out))
    (should-not (string-match-p "too old" out))))

(ert-deftest clel-guard-refuses-an-older-versioned-runtime ()
  "An older runtime is refused, and the message names the version installed."
  (pcase-let ((`(,exit . ,out) (clel-guard-test--load-against "0.7.0" "(guarded-ok)")))
    (should-not (equal 0 exit))
    (should (string-match-p "too old for this file" out))
    (should (string-match-p "0\\.7\\.0" out))))

(ert-deftest clel-guard-refuses-a-pre-guard-runtime-without-void-variable ()
  "A runtime predating the constant is refused by NAME, not by void-variable.
This is the whole point: the boundp check turns an undiagnosable
`void-variable clel-runtime-version' into a sentence saying what is wrong."
  (pcase-let ((`(,exit . ,out) (clel-guard-test--load-against nil "(guarded-ok)")))
    (should-not (equal 0 exit))
    (should (string-match-p "too old for this file" out))
    (should-not (string-match-p "void-variable" out))))

(provide 'clojure-elisp-runtime-guard-test)
;;; clojure-elisp-runtime-guard-test.el ends here
