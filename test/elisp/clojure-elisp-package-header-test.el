;;; clojure-elisp-package-header-test.el --- Emacs reads the emitted package headers -*- lexical-binding: t; -*-

;;; Commentary:

;; The fixture is compiled by the Makefile from a namespace carrying
;; :elisp/package.  package.el and lisp-mnt are the parsers MELPA and
;; `package-install-file' use, so they are the oracle here.

;;; Code:

(require 'ert)
(require 'package)
(require 'lisp-mnt)

(defconst clel-package-header-test--fixture
  (expand-file-name "fixtures/packaged.el"
                    (file-name-directory (or load-file-name buffer-file-name))))

(defmacro clel-package-header-test--in-fixture (&rest body)
  "Run BODY in a buffer visiting the compiled fixture."
  (declare (indent 0))
  `(with-temp-buffer
     (insert-file-contents clel-package-header-test--fixture)
     (emacs-lisp-mode)
     ,@body))

(ert-deftest clel-package-header-parsed-by-package-el ()
  (clel-package-header-test--in-fixture
    (let ((desc (package-buffer-info)))
      (should (eq (package-desc-name desc) 'packaged))
      (should (equal (package-desc-version desc) '(0 1 0)))
      (should (equal (package-desc-summary desc) "A packaged fixture"))
      (should (assq 'emacs (package-desc-reqs desc)))
      (should (assq 'clel (package-desc-reqs desc))))))

(ert-deftest clel-package-header-parsed-by-lisp-mnt ()
  (clel-package-header-test--in-fixture
    (should (equal (lm-summary) "A packaged fixture"))
    (should (equal (lm-homepage) "https://example.org/packaged"))
    (should (string-match-p "Commentary body" (lm-commentary)))
    (should (lm-code-start))))

(provide 'clojure-elisp-package-header-test)
;;; clojure-elisp-package-header-test.el ends here
