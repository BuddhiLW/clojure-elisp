;;; lint-package.el --- Run package-lint over files named on the command line -*- lexical-binding: t; -*-

;;; Commentary:

;; emacs -Q --batch -l scripts/lint-package.el FILE...
;; Installs package-lint from MELPA into target/elpa on first use and lints
;; each FILE as its own package.  Warnings fail the run, as in MELPA review.

;;; Code:

(require 'package)

(setq package-user-dir (expand-file-name "target/elpa"))
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

(unless (package-installed-p 'package-lint)
  (package-refresh-contents)
  (package-install 'package-lint))

(require 'package-lint)
(package-lint-batch-and-exit)

;;; lint-package.el ends here
