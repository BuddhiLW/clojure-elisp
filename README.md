# ClojureElisp

A Clojure dialect that compiles to Emacs Lisp, similar to ClojureScript for JavaScript.

## Status

🚧 **Early Development** - Core compiler infrastructure in progress.

## Vision

Write Clojure, run in Emacs:

```clojure
;; my-package.cljel
(ns my.package
  (:require [clojure.string :as str]))

(defn greet [name]
  (let [msg (str "Hello, " name "!")]
    (message msg)))

(defn process-buffer []
  (-> (buffer-string)
      str/upper-case
      insert))
```

Compiles to:

```elisp
;;; my-package.el -*- lexical-binding: t; -*-
(require 'clojure-elisp-runtime)

(defun my-package-greet (name)
  (let* ((msg (clel-str "Hello, " name "!")))
    (message msg)))

(defun my-package-process-buffer ()
  (insert (upcase (buffer-string))))

(provide 'my-package)
```

## Architecture

```
┌─────────────┐    ┌──────────────┐    ┌─────────────┐    ┌──────────────┐
│   Reader    │───▶│   Analyzer   │───▶│   Emitter   │───▶│  Elisp Code  │
│ (Clojure's) │    │ (AST + env)  │    │ (codegen)   │    │   (.el)      │
└─────────────┘    └──────────────┘    └─────────────┘    └──────────────┘
```

## Usage

```clojure
(require '[clojure-elisp.core :as clel])

;; Compile a form
(clel/emit '(defn foo [x] (+ x 1)))
;; => "(defun foo (x)\n  (+ x 1))"

;; Compile a file
(clel/compile-file "src/my_package.cljel" "out/my-package.el")
```

## Development

```bash
# Start REPL
clojure -M:dev

# Run tests
clojure -M:test
```

## License

MIT
