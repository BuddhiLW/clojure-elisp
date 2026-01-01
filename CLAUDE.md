# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

ClojureElisp is a Clojure dialect that compiles to Emacs Lisp, similar to how ClojureScript targets JavaScript. Write Clojure code in `.cljel` files and compile to `.el` files that run in Emacs.

**Status:** Early development - core compiler infrastructure in progress.

## Development Commands

```bash
# Start REPL with dev dependencies (nrepl, cider-nrepl)
clojure -M:dev

# Run tests (Kaocha)
clojure -M:test

# Build (tools.build)
clojure -T:build
```

## Architecture

```
Reader (Clojure's) → Analyzer (AST + env) → Emitter (codegen) → Elisp (.el)
```

### Compiler Pipeline

1. **analyzer.clj** (`src/clojure_elisp/analyzer.clj`)
   - Transforms Clojure forms into AST nodes
   - Uses dynamic `*env*` for tracking locals and namespace context
   - Handles special forms: `def`, `defn`, `fn`, `let`, `if`, `when`, `cond`, `do`, `ns`, `quote`, `loop`, `recur`
   - `analyze` multimethod dispatches on form type

2. **emitter.clj** (`src/clojure_elisp/emitter.clj`)
   - Transforms AST nodes to Elisp source strings
   - `emit-node` multimethod dispatches on `:op` key
   - `core-fn-mapping`: Maps Clojure core functions to Elisp equivalents (e.g., `first` → `car`, `str` → `clel-str`)
   - `mangle-name`: Converts Clojure symbols to valid Elisp identifiers (`?` → `-p`, `!` → `-bang`)

3. **core.clj** (`src/clojure_elisp/core.clj`)
   - Public API: `emit`, `emit-forms`, `compile-string`, `compile-file`, `compile-ns`
   - Entry point for compilation

4. **Runtime** (`resources/clojure-elisp/clojure-elisp-runtime.el`)
   - Elisp runtime library required by compiled code
   - Provides Clojure-like functions: `clel-conj`, `clel-get`, `clel-assoc`, `clel-str`, `clel-atom`, etc.
   - Requires Emacs 28.1+

### Quick REPL Test

```clojure
(require '[clojure-elisp.core :as clel])

;; Compile a form
(clel/emit '(defn foo [x] (+ x 1)))
;; => "(defun foo (x)\n  (+ x 1))"

;; Compile a file
(clel/compile-file "src/my_package.cljel" "out/my-package.el")
```

## Progress Log

### 2026-01-01: Atom Watch Functions (clel-015)

**Implemented add-watch/remove-watch for atoms via swarm (2 parallel slaves):**
- Updated atom structure: `(list 'clel-atom val watchers-alist)`
- Runtime functions: `clel-add-watch`, `clel-remove-watch`, `clel--notify-watchers`
- Updated `clel-reset!`, `clel-swap!` to notify watchers on state change
- Emitter mappings: `atom`, `deref`, `clojure.core/deref` (for `@`), `reset!`, `swap!`, `add-watch`, `remove-watch`

**Test stats:** 94 tests, 595 assertions, 0 failures

**Files modified:**
- `resources/clojure-elisp/clojure-elisp-runtime.el` - Updated atom structure, added watch functions
- `src/clojure_elisp/emitter.clj` - Added 7 atom-related core-fn-mapping entries
- `test/clojure_elisp/runtime_test.clj` - Added 4 atom watch test functions (28 assertions)

### 2026-01-01: Predicate Functions (clel-014)

**Implemented 11 predicate functions via swarm (3 parallel slaves):**
- Numeric: `zero?`, `pos?`, `neg?`, `even?`, `odd?` → direct Elisp cl-lib mapping
- Boolean: `some?`, `true?`, `false?` → `clel-some-p`, `clel-true-p`, `clel-false-p`
- Collection: `coll?`, `sequential?`, `associative?` → `clel-coll-p`, `clel-sequential-p`, `clel-associative-p`

**Test stats:** 90 tests, 556 assertions, 0 failures

**Files modified:**
- `resources/clojure-elisp/clojure-elisp-runtime.el` - Added 6 runtime functions
- `src/clojure_elisp/emitter.clj` - Added 11 core-fn-mapping entries
- `test/clojure_elisp/runtime_test.clj` - Added numeric, boolean, collection predicate tests

### 2026-01-01: Swarm Batch Implementation

**Implemented via emacs-mcp swarm (3 parallel slaves):**
- Test infrastructure: 71 tests, 395 assertions (Kaocha)
- Destructuring in let: vector `[a b]`, map `{:keys [x]}`, `:as`, `:or`
- Destructuring in fn/defn params: `& rest` → `(&rest args)` with `nthcdr`
- try/catch/finally: `condition-case` + `unwind-protect`
- throw: `(signal 'error ...)` for ex-info, Exception., rethrow
- Multi-arity: `cl-case` dispatch on `(length args)`

**Files modified:**
- `src/clojure_elisp/analyzer.clj` - Added special forms, destructuring expansion
- `src/clojure_elisp/emitter.clj` - Added emit-node methods for new forms
- `test/` - Created analyzer_test.clj, emitter_test.clj, core_test.clj
- `kanban.org` - Task tracking

**Workflow notes added to project memory:**
- Swarm conventions (monitoring, unsticking slaves, broadcasts)
- Session startup/wrap conventions (/catchup, /wrap, git)

## Key Mappings

The `core-fn-mapping` in emitter.clj defines Clojure→Elisp translations:
- Arithmetic: `+`, `-`, `*`, `/`, `inc` → `1+`, `dec` → `1-`
- Collections: `first` → `car`, `rest` → `cdr`, `cons` → `cons`, `count` → `length`
- Runtime functions (prefix `clel-`): `str`, `conj`, `get`, `assoc`, `keys`, `vals`
