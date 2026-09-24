# MELPA pull request: Add recipe for clel

Recipe file `recipes/clel` in a fork of melpa/melpa:

```elisp
(clel :fetcher github :repo "BuddhiLW/clojure-elisp" :files ("resources/clojure-elisp/clel.el"))
```

`:files` is needed: the repository is the ClojureElisp compiler (a Clojure
project) and the package is its single Emacs Lisp file.

---

### Brief summary of what the package does

clel is the runtime library of ClojureElisp, a compiler from a Clojure dialect
to Emacs Lisp. Code ClojureElisp emits calls clel for Clojure's data model and
core library: maps kept as alists that remember they are maps, sets, lazy
sequences, `map`/`filter`/`reduce`/`partition` and the rest of the sequence
functions, `get-in`/`assoc-in`/`update-in`, atoms and protocols.

clel.el is itself generated: ClojureElisp compiles it from
`resources/clojure-elisp/runtime.cljel`, and the generated file is committed.

Similar packages: dash.el, seq.el and s.el cover parts of the same ground for
hand-written Emacs Lisp. clel follows Clojure's semantics (nil punning, lazy
sequences, keyword-keyed maps) because compiled code depends on them.

### Direct link to the package repository

https://github.com/BuddhiLW/clojure-elisp

### Your association with the package

Maintainer.

### Relevant communications with the upstream package maintainer

**None needed**

### Checklist

- [x] The package is released under a GPL-Compatible Free Software License (MIT)
- [x] I've read CONTRIBUTING.org
- [ ] LLMs were used to generate some of the code, and if so, I've added an `Assisted-by:` line as described in CONTRIBUTING.org
- [x] I understand the package must have been maintained in a public repository for 1 month or more (public since 2026-01-01)
- [x] I've used the latest version of package-lint to check for packaging issues, and addressed its feedback
- [x] My elisp byte-compiles cleanly
- [x] I've used `M-x checkdoc` to check the package's documentation strings
- [x] I've built and installed the package using the instructions in CONTRIBUTING.org

### Local checks behind the checklist

- melpazoid (Docker, `RECIPE=... LOCAL_REPO=... make`): no byte-compile,
  checkdoc, package-lint or load findings; MIT license detected.
- `make recipes/clel` in a melpa/melpa clone builds `clel-<date>.tar`, and
  `package-install` of it into a fresh `package-user-dir` works.

### Before opening the PR

1. Merge the branch carrying clel.el into `main` and push: MELPA builds the
   default branch.
2. Tag `v0.8.0` and publish the release: MELPA Stable builds tags, and the
   latest tag (v0.7.2) predates clel.el.
