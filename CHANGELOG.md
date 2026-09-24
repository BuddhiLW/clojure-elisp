# Changelog

All notable changes to ClojureElisp are documented here.

Format follows [Keep a Changelog](https://keepachangelog.com/en/1.1.0/);
versioning follows [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

### Changed

- **Breaking: the runtime is the Emacs package `clel`.** The library is
  `resources/clojure-elisp/clel.el`, provides the feature `clel` and is the
  package `clel`; it was `clojure-elisp-runtime`. MELPA requires every
  definition in a package to start with the package's name, and the runtime's
  all start with `clel`: under the old name package-lint reported 186 of its
  188 definitions. What this means for you:
  - **Compiled output requires `clel`.** The guard in every compiled file now
    reads `(require 'clel)`, and a package's `Package-Requires` names
    `(clel "0.8.0")`. Put `clel.el` on `load-path` in place of
    `clojure-elisp-runtime.el`, and recompile code compiled by an earlier
    version, which still requires `clojure-elisp-runtime`.
  - **`minimum-runtime-version` is 0.8.0**, and so is VERSION: code that
    requires `clel` cannot run on a runtime that does not provide it, and
    none before 0.8.0 does. The guard does not fall back to the old feature:
    every runtime shipped under that name is older than 0.8.0, so a fallback
    would only change which error you see.
  - **Paths:** `bundle-runtime!` (and `clel.edn`'s `:runtime :bundled`)
    writes `clel.el`; `cider-cljel-runtime-file` names `clel.el`; build
    scripts that copy `resources/clojure-elisp/clojure-elisp-runtime.el` must
    copy `clel.el`. `make runtime` regenerates it from `runtime.cljel`.

### Added

- **Package library headers from ns metadata.** A namespace whose attr-map
  carries `:elisp/package` compiles to a file package.el, lisp-mnt and MELPA
  can read:

  ```clojure
  (ns my.pkg
    "One-line summary.

     Commentary paragraphs."
    {:elisp/package {:author "Jane Doe <jane@example.org>"
                     :url "https://example.org/my-pkg"
                     :version "0.1.0"
                     :keywords ["convenience"]
                     :license "GPL-3.0-or-later"}})
  ```

  The docstring's first line is the summary and the rest is `;;; Commentary:`
  (or pass `:commentary`). `Package-Requires` always names `emacs` (default
  `"28.1"`) and `clel` (the runtime) at `minimum-runtime-version`, because
  every compiled file loads the runtime; declaring the runtime older than that
  is a compile error rather than a file that refuses to load after install.
  `;;; Code:` now precedes the runtime guard in this mode. Namespaces without
  `:elisp/package` compile byte-for-byte as before. `package-buffer-info` and
  `lm-*` are the oracle in `test/elisp/clojure-elisp-package-header-test.el`.
- **Headers for every file of a multi-file package.** A package can be
  described once, in `clel.edn`:

  ```clojure
  {:source-paths ["src"] :output-dir "."
   :package {:name "tod" :author "..." :url "..." :version "0.1.0"
             :package-requires [[emacs "28.1"]] :keywords ["faces"]
             :license "GPL-3.0-or-later"}}
  ```

  or by the `:elisp/package` of one namespace; `compile-project` (and
  `compile-project-from-config`) then gives every file of the package a
  header. The main file, the one named after the package (`:name`, else the
  namespace declaring `:package-requires`, else the shortest name), gets the
  full header. Every other file named `<package>` or `<package>-*` gets the
  one package-lint and melpazoid want of a secondary file: summary from its
  ns docstring, Copyright/Author, `SPDX-License-Identifier`, a non-empty
  `;;; Commentary:` and `;;; Code:` before the runtime guard, and no
  `Package-Requires`, which package-lint rejects outside the main file. A
  namespace's own `:elisp/package` overrides per file (e.g. `:commentary`).
  Projects that declare no package compile byte-for-byte as before. The
  incremental cache records each file's package map, so a version bump in
  `clel.edn` recompiles the main file.
- **A MELPA gate in `make test-elisp`.** `test/elisp/clojure-elisp-melpa-test.el`
  compiles a three-file package from its `clel.edn` and holds it to what
  MELPA runs: byte-compile with warnings as errors, checkdoc as melpazoid
  configures it, main and secondary headers as package.el and lisp-mnt read
  them, autoloads as loaddefs generates them, and the signatures `help` and
  eldoc show.
- **`;;;###autoload` cookies.** `^:autoload` on the name of a `defn`,
  `define-minor-mode` or `defcustom` (or `{:autoload true}` in a `defn`
  attr-map) puts the cookie on the line before the definition. package-lint
  errors on a global minor mode that is not autoloaded. `defn` now accepts
  Clojure's `(defn name doc? attr-map? ...)` shape; an attr-map used to be
  read as the parameter vector.

### Fixed

- **The runtime guard's error message starts with a capital letter**
  (`Installed clel runtime %s is too old ...`). checkdoc, which
  MELPA's melpazoid runs, reported "Messages should start with a capital
  letter" once in every compiled file.
- **`when-let` / `if-let` emit `when-let*` / `if-let*`.** The unstarred Emacs
  macros are obsolete since Emacs 31.1, so every use drew a byte-compile
  warning, and MELPA asks for a clean byte-compile.
- **`(:require [clojure.string :as str])` no longer emits
  `(require 'clojure-string)`**, which failed with "Cannot open load file".
  `clojure.*` namespaces compile to runtime calls (`str/join` →
  `clel-str-join`), so there is nothing to load; the alias keeps resolving. A
  namespace required with both `:as` and `:refer` is now required once.
- **Compiling the same source twice yields byte-identical output.** Generated
  names came from the JVM-wide gensym counter (destructuring's `p__31976`,
  `#()`'s `p1__N#`, `cond->`'s `G__N`, the global reify counter), so committed
  `.el` files churned on every build. Names are now numbered per top-level
  form (`clojure-elisp.gensym`): `p__1`, `map__3`, `G__1`; editing one form
  does not renumber another. Reify types are numbered per file and carry the
  namespace prefix (`my-pkg--reify-1`). A destructured parameter with `:as`
  takes that name, so `help` and checkdoc see `state`, not `p__1`.
- **Variadic `defn` emits its real arglist**, `(defun f (a &rest more) "doc"
  ...)`, instead of `(&rest clel--args)` plus a `let`. The docstring used to
  land inside that `let`, where it is not a docstring: 24 public runtime
  functions (`clel-map`, `clel-merge`, `clel-apply`, ...) had none, and
  checkdoc demanded that every docstring mention `CLEL--ARGS`. The runtime is
  regenerated and has them back. A multi-arity `defn`, which must dispatch on
  `(&rest clel--args)`, ends its docstring with the signature `help` and eldoc
  show, `\(fn START &optional END)`, and a `;; checkdoc-params:` line exempts
  the compiler's own parameter names (`clel--args`, `p__1`) from checkdoc.
- **Docstrings keep their lines.** A multi-line docstring was emitted on one
  physical line with `\n` escapes, so checkdoc saw its whole text as the
  first line ("First sentence should end with punctuation"). Docstrings are
  now written with real newlines, and a `(` opening a line is written `\(`.
  The indentation Clojure puts on continuation lines (aligned under the
  opening quote) is removed, relative indentation kept: Emacs shows docstring
  lines as written, and checkdoc wants the second line flush left.
  An Elisp-style `(defn f [x] "Doc." body)`, a string opening a body that goes
  on, is taken as the docstring. The regenerated runtime's checkdoc
  diagnostics drop from 36 to 16.
- **An unmapped `clojure.core/NAME` is a compile error** instead of a call to
  `clojure-core-NAME`, a function nothing defines, which failed only when the
  code ran. Such names come mostly from syntax-quote and from macros expanded
  on the JVM (`with-out-str` writes `clojure.core/push-thread-bindings`); the
  error names the symbol and its line. `clojure.core/vector`, which the
  reader writes for a syntax-quoted `[...]`, now resolves to Elisp's
  `vector`, as the bare name always did: a syntax-quoted vector used to
  compile to `(clel-apply #'clojure-core-vector ...)`, a void-function error
  at macro-expansion time. The runtime's `clojure-core-vector` and
  `clojure-core-list` bridge variables are gone. Emitted code named them as
  functions (`#'clojure-core-vector`), which a variable never satisfied, and
  package-lint rejects their names.

## [0.7.2] - 2026-09-05

A runtime correctness release. `map` and `filter` have always returned a lazy
sequence, which is right, but almost nothing forced one. The consequence was
that the most ordinary line of Clojure there is either threw or, worse,
answered wrongly:

```clojure
(reduce + (map inc coll))   ; wrong-type-argument
(apply + (map inc coll))    ; wrong-type-argument
(count (map inc coll))      ; 4, for a three-element sequence
(last (map inc coll))       ; nil
(into [] (map inc coll))    ; the lazy-seq struct itself
```

The two silent answers are the dangerous half: no error, just a wrong number.

None of the 603 tests could see any of it. They assert on emitted STRINGS, and
the emitted string was correct the whole time. The defect lived one layer down,
in what that string does when Emacs runs it. This release adds the tier that
can see it: an ERT suite that loads the runtime and calls it, wired into `make
test-elisp` and into CI.

### Fixed

- **Eager consumers did not force lazy sequences.** Elisp primitives cannot
  force a `clel-lazy-seq`: `length` measures the four-element struct and
  `apply` signals `wrong-type-argument`. The runtime now has one coercion,
  `clel-realize`, and every fn that hands a sequence to a raw primitive goes
  through it first. `count`, `apply`, `second`, `butlast`, `reverse`, `flatten`
  and `remove` gained `clel-` wrappers rather than mapping straight onto Elisp.
- **Walking loops ran one iteration past the end.** `clel-rest` returned an
  unforced thunk as the tail, which is truthy, so every `while` driven by
  `clel-first`/`clel-rest` saw a phantom trailing `nil`. That is why
  `frequencies` reported an extra `(nil . 1)` entry, `every?` answered `nil`
  for a sequence whose elements all satisfy the predicate, and `reduce` threw.
  `clel-rest` now forces one cell.
- **A transducer could be returned where a sequence was expected.** `distinct`,
  `keep`, `dedupe` and `interpose` dispatch on `&optional coll`, which cannot
  tell "no collection given" from "collection is empty". Recursing onto an
  empty tail therefore hit the no-collection arity and returned a transducer in
  the middle of a sequence. They now use the supplied-p idiom `clel-nth`
  already used.
- **`distinct` did not remove distant duplicates.** Its recursion allocated a
  fresh `seen` table per step, so `(distinct [1 2 1])` returned `(1 2 1)`. The
  table is now carried across the whole sequence.
- **Destructuring `:or` overrode a present nil or false.** Clojure applies a
  default only when the key is ABSENT; cljel emitted `(or (get m :x) 5)`, so
  `{:keys [x] :or {x 5}}` over `{:x nil}` bound 5 instead of nil. All four
  binding forms (`:keys`, `:strs`, `:syms` and explicit) now reach `get`'s
  3-arity, and `clel-get` distinguishes an absent key from a falsy value in
  every branch rather than only for hash tables.

  A second bug fell out of the same line: the default was read with `(get
  or-map sym)`, which cannot tell "no default" from "the default is nil or
  false", so `:or {x false}` was silently dropped. It now uses `contains?`.

### Added

- **`test/elisp/clojure-elisp-runtime-test.el`** — 13 ERT tests asserting that
  an eager consumer answers the same for a lazy sequence as for the realized
  list it stands for, that a lazy tail reached through a plain `cons` is still
  forced, and that laziness survives (taking from `cycle` and `iterate` still
  terminates). Against the 0.7.1 runtime, 12 of the 13 fail.
- **`mappings/lazy-seq-consuming-fns`** declares which Clojure fns read a
  sequence that may be lazy, and `validate-tables!` now rejects any of them
  pointed at a raw Elisp primitive. The rule is stated once and enforced
  mechanically, so a future mapping edit cannot quietly reintroduce this.
- **Elisp tests now run in CI.** They previously existed only behind a
  `make` target that CI never invoked.
- **Compiled files refuse a runtime that is too old to run them.** The runtime
  now defines `clel-runtime-version`, stamped from `VERSION` at regen, and
  every emitted file opens with:

  ```elisp
  (eval-and-compile
    (require 'clojure-elisp-runtime)
    (unless (and (boundp 'clel-runtime-version)
                 (version<= "0.7.2" clel-runtime-version))
      (error "clojure-elisp-runtime %s is too old for this file (needs %s)" ...)))
  ```

  This is harmless while the runtime is bundled beside its application, since
  that copy is always the one that compiled the file. It stops being harmless
  the moment the runtime is package managed, which is the MELPA goal: MELPA
  ships HEAD and MELPA Stable ships the latest tag, so a 0.9 file can meet a
  0.7 runtime. Without the guard that surfaces as `void-function clel-count`,
  which reads as a broken package rather than a version mismatch, or worse as
  a silently changed semantic. The `boundp` check matters: a runtime older
  than 0.7.2 does not define the constant at all, and an unguarded read would
  signal `void-variable` instead of saying what is wrong.

  The guard compares against `version/minimum-runtime-version`, a deliberate
  constant, not against the project version. Deriving it from `VERSION` would
  rewrite every compiled file on every patch release and make the message a
  lie: a 0.7.2 file does not need a 0.7.2 runtime, it needs whichever runtime
  first provided what it uses.

- **`sync-version` now covers every `.el` header.** It propagated `/VERSION` to
  the classpath VERSION resource and nothing else, while six locations restate
  the version. `clojure-elisp-mode.el` and `cider-clojure-elisp.el` sat at
  0.5.0 while `VERSION` read 0.6.1 — two releases of drift, invisible because
  no gate looked at them, hand-fixed twice. MELPA Stable reads that header, so
  drift ships the wrong version. `version-consistency-test` now asserts all
  three against `/VERSION`, and a second test fails if a `.el` file appears
  that is in neither list.

### Fixed (tests)

- **`cross-file-warning-test` compiled every `.cljel` in the shared system temp
  directory.** It passed `(.getParent f1)` — that is, `/tmp` — to
  `compile-project`, so it compiled whatever any other process had left there
  and was green or red depending on what else was on the box. It now creates a
  directory it owns. Reproduced by dropping one unparseable `.cljel` in `/tmp`:
  errors before, passes after.

### Changed

- `count`, `apply`, `second`, `butlast`, `reverse`, `flatten` and `remove` now
  emit `clel-` wrappers instead of `length`, `apply`, `cadr`, `butlast`,
  `reverse`, `flatten-tree` and `cl-remove-if`. Emitted output for these fns
  differs from 0.7.1; behaviour only becomes more correct, and `remove` is now
  lazy as in Clojure. Recompile rather than mixing 0.7.1 output with the 0.7.2
  runtime.

## [0.7.1] - 2026-09-05

Follow-up to 0.7.0. The interactive loop had a second half of the parity bug,
and the Babashka story shipped incomplete.

### Fixed

- **Interactive eval emitted unprefixed calls to sibling definitions.** 0.7.0
  fixed the name a form DEFINES; it did not fix the names a form CALLS. The
  analyzer pre-scans the definitions it is handed, so compiling against the
  `(ns ...)` form alone left a sibling call bare:

  ```
  (defn shout [n] (upcase (greet n)))
  => (defun demo-greeter-shout (n) (upcase (greet n)))       ; void-function
  ```

  The client now sends the whole buffer as `cljel-context`, so calls resolve
  the way they do in the compiled file. When the buffer cannot be compiled the
  server falls back to its leading `(ns ...)` form and then to no context, so a
  half-typed form elsewhere does not block evaluating a good one.
- **`bbin install` could not find `clel.main`.** `bbin` resolves the project as
  a `:local/root` dependency, which reads `deps.edn` `:paths`; the Babashka
  entry points were only on `bb.edn`'s. Pre-existing, and it made the
  recommended install route non-functional. `bb` is now on `deps.edn` `:paths`.
- **The published jar carried no Babashka entry points.** `clel.nrepl-server`
  and `clel.main` were absent from the 0.7.0 artifact, so the Clojars
  coordinate could not start the standalone server. The jar now ships `bb`.

### Added

- **`clojure-elisp.core/bundle-runtime!`** writes `clojure-elisp-runtime.el`
  from the classpath into a directory of your choosing. Previously this was
  private and reachable only through `compile-project-from-config`, so a
  consumer had to name a path into the ClojureElisp checkout to get the
  runtime.
- **`examples/bb-demo`**, a complete ClojureElisp project running on Babashka:
  compile, show, eval-form, runtime, nrepl, and a `demo` task that loads the
  output into a real Emacs and calls the functions.
- **`compile-string-in-ns-result`**, the Railway variant, alongside the
  existing `compile-file-string-result`.

### Changed

- **`clojure-elisp.nrepl-kernel` restratified** along Collect / Promote /
  Pipeline / Boundary, and moved onto the project's own error vocabulary
  (`hive-dsl.result` plus the `clojure-elisp.errors` schemas) instead of the
  ad-hoc `{:status :ok}` maps it shipped with. `compile-code` is kept as the
  compatibility surface. The namespace now carries `m/=>` contracts and joins
  the instrumented set, so they are enforced by the suite rather than declared.
- Version headers in the three `.el` files track the release.

### Verification

603 tests, 3042 assertions, 0 failures. The demo was run end to end: compiled
`.cljel`, bundled runtime, loaded both into Emacs 28+, called the functions and
confirmed `commandp` on the interactive one. The standalone server was driven
over a socket from the demo project. Neutralizing the context handling turns
the parity suite red (13 failures, 1 error).

## [0.7.0] - 2026-09-05

The theme of this release is that **the interactive loop is the primary way to
write ClojureElisp**, and the tooling around it is no longer heavier than the
elisp REPL it replaces.

### Why

Public feedback on the project said, in substance: an external transpiler
process is not ergonomic while you are writing elisp functions, unless there is
a way to invoke the transpiler, and that comes with a lot of tooling when elisp
already has a good REPL model.

Half of that was already wrong about ClojureElisp. The nREPL middleware
(`clojure-elisp.nrepl`) and the CIDER minor mode
(`resources/clojure-elisp/cider-clojure-elisp.el`) have shipped for several
releases: `C-c C-c` compiles the form at point and evaluates the resulting Elisp
in the running Emacs, redefining the function in the live image with nothing
written to disk. That is the same loop the critique credits elisp with.

The other half was fair, on three counts, and this release addresses all of
them:

1. The README documented the interactive loop in exactly one table row, with no
   workflow section. A reader could not discover it, so concluding it did not
   exist was reasonable.
2. Setup really was heavy: a JVM, a `.nrepl.edn` edit, `cider-jack-in`, then
   `M-x cider-cljel-start`.
3. The interactive path had defects that made it feel second-class, the worst
   of which silently disagreed with the compiler.

### Added

- **`clel nrepl`, a ClojureElisp nREPL server that needs no JVM.** Starts in
  roughly 275 ms under Babashka. Connect with `M-x cider-connect-clj`; there is
  no `deps.edn` to write, no `.nrepl.edn` middleware entry, and no jack-in.
  Implemented as `clel.nrepl-server`, a bencode socket loop speaking `clone`,
  `close`, `describe`, `ls-sessions`, `eval`, `load-file`, `cljel-start` and
  `cljel-stop`.
- **`clojure-elisp.nrepl-kernel`**, the transport-independent core: session
  registry, compile modes, and op semantics. `handle-op` returns a vector of
  response maps and writes nothing, so a transport only has to merge its
  correlation keys and serialize. Both servers now run this one definition of
  `compile-code`.
- **`compile-string-in-ns`** (in `clojure-elisp.compile`, re-exported from
  `clojure-elisp.core`): compiles forms in the namespace context of an
  `(ns ...)` source string, emitting the forms alone with no file header and no
  `(provide ...)`.
- **`cider-cljel-runtime-file`**, a defcustom pointing at
  `clojure-elisp-runtime.el` for setups where it is not on `load-path`.
- **README "Interactive Development" section**, placed ahead of Features,
  Installation and CLI Usage, with both server routes, the keymap, and a worked
  session that builds an interactive `fence-region` command from the `ns` form
  through `M-x` to `clel compile`. Every code block in it is verified compiler
  output.

### Fixed

- **`C-c C-e` and `C-c C-k` no longer define different functions.** `handle-eval`
  hardcoded `:expr` mode while `handle-load-file` used `:file`, so evaluating
  `(defn greet ...)` inside `(ns my.pkg)` installed `greet` while compiling the
  same buffer installed `my-pkg-greet`. The running image and the compiled
  artifact disagreed about every namespaced definition. The CIDER client now
  sends the buffer's `(ns ...)` form as `cljel-ns` and the kernel compiles in
  that context, so all four paths agree.
- **First interactive evaluation no longer dies on `void-function clel-str`.**
  Expression mode emits no runtime require and `cider-cljel-start` loaded
  nothing, so any first eval touching `str` failed. `cider-cljel-ensure-runtime`
  now loads the runtime from `load-path` or `cider-cljel-runtime-file`, and
  reports clearly when it cannot find it instead of leaving a void-function to
  surface later.
- **Evaluation results appear at point.** The response handler accepted a point
  argument it never used and sent everything to the echo area. It now renders a
  CIDER inline overlay, falling back to `message` where that is unavailable.

### Changed

- **`clojure-elisp.ast/gen-node` resolves `malli.generator` lazily** through
  `requiring-resolve` rather than requiring it at load time. It is used by tests
  only, and requiring it put `clojure.test.check` on the compile path, which no
  lightweight host provides. Pinned by a test asserting no compile-path
  namespace aliases `malli.generator`.
- **`bb.edn` now puts `src` and `resources` on `:paths`** with `hive-dsl` and
  `malli`, so the compiler loads in the Babashka process rather than shelling
  out to a jar.
- **`clojure-elisp.nrepl` is now a transport only.** `wrap-cljel` and the
  `handle-*` functions delegate to the kernel; the public surface
  (`cljel-sessions`, `cljel-active?`, `compile-code`, `handle-eval`,
  `handle-load-file`, `handle-start`, `handle-stop`, `wrap-cljel`) is unchanged.
- **`clojure-elisp-runtime.el` regenerated** from `runtime.cljel` with the
  current emitter. The only differences beyond the version header are
  `(when x y)` forms emitted as the equivalent `(if x y nil)`; behaviour is
  identical. The checked-in file had been generated by an older emitter.
- Version headers in `clojure-elisp-mode.el` and `cider-clojure-elisp.el` were
  stale at 0.5.0 and now track the release.

### Notes

Sessions on the standalone server start with compilation active, because that
server has no Clojure evaluator to fall through to. `M-x cider-cljel-start` is
therefore no longer a required step there, and `cider-cljel-mode` is only about
keybindings. The JVM middleware is unchanged in this respect: a session there
still opts in.

ClojureWasm was evaluated as a faster host, since `cljw` starts in 36 ms and
already ships `cljw nrepl`. The transport is ready for it and needs no changes.
malli is not: `malli.core/-memoize` reaches
`java.util.concurrent.atomic.AtomicReference`, which ClojureWasm does not
provide, and that is malli's own memoization rather than anything ClojureElisp
can route around. Clearing the earlier blockers in that chain is what produced
the `malli.generator` change above, which was worth making on its own.

### Verification

598 tests, 3021 assertions, 0 failures. The standalone server was driven over a
real socket through clone, namespace-aware eval, bare eval, load-file, a
compilation error and describe. Neutralizing `compile-string-in-ns` so it
ignores its namespace argument turns the new suite red (6 failures, 1 error),
so the namespace-parity coverage is not vacuous.

## [0.6.2] - 2026-07-21

### Fixed

- The release job no longer writes the version back to a protected `main`. The
  v0.6.2 release had failed because `git push origin HEAD:main` is rejected by
  branch protection. README install coordinates use a placeholder and the
  Clojars badge carries the current version, guarded by a test that fails if a
  concrete version is ever pinned again.
- `load-file` compiles as a whole file, and every compiled form is evaluated.

### Changed

- Clojure 1.12.4, nREPL 1.7.0, cider-nrepl 0.62.2.
- The release patch auto-bumps so `main` always ships.

## [0.6.1] - 2026-07-11

### Fixed

- The analyzer keeps every subform instead of destructuring it away, closing a
  class of silently dropped forms.
- Pre-grouped `cond` is rejected loudly instead of being mis-compiled.

### Added

- `dev/migrate_cond.clj`, a source rewriter for the elisp-`cond` migration.

## Earlier releases

Releases before 0.6.1 are recorded in the
[GitHub releases](https://github.com/BuddhiLW/clojure-elisp/releases) and in the
Progress Log in `CLAUDE.md`.

[0.7.1]: https://github.com/BuddhiLW/clojure-elisp/releases/tag/v0.7.1
[0.7.0]: https://github.com/BuddhiLW/clojure-elisp/releases/tag/v0.7.0
[0.6.2]: https://github.com/BuddhiLW/clojure-elisp/releases/tag/v0.6.2
[0.6.1]: https://github.com/BuddhiLW/clojure-elisp/releases/tag/v0.6.1
