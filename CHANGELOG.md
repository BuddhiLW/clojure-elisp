# Changelog

All notable changes to ClojureElisp are documented here.

Format follows [Keep a Changelog](https://keepachangelog.com/en/1.1.0/);
versioning follows [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

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

[0.7.0]: https://github.com/BuddhiLW/clojure-elisp/releases/tag/v0.7.0
[0.6.2]: https://github.com/BuddhiLW/clojure-elisp/releases/tag/v0.6.2
[0.6.1]: https://github.com/BuddhiLW/clojure-elisp/releases/tag/v0.6.1
