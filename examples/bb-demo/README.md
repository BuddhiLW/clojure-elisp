# bb-demo

ClojureElisp on Babashka. No JVM, no `deps.edn`, no `cider-jack-in`.

## Run it

```bash
cd examples/bb-demo
bb demo
```

```
Compiled src/demo/greeter.cljel -> out/demo-greeter.el (963 chars)
out/clojure-elisp-runtime.el
greet:     Hello, world!
shout:     HELLO, CLJEL!
region:    Hello, alpha!
M-x ready: t
```

That compiled `src/demo/greeter.cljel`, wrote the runtime beside it, loaded
both into a real Emacs, and called the functions. The last line is `commandp`:
`greet-region` is a genuine interactive command, reachable from `M-x`.

## Tasks

| Task | Does |
|---|---|
| `bb compile` | `src/demo/greeter.cljel` to `out/demo-greeter.el` |
| `bb show` | compile to stdout |
| `bb eval-form '<form>'` | compile one form the way `C-c C-c` does |
| `bb runtime` | write `clojure-elisp-runtime.el` into `out/` |
| `bb demo` | all of the above, then run it in Emacs |
| `bb nrepl [port]` | ClojureElisp nREPL server for CIDER, default 7888 |

## Interactive

```bash
bb nrepl
```

```
M-x cider-connect-clj   RET localhost RET 7888 RET
M-x cider-cljel-mode
```

Then `C-c C-c` on a form in `src/demo/greeter.cljel`. It compiles, evaluates
the Elisp in your running Emacs, and the function is immediately callable with
`M-x`. Nothing is written to disk.

`bb eval-form` shows what the server sends back:

```bash
$ bb eval-form '(defn wave [n] (message (shout n)))'
(defun demo-greeter-wave (n)
  (message (demo-greeter-shout n)))
```

Both names carry the `demo-greeter-` prefix from `(ns demo.greeter)`: the one
being defined, and the sibling being called. The whole buffer travels with the
request, so an interactively evaluated form and the compiled file agree.

## Using it in your own project

`bb.edn` here points at this checkout so the demo runs from a fresh clone.
Elsewhere, take the published coordinate:

```clojure
{:deps {io.github.buddhilw/clojure-elisp {:mvn/version "<latest>"}}
 :tasks
 {compile {:requires ([clojure-elisp.core :as clel])
           :task (clel/compile-file "src/my/pkg.cljel" "out/my-pkg.el")}
  nrepl   {:requires ([clel.nrepl-server :as server])
           :task (server/start-server! 7888)}}}
```

`clel/bundle-runtime!` writes `clojure-elisp-runtime.el` out of the dependency
into a directory you choose, so nothing has to name a path into the
ClojureElisp checkout.

## Requirements

Babashka, and Emacs 28.1+ for `bb demo`.
