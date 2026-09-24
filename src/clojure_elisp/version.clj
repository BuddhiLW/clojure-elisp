(ns clojure-elisp.version
  "Version facts the compiler projects into the code it emits.

   The project VERSION file is the source of truth for the artifact version;
   `clojure-elisp.project/read-version` reads it through the fs port and stamps
   the runtime .el header and its `clel-runtime-version` constant.

   `minimum-runtime-version` is a different fact, and deliberately not derived
   from VERSION: it names the oldest runtime that can correctly run emitted
   code, which changes only when the runtime contract does.")

(def runtime-feature
  "The runtime's package name, file name (clel.el) and Emacs feature. Every
   compiled file requires it, and a package's Package-Requires names it.

   MELPA requires every definition in a package to start with the package's
   name, and the runtime's definitions all start with clel, so the package is
   clel. Before 0.8.0 it was clojure-elisp-runtime, and package-lint reported
   186 of its 188 definitions."
  'clel)

(def minimum-runtime-version
  "Oldest runtime release that can correctly run code this compiler emits.
   Every compiled file carries it, so deriving it from VERSION would rewrite
   all output on every patch release and make the guard's message a lie: a
   0.7.2 file does not require a 0.7.2 runtime, it requires whatever runtime
   first provided what it depends on.

   Bump ONLY when emitted output stops working against the previous runtime.

   0.7.2 - eager consumers force lazy seqs. Output emitted from 0.7.2 calls
           clel-count/clel-apply/clel-second/clel-butlast/clel-reverse/
           clel-flatten/clel-remove, none of which exist before it.
   0.8.0 - the runtime is the package clel. Output requires the feature clel,
           which no earlier runtime provides."
  "0.8.0")

(def runtime-version-symbol
  "Elisp constant the runtime defines to announce its own version."
  "clel-runtime-version")

(def runtime-too-old-message
  "Format string of the error the guard signals. Every compiled file carries
   it, and checkdoc (which MELPA's melpazoid runs) flags an `error' message
   that does not start with a capital letter, so it must start with one."
  (str "Installed " runtime-feature " runtime %s is too old for this file (needs %s)"))

(defn runtime-guard
  "Elisp that loads the runtime and refuses to continue when it is too old.

   `boundp` rather than a bare read: a runtime older than 0.7.2 does not define
   the constant at all, and an unguarded read would signal void-variable, which
   is precisely the undiagnosable failure this guard exists to replace.

   It requires `runtime-feature` alone, not the old feature
   clojure-elisp-runtime as a fallback: every runtime shipped under that name
   is older than `minimum-runtime-version`, so a fallback would only trade
   \"Cannot open load file: clel\" for the too-old error, at the price of a
   soft require in every compiled file."
  []
  (str "(eval-and-compile\n"
       "  (require '" runtime-feature ")\n"
       "  (unless (and (boundp '" runtime-version-symbol ")\n"
       "               (version<= \"" minimum-runtime-version "\" "
       runtime-version-symbol "))\n"
       "    (error \"" runtime-too-old-message "\"\n"
       "           (if (boundp '" runtime-version-symbol ") "
       runtime-version-symbol " \"(pre-" minimum-runtime-version ")\")\n"
       "           \"" minimum-runtime-version "\")))\n"))
