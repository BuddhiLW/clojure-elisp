(ns clojure-elisp.version
  "Version facts the compiler projects into the code it emits.

   The project VERSION file is the source of truth for the artifact version;
   `clojure-elisp.project/read-version` reads it through the fs port and stamps
   the runtime .el header and its `clel-runtime-version` constant.

   `minimum-runtime-version` is a different fact, and deliberately not derived
   from VERSION: it names the oldest runtime that can correctly run emitted
   code, which changes only when the runtime contract does.")

(def minimum-runtime-version
  "Oldest clojure-elisp-runtime release that can correctly run code this
   compiler emits. Every compiled file carries it, so deriving it from VERSION
   would rewrite all output on every patch release and make the guard's message
   a lie: a 0.7.2 file does not require a 0.7.2 runtime, it requires whatever
   runtime first provided what it depends on.

   Bump ONLY when emitted output stops working against the previous runtime.

   0.7.2 - eager consumers force lazy seqs. Output emitted from 0.7.2 calls
           clel-count/clel-apply/clel-second/clel-butlast/clel-reverse/
           clel-flatten/clel-remove, none of which exist before it."
  "0.7.2")

(def runtime-version-symbol
  "Elisp constant the runtime defines to announce its own version."
  "clel-runtime-version")

(defn runtime-guard
  "Elisp that loads the runtime and refuses to continue when it is too old.

   `boundp` rather than a bare read: a runtime older than 0.7.2 does not define
   the constant at all, and an unguarded read would signal void-variable, which
   is precisely the undiagnosable failure this guard exists to replace."
  []
  (str "(eval-and-compile\n"
       "  (require 'clojure-elisp-runtime)\n"
       "  (unless (and (boundp '" runtime-version-symbol ")\n"
       "               (version<= \"" minimum-runtime-version "\" "
       runtime-version-symbol "))\n"
       "    (error \"clojure-elisp-runtime %s is too old for this file (needs %s)\"\n"
       "           (if (boundp '" runtime-version-symbol ") "
       runtime-version-symbol " \"(pre-" minimum-runtime-version ")\")\n"
       "           \"" minimum-runtime-version "\")))\n"))
