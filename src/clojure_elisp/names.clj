(ns clojure-elisp.names
  "Fresh-name supply for compiler-generated symbols.

   The reader (#() params, foo# auto-gensyms) and destructuring (vec__, map__,
   p__, rest__) need names no user wrote. clojure.core/gensym numbers them
   from a process-wide counter, so the same source compiled twice, or on two
   hosts, gets different names and different output.

   Inside with-fresh-names every generated name is numbered from a counter
   that belongs to that one compilation, starting at 1. Output becomes a pure
   function of the source: the same bytes on the JVM, Babashka and
   ClojureWasm. The names are only ever locals, so restarting the count per
   compilation cannot collide across compilations, and one shared counter keeps
   reader names and destructuring names distinct within one.

   Outside with-fresh-names (a bare analyze or emit call) the supply falls
   back to gensym's global counter, as before.")

(def ^:dynamic *counter*
  "Counter (an atom) for the compilation in progress, or nil outside one."
  nil)

(defn next-id
  "Return the next unique numeric suffix as a string."
  []
  (if-let [counter *counter*]
    (str (swap! counter inc))
    (str (gensym ""))))

(defn fresh-symbol
  "Return a fresh unqualified symbol: prefix followed by the next id."
  [prefix]
  (symbol (str prefix (next-id))))

(defn call-with-fresh-names
  "Call f with a per-compilation name counter bound. A nested call reuses the
   enclosing counter, so one compilation never numbers the same name twice."
  [f]
  (if *counter*
    (f)
    (binding [*counter* (atom 0)]
      (f))))

(defmacro with-fresh-names
  "Evaluate body as one compilation: generated names are numbered from 1."
  [& body]
  `(call-with-fresh-names (fn [] ~@body)))
