(ns clojure-elisp.kaocha-instrument
  "Kaocha plugin that enables Malli boundary-contract instrumentation for the
   whole test run.

   Kaocha runs lifecycle hooks off registered PLUGINS (not top-level tests.edn
   keys), so this is a real `defplugin`. Its post-load hook fires once all test
   and source namespaces are loaded — i.e. after every `m/=>` contract is
   registered — and turns on instrumentation of core + compile + project. Every
   existing test then doubles as contract coverage: a contract violation
   anywhere fails the suite.

   Registered via `:plugins [… :clojure-elisp.kaocha-instrument/instrument]`."
  (:require [kaocha.plugin :refer [defplugin]]
            [clojure-elisp.core :as clel]))

(defplugin clojure-elisp.kaocha-instrument/instrument
  (post-load [test-plan]
    (clel/instrument!)
    test-plan)
  ;; Tear down so instrumentation (which mutates var roots process-globally)
  ;; does not leak past the run in a persistent REPL. A one-shot CI JVM exits
  ;; anyway, so this only matters for `(clojure.test/run-tests)`-style reuse.
  (post-run [result]
    (clel/unstrument!)
    result))
