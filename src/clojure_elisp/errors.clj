(ns clojure-elisp.errors
  "Categorized compiler error types using hive-dsl ADT.

   Provides a closed sum type for all compiler errors, enabling
   exhaustive pattern matching at API boundaries."
  (:require [hive-dsl.adt :as adt]))

(adt/defadt CompileError
  "Categorized compiler errors with structured data."
  [:compile/read-error     {:message string?}]
  [:compile/analysis-error {:message string? :form any?}]
  [:compile/emit-error     {:message string? :op keyword?}]
  [:compile/file-error     {:message string? :path string?}])
