(ns clojure-elisp.regression-alias-resolution-test
  "A name written through a namespace alias, a refer or the elisp/ prefix
   compiles to the same Emacs name wherever it appears: called, passed to a
   higher-order function, #'-quoted, or as a defcustom's default or option."
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.string :as str]
            [clojure-elisp.analyzer :as ana]
            [clojure-elisp.core :as clel]
            [clojure-elisp.emitter :as emit]))

(defn- compile-ns
  "Compile SRC as a file, returning the code after the ;;; Code: marker."
  [src]
  (second (str/split (clel/compile-file-string src) #";;; Code:\n" 2)))

(def ^:private ns-form
  "(ns app.core (:require [app.look :as look] [clojure.string :as str :refer [join]]))\n")

(deftest sharp-quote-resolves-like-a-symbol
  (let [out (compile-ns (str ns-form
                             "(defn- helper [] 1)\n"
                             "(defn f [xs] [(map look/pick xs) #'look/pick #'join #'str/trim"
                             " #'helper #'car #'nil? #'elisp/message])"))]
    (testing "a namespace alias, as in call and higher-order position"
      (is (str/includes? out "(clel-map #'app-look-pick xs)"))
      (is (not (str/includes? out "#'look-pick"))))
    (testing "a refer and an alias of a mapped namespace"
      (is (str/includes? out "#'clel-str-join"))
      (is (str/includes? out "#'clel-str-trim")))
    (testing "an own private function"
      (is (str/includes? out "#'app-core--helper")))
    (testing "Emacs functions, core mappings and the elisp/ prefix"
      (is (str/includes? out "#'car"))
      (is (str/includes? out "#'null"))
      (is (str/includes? out "#'message"))
      (is (not (str/includes? out "elisp-message"))))))

(deftest sharp-quote-names-a-var-not-a-local
  (is (= "(let* ((car 1))\n    #'car)" (-> '(let [car 1] #'car) ana/analyze emit/emit))))

(deftest elisp-prefixed-value-is-the-bare-emacs-name
  (is (= "(clel-map #'car xs)" (clel/emit '(map elisp/car xs))))
  (is (= "(foo buffer-file-name)" (clel/emit '(foo elisp/buffer-file-name)))))

(deftest defcustom-default-and-options-resolve
  (let [out (compile-ns (str ns-form
                             "(defcustom app-rules look/default-rules\n"
                             "  \"Rules.\"\n"
                             "  :type 'sexp :set #'look/set-rules :group 'app)"))]
    (is (str/includes? out "(defcustom app-rules app-look-default-rules"))
    (is (str/includes? out ":set #'app-look-set-rules"))
    (is (str/includes? out ":type 'sexp"))
    (is (str/includes? out ":group 'app"))))

(defn- are-defaults
  "Assert each default emits as the expected Elisp."
  [& expected-default]
  (doseq [[expected default] (partition 2 expected-default)]
    (is (str/starts-with? (clel/emit (list 'defcustom 'x default "Doc."))
                          (str "(defcustom x " expected "\n"))
        (pr-str default))))

(deftest defcustom-data-defaults-emit-as-before
  (are-defaults
   "nil"           nil
   "t"             true
   "30"            30
   "\"prefix-\""   "prefix-"
   ":auto"         :auto
   "'(a b)"        ''(a b)
   "#'ignore"      '(var ignore)))
