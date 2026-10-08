(ns clojure-elisp.nrepl-response-ns-test
  "The eval and load-file responses name the real namespace, and a plain
   nREPL client can opt into the compiled Elisp under the standard :value key.

   Subject: handle-op over a request map, so the facets see what a transport
   would send: the compiled Elisp, the :ns, and whether :value is present."
  (:require [clojure.test :refer [deftest is testing]]
            [hive-test.trifecta :refer [deftrifecta]]
            [hive-test.mutation.combinators :as mutc]
            [clojure-elisp.nrepl-kernel :as kernel]))

(def ns-src "(ns my.app)")

(defn response-shape
  "The parts of the eval/load-file responses a client reads: ns, whether
   :value is present and equals the compiled Elisp, and whether Elisp came."
  [request]
  (let [responses (case (:op request)
                    "load-file" (kernel/handle-load-file request)
                    (kernel/handle-eval request))
        payload   (first responses)]
    {:ns            (:ns payload)
     :compiled?     (string? (:cljel-compiled-elisp payload))
     :value-mirror? (and (contains? payload :value)
                         (= (:value payload) (:cljel-compiled-elisp payload)))
     :err?          (contains? payload :err)}))

(deftrifecta response-ns-and-mirror
  #'response-shape
  {:golden-path "test/golden/nrepl-response-ns.edn"
   :cases {:eval-no-context     {:code "(+ 1 2)"}
           :eval-cljel-ns       {:code "(defn f [] 1)" :cljel-ns ns-src}
           :eval-cljel-context  {:code "(defn f [] 1)"
                                 :cljel-context (str ns-src "\n(defn g [] 2)")}
           :eval-mirror-opt-in  {:code "(+ 1 2)" :cljel-mirror-value "true"}
           :eval-mirror-off     {:code "(+ 1 2)" :cljel-mirror-value "false"}
           :load-file-ns        {:op "load-file"
                                 :file (str ns-src "\n(defn f [] 1)")}
           :load-file-no-ns     {:op "load-file" :file "(defn f [] 1)"}
           :load-file-mirror    {:op "load-file"
                                 :file (str ns-src "\n(defn f [] 1)")
                                 :cljel-mirror-value true}
           :eval-compile-error  {:code "(defn broken [" :cljel-ns ns-src}}
   :mutations [(mutc/always {:ns "user" :compiled? true
                             :value-mirror? false :err? false})
               (mutc/echo-arg)]})

(deftest source-ns-name-test
  (testing "the leading ns form names the namespace"
    (is (= "my.app" (kernel/source-ns-name "(ns my.app)\n(defn f [] 1)"))))
  (testing "no source, blank source, ns-less or unreadable source fall back"
    (is (= kernel/default-ns (kernel/source-ns-name nil)))
    (is (= kernel/default-ns (kernel/source-ns-name "   ")))
    (is (= kernel/default-ns (kernel/source-ns-name "(defn f [] 1)")))
    (is (= kernel/default-ns (kernel/source-ns-name "(ns broken")))))

(deftest default-response-keeps-cider-shape-test
  (testing "without the opt-in, :value stays absent so CIDER does not render it"
    (let [[payload done] (kernel/handle-eval {:code "(+ 1 2)"})]
      (is (string? (:cljel-compiled-elisp payload)))
      (is (not (contains? payload :value)))
      (is (= ["done"] (:status done))))))
