(ns clojure-elisp.nrepl-session-key-test
  "The CLJEL session registry is keyed by session-id strings and forgets a
   session when nREPL closes it.

   Behind nREPL's session middleware a message's :session is the session
   ATOM. Keying the registry by that atom leaked one atom, with its whole
   dynamic-binding map, per connect/disconnect cycle: only an explicit
   cljel-stop ever removed it, and nothing in the set could be matched
   against nREPL's own session ids."
  (:require [clojure.test :refer [deftest is testing]]
            [hive-test.trifecta :refer [deftrifecta]]
            [hive-test.mutation.combinators :as mutc]
            [clojure-elisp.nrepl-kernel :as kernel]))

(defn- session-atom
  "A stand-in for an nREPL session: an atom whose meta carries :id."
  [id]
  (atom {} :meta {:id id}))

(defn lifecycle
  "Run ops against one session and report what the registry holds after
   each. Each request is [op session-shape]; the session shape is :atom or
   :string, both naming the id \"sk-1\". The registry is restored afterwards."
  [requests]
  (let [saved @kernel/cljel-sessions]
    (try
      (reset! kernel/cljel-sessions #{})
      (mapv (fn [[op shape]]
              (let [session (if (= :atom shape) (session-atom "sk-1") "sk-1")]
                (kernel/handle-op {:op op :session session})
                {:op       op
                 :active?  (kernel/cljel-active? "sk-1")
                 :registry (vec (sort @kernel/cljel-sessions))}))
            requests)
      (finally
        (reset! kernel/cljel-sessions saved)))))

(deftrifecta session-lifecycle
  #'lifecycle
  {:golden-path "test/golden/nrepl-session-key.edn"
   :cases       {:start-close-atom   [["cljel-start" :atom] ["close" :atom]]
                 :start-close-string [["cljel-start" :string] ["close" :string]]
                 :start-stop-atom    [["cljel-start" :atom] ["cljel-stop" :atom]]
                 :close-unknown      [["close" :atom]]}
   :mutations   [(mutc/always [])
                 (mutc/always [{:op "cljel-start" :active? true :registry ["sk-1"]}])]})

(deftest session-id-test
  (testing "an nREPL session atom yields the id in its meta"
    (is (= "abc" (kernel/session-id (session-atom "abc")))))
  (testing "a session-id string is already the key"
    (is (= "abc" (kernel/session-id "abc"))))
  (testing "no session, no key"
    (is (nil? (kernel/session-id nil)))))

(deftest registry-holds-strings-test
  (let [saved @kernel/cljel-sessions]
    (try
      (reset! kernel/cljel-sessions #{})
      (kernel/handle-op {:op "cljel-start" :session (session-atom "sk-2")})
      (is (= #{"sk-2"} @kernel/cljel-sessions))
      (is (every? string? @kernel/cljel-sessions))
      (testing "an eval on the same atom session is intercepted"
        (is (some? (kernel/handle-op {:op "eval" :code "(+ 1 2)"
                                      :session (session-atom "sk-2")}))))
      (testing "close is not answered by the kernel, so the transport still answers it"
        (is (nil? (kernel/handle-op {:op "close" :session (session-atom "sk-2")}))))
      (is (empty? @kernel/cljel-sessions))
      (finally
        (reset! kernel/cljel-sessions saved)))))
