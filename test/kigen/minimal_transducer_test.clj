(ns kigen.minimal-transducer-test
  (:require [clojure.test :refer [is testing deftest]]
            [kigen.transducer.minimal :refer [minimize-transducer]]
            [kigen.transducer.trie :as trie]
            [kigen.transducer.common :refer [check]]))

;; example off the internet: https://www.geeksforgeeks.org/minimization-of-dfa/
(def A {:delta {\a [1 0 4 4 4 5] \b [2 3 5 5 5 5]}
        :omega [:reject :reject :accept :accept :accept :reject]
        :n 6})

;; the original Hopcroft-Ullman example from 1979
(def HU {:delta {\a [1 6 0 2 7 2 6 6] \b [5 2 2 6 5 6 4 2]}
         :omega [:reject :reject :accept :reject :reject :reject :reject :reject]
         :n 8})

(deftest test-minimize-transducer
  (testing "Testing a trie for retrieving all words stored."
    (is (= (minimize-transducer A)
           {:delta {\a [0 1 2], \b [0 2 0]},
            :omega [:reject :reject :accept],
            :n 3}))
    (is (= (minimize-transducer HU)
           {:delta {\a [4 1 1 0 2], \b [0 4 0 1 3]},
            :omega [:accept :reject :reject :reject :reject],
            :n 5}))))

(def asbs [["" :mixed]
           ["aa" :as]
           ["bb" :bs]
           ["ab" :mixed]
           ["ba" :mixed]
           ["bbab" :mixed]
           ["aaaa" :as]
           ["b" :bs]
           ["bbbbba" :mixed]
           ["aaaaaaa" :as]
           ["ababababab" :mixed]])

(deftest test-minimize-trie-transducer
  (testing "Testing minimizing a trie-transducer."
    (let [T (trie/transducer asbs)
          minT (minimize-transducer T)]
      (is (> (:n T) (:n minT)))
      (is (check asbs minT)))))