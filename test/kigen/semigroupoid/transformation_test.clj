(ns kigen.semigroupoid.transformation-test
  (:require [clojure.test :refer :all]
            [kigen.semigroupoid.transformation
             :refer [sgpoid-by-gens
                     arrows-by-type
                     symbols-for-elts
                     comptab
                     symbol-comptab
                     graph
                     transitive-closure
                     full-semigroupoid]]))

; example transformation semigroupoid
; Example A.1 in https://arxiv.org/abs/2504.04660
(def dual-mode-counter-gens
  [{:s 0, :t 0, :m [1 0]}
   {:s 1, :t 1, :m [1 2 0]}
   {:s 0, :t 1, :m [0 0]}
   {:s 1, :t 0, :m [0 0 0]}])

(deftest dual-mode-counter-semigroupoid-from-generators
  (testing "Testing the closure algorithm for semigroupoid generators."
    (let [S (sgpoid-by-gens dual-mode-counter-gens)]
      (is (= 15 (count S)))
      (is (= 4 (count (arrows-by-type S)))))))

; Example 3.2
(def two-objs-six-arrows-gens
  [;{:s 0, :t 0, :m [0 1]} ;a
   {:s 0, :t 0, :m [1 0]} ;b
   {:s 0, :t 1, :m [0 0]} ;c
   {:s 0, :t 1, :m [0 1]} ;d
   ;{:s 0, :t 1, :m [1 0]} ;e
   {:s 1, :t 1, :m [0 0]}]) ;f

(deftest Ex3.2-semigroupoid-from-generators
  (testing "Example 3.2"
    (let [S (sgpoid-by-gens two-objs-six-arrows-gens)]
      (is (= 6 (count S)))
      (is (= (symbols-for-elts S) [[\a {:s 0, :t 0, :m [0 1]}]
                                   [\b {:s 0, :t 0, :m [1 0]}]
                                   [\c {:s 0, :t 1, :m [0 0]}]
                                   [\d {:s 0, :t 1, :m [0 1]}]
                                   [\e {:s 0, :t 1, :m [1 0]}]
                                   [\f {:s 1, :t 1, :m [0 0]}]]))
      (is (= (symbol-comptab (comptab S))
             ["abcde."
              "baced."
              ".....c"
              ".....c"
              ".....c"
              ".....f"])))))

(def A2-gens ;communicating vessels
  [{:s 0 :t 0 :m [1 0]}
   {:s 1 :t 1 :m [1 1]}
   {:s 0 :t 1 :m [1 0]}
   {:s 1 :t 0 :m [1 0]}])

(deftest ExA2
  (testing "Communicating Vessels Example A2"
    (let [A2 (sgpoid-by-gens A2-gens)]
      (is (= 16 (count A2)))
      (is (= #{[0 0] [0 1] [1 0] [1 1]}
             (first (distinct (vals (arrows-by-type A2)))))))))

(deftest graph-test
  (testing "Getting the underlying graph"
    (is (= (graph (sgpoid-by-gens [{:s 0 :t 1 :m [0 1]}
                                   {:s 1 :t 2 :m [0 1]}]))
           #{[0 2] [1 2] [0 1]}))))

(deftest transitive-closure-test
  (testing "The transitive closure of directed graphs."
    (is (= (transitive-closure [[0 0] [1 1]])
           #{[0 0] [1 1]}))
    (is (= (transitive-closure [[0 1] [1 0]])
           #{[0 0] [1 0] [1 1] [0 1]}))
    (is (= (transitive-closure [[0 1] [1 2] [2 3]])
           #{[2 3] [1 3] [0 3] [0 2] [1 2] [0 1]}))
    (is (= (transitive-closure [[0 1] [1 2] [2 3] [3 0]])
           #{[2 2] [0 0] [1 0] [2 3] [3 3] [1 1] [3 0] [1 3] [0 3] [0 2]
             [2 0] [3 1] [2 1] [1 2] [3 2] [0 1]}))))

(deftest full-semigroupoid-test
  (testing "Full transformation semigroupoid."
    (is (= 48 (count (full-semigroupoid ;4 + 9 + 27 + 8
                      #{[0 0] [1 0] [1 1] [0 1]}
                      [2 3]))))))