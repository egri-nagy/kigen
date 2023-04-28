(ns kigen.transducer-test
  (:require [clojure.test :refer :all]
            [kigen.transducer.common :refer :all]
            [kigen.transducer.flexible :refer :all]
            [kigen.transducer.fixed :refer :all]
            [kigen.transducer.common :refer :all]
            [clojure.math.combinatorics :as combo]))

;;hardcoded test cases
(deftest test-minimal-example
  (testing "Testing an edge-case automaton with one state one input symbol."
    (is (= [[0]]
           (:delta (first (fixed-output-transducer [[[0] 0]] 1)))))))

(deftest test-two-state-reset-automaton
  (testing " "
    (is (= (:delta (first (fixed-output-transducer [[[0] 0]
                                                    [[1] 1]
                                                    [[1 1] 1]
                                                    [[1 0] 0]] 2)))
           [[0 0]
            [1 1]]))))


(deftest test-flip-flop
  (testing "Testing the construction of the flip-flop. "
    (is (= (:delta (first (fixed-output-transducer [[[0] 0]
                                                    [[1] 0]
                                                    [[2] 1]
                                                    [[2 0] 1]
                                                    [[2 1] 0]
                                                    [[2 2] 1]] 2)))
           [[0 1]
            [0 0]
            [1 1]]))))

(deftest test-parity-check
  (testing "Testing the construction of parity checker. "
    (is (=
         ;two states enough since no memory is needed
         (:delta (first (fixed-output-transducer [[[0 0 0] 0]
                                                  [[0 0 1] 1]
                                                  [[0 1 0] 1]
                                                  [[0 1 1] 0]
                                                  [[1 0 0] 1]
                                                  [[1 0 1] 0]
                                                  [[1 1 0] 0]
                                                  [[1 1 1] 1]] 2)))
         ;;same for more bits
         (:delta (first (fixed-output-transducer
                         (map (fn [l]
                                [l (if (even? (count (filter #{1} l)))
                                     0
                                     1)])
                              (combo/selections [0 1] 6)) 2)))
         [[0 1] ; 0 is the identity, symbol 1 flips the state
          [1 0]]))))

;;systematic tests
(deftest small-fixed-transducer-test
  (testing "Testing a small example."
    (let [io-pairs [[[0 1 2] 0]
                    [[2] 1]
                    [[1 0 2 1 1 2 0 1 1 0 2] 2]
                    [[1 1 0] 0]]
          solution (first (fixed-output-transducer io-pairs 4))]
      (is
       (check io-pairs solution))
      (is (= '("0 ·0 0 ·1 1 ·2 0 = 0 ✔"
               "0 ·2 1 = 1 ✔"
               "0 ·1 1 ·0 1 ·2 0 ·1 1 ·1 3 ·2 1 ·0 1 ·1 3 ·1 2 ·0 2 ·2 2 = 2 ✔"
               "0 ·1 1 ·1 3 ·0 0 = 0 ✔")
           (trajectories io-pairs solution))))))

(deftest small-transducer-test
  (testing "Testing a small example with varied types."
    (let [io-pairs [[[:zero 1 []] :foo]
                    [[[]] :bar]
                    [[1 :zero [] 1 1 [] :zero 1 1 :zero []] "foobar"]
                    [[1 1 :zero] :bar]]
          solution (first (transducer io-pairs 3))]
      (is
       (check io-pairs solution))
      (is (= '("0 ·:zero 0 ·1 1 ·[] 0 = :foo ✔"
               "0 ·[] 2 = :bar ✔"
               "0 ·1 1 ·:zero 0 ·[] 2 ·1 0 ·1 1 ·[] 0 ·:zero 0 ·1 1 ·1 2 ·:zero 2 ·[] 1 = foobar ✔"
               "0 ·1 1 ·1 2 ·:zero 2 = :bar ✔")
           (trajectories io-pairs solution))))))