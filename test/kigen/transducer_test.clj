(ns kigen.transducer-test
  (:require [clojure.test :refer :all]
            [kigen.transducer :refer :all]
            [clojure.math.combinatorics :as combo]))

(deftest test-minimal-example
  (testing "Testing an edge-case automaton with one state one input symbol."
    (is (= [[0]]
           (first (construct-transducer [[[0] 0]] 1))))))

(deftest test-two-state-reset-automaton
  (testing " "
    (is (= (first (construct-transducer [[[0] 0]
                                         [[1] 1]
                                         [[1 1] 1]
                                         [[1 0] 0]] 2))
           [[0 0]
            [1 1]]))))


(deftest test-flip-flop
  (testing "Testing the construction of the flip-flop. "
    (is (= (first (construct-transducer [[[0] 0]
                                         [[1] 0]
                                         [[2] 1]
                                         [[2 0] 1]
                                         [[2 1] 0]
                                         [[2 2] 1]] 2))
           [[0 1]
            [0 0]
            [1 1]]))))

(deftest test-parity-check
  (testing "Testing the construction of parity checker. "
    (is (=
         ;two states enough since no memory is needed
         (first (construct-transducer [[[0 0 0] 0]
                                       [[0 0 1] 1]
                                       [[0 1 0] 1]
                                       [[0 1 1] 0]
                                       [[1 0 0] 1]
                                       [[1 0 1] 0]
                                       [[1 1 0] 0]
                                       [[1 1 1] 1]] 2))
         ;;same for more bits
         (first (construct-transducer
                 (map (fn [l]
                        [l (if (even? (count (filter #{1} l)))
                             0
                             1)])
                      (combo/selections [0 1] 6)) 2))
         [[0 1] ; 0 is the identity, symbol 1 flips the state
          [1 0]]))))