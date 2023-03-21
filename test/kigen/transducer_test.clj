(ns kigen.transducer-test
  (:require [clojure.test :refer :all]
            [kigen.transducer :refer :all]))

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
           [[0 0] [1 1]]))))
