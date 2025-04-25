(ns kigen.combinatorics-test
  (:require [clojure.test :refer :all]
            [kigen.combinatorics :refer :all]))

((deftest non-empty-subsets-test
      (testing "Non-empty subsets"
        (is (= 31
               (count (non-empty-subsets (range 5))))))) )

(deftest big-enough-partitions-test
  (testing "big-enough-partitions"
    (is (=  #{'(#{0 1} #{2} #{3})
              '(#{0 2} #{1} #{3})
              '(#{0} #{1 2} #{3})
              '(#{0 3} #{1} #{2})
              '(#{0} #{1 3} #{2})
              '(#{0} #{1} #{3 2})
              '(#{0} #{1} #{2} #{3})}
            (set (big-enough-partitions (range 4) 3))))))
