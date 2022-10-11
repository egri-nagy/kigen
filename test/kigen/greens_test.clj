(ns kigen.greens-test
  (:require [clojure.test :refer [deftest testing is]]
            [kigen.transf :as t]
            [kigen.sgp :as s]
            [kigen.greens :refer :all]))

(def T3 (s/sgp-by-gens (t/full-ts-gens 3) t/mul))

(deftest test-right-ideal
  (testing "Testing principal right ideals."
    (is (= (principal-right-ideal [1 2 2] T3 t/mul)
           (principal-right-ideal [2 0 0] T3 t/mul)))
    (is (= (principal-right-ideal [1 2 1] T3 t/mul)
           #{[0 1 0] [2 0 2] [1 1 1] [1 2 1] [2 2 2]
             [0 2 0] [2 1 2] [1 0 1] [0 0 0]}))))

(deftest test-left-ideal
  (testing "Testing principal left ideals."
    (is (= (principal-left-ideal [0 2 2] T3 t/mul)
           (principal-left-ideal [2 0 0] T3 t/mul)))
    (is (= (principal-left-ideal [1 2 1] T3 t/mul) 
           #{[1 1 1] [1 2 1] [2 2 2] [2 2 1] [2 1 1]
             [1 1 2] [2 1 2] [1 2 2]}))))

(deftest test-ideal
  (testing "Testing principal  ideals."
    (is (= (count (principal-ideal [1 2 1] T3 t/mul))
           21))))

(deftest test-classes
  (testing "Testing R, L, and D classes."
    (is (= (set (R-classes T3 t/mul))
        #{[[0 1 0] [2 0 2] [1 2 1] [0 2 0] [2 1 2] [1 0 1]]
          [[1 0 2] [0 1 2] [1 2 0] [2 1 0] [2 0 1] [0 2 1]]
          [[0 1 1] [1 0 0] [2 1 1] [2 0 0] [0 2 2] [1 2 2]]
          [[0 0 2] [0 0 1] [2 2 0] [2 2 1] [1 1 0] [1 1 2]]
          [[1 1 1] [2 2 2] [0 0 0]]} ))
    (is (= (set (L-classes T3 t/mul))
           #{[[0 1 0] [0 1 1] [1 0 0] [0 0 1] [1 1 0] [1 0 1]]
             [[1 0 2] [0 1 2] [1 2 0] [2 1 0] [2 0 1] [0 2 1]]
             [[0 0 2] [2 0 2] [2 2 0] [0 2 0] [2 0 0] [0 2 2]]
             [[1 1 1]]
             [[1 2 1] [2 2 1] [2 1 1] [1 1 2] [2 1 2] [1 2 2]]
             [[2 2 2]]
             [[0 0 0]]}))
    (is (= (set(D-classes T3 t/mul))
           #{[[0 1 0] [0 1 1] [0 0 2] [2 0 2] [1 0 0]
              [1 2 1] [0 0 1] [2 2 0] [0 2 0] [2 2 1]
              [2 1 1] [1 1 0] [1 1 2] [2 0 0] [0 2 2]
              [2 1 2] [1 2 2] [1 0 1]]
             [[1 0 2] [0 1 2] [1 2 0] [2 1 0] [2 0 1] [0 2 1]]
             [[1 1 1] [2 2 2] [0 0 0]]}))))