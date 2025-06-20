(ns kigen.sgp-test
  (:require [clojure.test :refer :all]
            [kigen.diagram.transf :as t]
            [kigen.sgp :as sgp]))

(deftest commutativity-test
  (testing "Testing commutative?"
    (is (sgp/commutative? (t/sgp-by-gens
                           (t/cyclic-gens 11))
                          t/mul))
    (is (not (sgp/commutative? (t/sgp-by-gens
                                (t/full-ts-gens 4))
                               t/mul)))))
(deftest k-nilpotency-test
  (testing "Testing k-nilpotency."
    (is (sgp/k-nilpotent? 4 (t/sgp-by-gens
                             [[0 0 1 2]])
                          t/mul))
    (is (not (sgp/k-nilpotent? 3 (t/sgp-by-gens
                                  (t/full-ts-gens 3))
                               t/mul)))))

(deftest index-period-test
  (let [pow (fn [x n]
              (reduce (fn [prod _] (t/mul prod x))
                      x
                      (range (dec n))))
        check (fn [x]
                (let [[i p] (sgp/index-period x t/mul)]
                  (= (pow x i) (pow x (+ i p)))))]
      (testing "Testing index-period."
        (is (= [4 1] (sgp/index-period [1 1 3 0 2] t/mul)))
        (is (= [1 1] (sgp/index-period [0 1 2 3 4 5 6 7] t/mul)))
        (is (every? check [[0 0 0] [2 1 3 0] [2 1 2 1 2 1 2]])))))
