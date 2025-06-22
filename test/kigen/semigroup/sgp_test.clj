(ns kigen.semigroup.sgp-test
  (:require [clojure.test :refer :all]
            [kigen.diagram.transf :as t]
            [kigen.semigroup.sgp :as sgp]))

(deftest commutativity-test
  (testing "Testing commutative?"
    (is (sgp/commutative? (sgp/sgp-by-gens
                           (t/cyclic-gens 11) t/mul)
                          t/mul))
    (is (not (sgp/commutative? (sgp/sgp-by-gens
                                (t/full-ts-gens 4) t/mul)
                               t/mul)))))
(deftest k-nilpotency-test
  (testing "Testing k-nilpotency."
    (is (sgp/k-nilpotent? 4 (sgp/sgp-by-gens
                             [[0 0 1 2]] t/mul)
                          t/mul))
    (is (not (sgp/k-nilpotent? 3 (sgp/sgp-by-gens
                                  (t/full-ts-gens 3) t/mul)
                               t/mul)))))

(deftest index-period-test
  (let [pow (fn [x n]
              (reduce (fn [prod _] (t/mul prod x))
                      x
                      (range (dec n))))
        check (fn [x]
                (let [[i p] (sgp/index-period x t/mul)]
                  (and (= (pow x i) (pow x (+ i p)))
                       (= (dec (+ i p))
                          (count (sgp/sgp-by-gens [x] t/mul))))))]
      (testing "Testing index-period."
        (is (= [4 1] (sgp/index-period [1 1 3 0 2] t/mul)))
        (is (= [1 1] (sgp/index-period [0 1 2 3 4 5 6 7] t/mul)))
        (is (every? check [[0 0 0]
                           [2 1 3 0]
                           [2 1 2 1 2 1 2]
                           [1 5 4 3 7 8 4 5 6 2 3 4 8]
                           [1 1 1 5 5 5 5 8 8 8 1 1 1 6 7 8 9 4 3 2]])))))
