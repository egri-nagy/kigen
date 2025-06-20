(ns kigen.subsgp-test
  (:require [clojure.test :refer :all]
            [kigen.diagram.transf :as t]
            [kigen.sgp :as sgp]
            [kigen.subsgp :as subsgp]))

(deftest subsgp-closure-test
  (testing "Testing subsemigroup closure with a single new generator."
    (let [S3 (t/sgp-by-gens (t/symmetric-gens 3))
          T3 (t/sgp-by-gens (t/full-ts-gens 3))]
      (is (= (set T3) (set (subsgp/subsgp-closure S3 [0 0 2] t/mul)))))))

(deftest subsgps-test
  (testing "Testing subsemigroup enumeration."
    (let [T3 (t/sgp-by-gens (t/full-ts-gens 3))]
      (is (= 1299 (count (subsgp/subsgps T3 t/mul)))))))
