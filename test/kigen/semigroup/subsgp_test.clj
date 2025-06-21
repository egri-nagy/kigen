(ns kigen.semigroup.subsgp-test
  (:require [clojure.test :refer :all]
            [kigen.diagram.transf :as t]
            [kigen.semigroup.sgp :as sgp]
            [kigen.semigroup.subsgp :as subsgp]))

(deftest subsgp-closure-test
  (testing "Testing subsemigroup closure with a single new generator."
    (let [S3 (sgp/sgp-by-gens (t/symmetric-gens 3) t/mul)
          T3 (sgp/sgp-by-gens (t/full-ts-gens 3) t/mul)]
      (is (= (set T3) (set (subsgp/subsgp-closure S3 [0 0 2] t/mul)))))))

(deftest subsgps-test
  (testing "Testing subsemigroup enumeration."
    (let [T3 (sgp/sgp-by-gens (t/full-ts-gens 3) t/mul)]
      (is (= 1299 (count (subsgp/subsgps T3 t/mul)))))))
