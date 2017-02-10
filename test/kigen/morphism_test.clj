(ns kigen.morphism-test
  (:require [clojure.test :refer :all]
            [kigen.morphism :refer :all]
            [kigen.multab :as mt]
            [kigen.sgp :as sgp]
            [kigen.transf :as transf]))

(deftest test-multab
  (testing "Testing multiplication tables."
    (let [mtT2 (mt/multab
                (sgp/sgp-by-gens (transf/full-ts-gens 2) transf/mul)
                transf/mul)]
      (is (= 120 (count (relmorphisms mtT2 mtT2))))
      (is (= 2 (count (divisions mtT2 mtT2))))
      (is (= 2 (count (isomorphisms mtT2 mtT2))))
      (is (= 7 (count (homomorphisms mtT2 mtT2)))))))
