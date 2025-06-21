(ns kigen.table.multab-test
  (:require [clojure.test :refer :all]
            [kigen.table.multab :as mt]
            [kigen.semigroup.sgp :as sgp]
            [kigen.diagram.transf :as transf]))

(deftest test-multab
  (testing "Testing multiplication tables."
    (let [mtT2 (mt/multab
                (sgp/sgp-by-gens (transf/full-ts-gens 2) transf/mul)
                transf/mul)]
      (is (= 4 (count mtT2)))
      (is (= 10 (count (mt/subsgps mtT2)))))))
