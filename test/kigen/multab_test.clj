(ns kigen.multab-test
  (:require [clojure.test :refer :all]
            [kigen.multab :as mt]
            [kigen.sgp :as sgp]
            [kigen.transf :as transf]
            [kigen.pbr :as pbr]))

(deftest test-multab
  (testing "Testing multiplication tables."
    (let [mtT2 (mt/multab
                (sgp/sgp-by-gens (transf/full-ts-gens 2) transf/mul)
                transf/mul)]
      (is (= 4 (count mtT2)))
      (is (= 10 (count (mt/subsgps mtT2)))))))
