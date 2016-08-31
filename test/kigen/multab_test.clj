(ns kigen.multab-test
  (:require [clojure.test :refer :all]
            [kigen.multab :as mt]
            [kigen.orbit :as o]
            [kigen.sgp :as sgp]
            [kigen.transf :as transf]
            [kigen.pbr :as pbr]))

(deftest test-multab
  (testing "Testing multiplication tables."
    (let [mtT2 (mt/multab
                (sgp/sgp-by-gens (transf/full-ts-gens 2) pbr/mul)
                pbr/mul)]
      (is (= 4 (count mtT2)))
      (is (= 10 (count (o/bfs [#{}] (partial mt/min-extensions mtT2))))))))
