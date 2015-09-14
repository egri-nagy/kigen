(ns kigen.multab-test
  (:require [clojure.test :refer :all]
            [kigen.multab :as mt]
            [kigen.orbit :as o]
            [kigen.sgp :as sgp]
            [kigen.transf :as transf]
            [kigen.pbr :as pbr]))

(deftest test-multab
  (testing "Testing multiplication tables."
    (let [mtT3 (mt/multab
                (sgp/sgp-by-gens (transf/full-ts-gens 3) pbr/mul)
                pbr/mul)]
      (is (= 27 (count mtT3)))
      (is (= 1299 (count (o/bfs [#{}] (partial mt/min-extensions mtT3))))))))
