(ns kigen.transf-test
  (:require [clojure.test :refer :all]
            [kigen.core :refer :all]))

(deftest pbr-full-ts-test
  (testing "Testing full transformation semigroups."
    (let [pow #(reduce * 1 (repeat %2 %1))]
      (doseq [n [2 3 4 5]]
        (is (= (pow n n)
               (count(kigen.sgp/sgp-by-gens
                      (kigen.transf/full-ts-gens n)
                      kigen.pbr/mul))))))))
