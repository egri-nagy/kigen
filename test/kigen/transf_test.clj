(ns kigen.transf-test
  (:require [clojure.test :refer :all]
            [kigen.transf :as transf]
            [kigen.sgp :as sgp]
            [kigen.pbr :as pbr]))

(deftest pbr-full-ts-test
  (testing "Testing full transformation semigroups."
    (let [pow #(reduce * 1 (repeat %2 %1))]
      (doseq [n [1 2 3 4 5]]
        (is (= (pow n n)
               (count(sgp/sgp-by-gens (transf/full-ts-gens n) pbr/mul))))))))

(deftest pbr-transf-test
  (testing "Testing converting to image list and back."
    (let [pred #(= (transf/transf->pbr (transf/pbr->transf %)) %)]
      (doseq [n [1 2 3 4]]
        (is (= true
               (every? pred
                       (sgp/sgp-by-gens (transf/full-ts-gens n) pbr/mul))))))))
