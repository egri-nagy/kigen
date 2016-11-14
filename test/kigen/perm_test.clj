(ns kigen.perm-test
  (:require [clojure.test :refer :all]
            [kigen.perm :as perm]
            [kigen.transf :as transf]
            [kigen.sgp :as sgp]
            [kigen.pbr :as pbr]))

(deftest perm-binrel-test
  (testing "Testing the binary relation embedding of permutations."
    (let [ts [[5 4 3 2 1] [2 1 3 4 5]]
          pbrs (map transf/transf->binrel ts)]
      (is (every? #(= 5 %) (map transf/transf-binrel-degree pbrs)))
      (is (every? perm/binrel-perm? pbrs))
      (is (= ts (map transf/binrel->transf pbrs))))))

(deftest perm-conjugate-test
  (testing "Testing permutation conjugation."
    (let [ts [[2 3 4 5 1] [2 1 3 4 5] [3 1 4 5 2]]
          pbrs (map transf/transf->binrel ts)]
      (is (= (ts 2) (transf/binrel->transf (perm/conjugate (first pbrs)
                                                       (second pbrs))))))))
