(ns kigen.diagram.transf-pbr-test
  (:require [clojure.test :refer :all]
            [kigen.diagram.transf-pbr :as tpbr]
            [kigen.diagram.transf :as t]
            [kigen.sgp :as sgp]))

(deftest transf-binrel-test
  (testing "Testing the binary relation embedding."
    (let [ts [[1] [2 2 2] [5 4 3 2 1] [1 2 3 4] [1 1 4 3]]
          pbrs (map tpbr/transf->binrel ts)]
      (is (= [1 3 5 4 4] (map tpbr/transf-binrel-degree pbrs)))
      (is (every? tpbr/binrel-transf? pbrs))
      (is (= ts (map tpbr/binrel->transf pbrs))))))

(deftest transf-bipart-test
  (testing "Testing the binary relation embedding."
    (let [ts [[1] [2 2 2] [5 4 3 2 1] [1 2 3 4] [1 1 4 3]]
          pbrs (map tpbr/transf->bipart ts)]
      (is (= [1 3 5 4 4] (map tpbr/transf-degree pbrs)))
      ;(is (every? tpbr/bipart-transf? pbrs))
      (is (= ts (map tpbr/bipart->transf pbrs))))))

(deftest pbr-transf-test
  (testing "Testing converting to image list and back."
    (let [pred #(= (tpbr/->transf (tpbr/transf-> %)) %)]
      (doseq [n [1 2 3 4]]
        (is (= true
               (every? pred
                       (sgp/sgp-by-gens (t/full-ts-gens n) t/mul))))))))

(deftest perm-binrel-test
  (testing "Testing the binary relation embedding of permutations."
    (let [ts [[5 4 3 2 1] [2 1 3 4 5]]
          pbrs (map tpbr/transf->binrel ts)]
      (is (every? #(= 5 %) (map tpbr/transf-binrel-degree pbrs)))
      (is (every? tpbr/binrel-perm? pbrs))
      (is (= ts (map tpbr/binrel->transf pbrs))))))

(deftest perm-conjugate-test
  (testing "Testing permutation conjugation."
    (let [ts [[2 3 4 5 1] [2 1 3 4 5] [3 1 4 5 2]]
          pbrs (map tpbr/transf->bipart ts)]
      (is (= (last pbrs) (tpbr/conjugate (first pbrs)
                                         (second pbrs)))))))