(ns kigen.transf-pbr-test
  (:require [clojure.test :refer :all]
            [kigen.transf-pbr :as tpbr]
            [kigen.transf :as t]
            [kigen.sgp :as sgp]
            [kigen.pbr :as pbr]
            [kigen.conjugacy :as conjugacy]))

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
