(ns kigen.transf-test
  (:require [clojure.test :refer :all]
            [kigen.transf :as transf]
            [kigen.sgp :as sgp]
            [kigen.pbr :as pbr]))

(deftest transf-singleton-test
  (testing "Testing the singleton predicate."
    (is (= true (transf/singleton? [:x])))
    (is (= false (transf/singleton? [:x :y])))
    (is (= false (transf/singleton? [])))
    (is (= false (transf/singleton? nil)))))

(deftest transf-binrel-test
  (testing "Testing the binary relation embedding."
    (let [ts [[1] [2 2 2] [5 4 3 2 1] [1 2 3 4] [1 1 4 3]]
          pbrs (map transf/transf->binrel ts)]
      (is (= [1 3 5 4 4] (map transf/transf-binrel-degree pbrs)))
      (is (every? transf/binrel-transf? pbrs))
      (is (= ts (map transf/binrel->transf pbrs))))))

(deftest pbr-full-ts-test
  (testing "Testing full transformation semigroups."
    (let [pow #(reduce * 1 (repeat %2 %1))]
      (doseq [n [1 2 3 4]]
        (is (= (pow n n)
               (count(sgp/sgp-by-gens (transf/full-ts-gens n) pbr/mul))))))))


(deftest pbr-full-ts-test
  (testing "Testing full transformation semigroups."
    (let [pow #(reduce * 1 (repeat %2 %1))]
      (doseq [n [1 2 3 4]]
        (is (= (pow n n)
               (count(sgp/sgp-by-gens (transf/full-ts-gens n) pbr/mul))))))))

(deftest pbr-transf-test
  (testing "Testing converting to image list and back."
    (let [pred #(= (transf/transf->bipart (transf/bipart->transf %)) %)]
      (doseq [n [1 2 3 4]]
        (is (= true
               (every? pred
                       (sgp/sgp-by-gens (transf/full-ts-gens n) pbr/mul))))))))
