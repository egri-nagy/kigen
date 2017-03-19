(ns kigen.transf-test
  (:require [clojure.test :refer :all]
            [kigen.transf :as transf]
            [kigen.sgp :as sgp]
            [kigen.pbr :as pbr]
            [kigen.conjugacy :as conjugacy]))

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

(deftest transf-bipart-test
  (testing "Testing the binary relation embedding."
    (let [ts [[1] [2 2 2] [5 4 3 2 1] [1 2 3 4] [1 1 4 3]]
          pbrs (map transf/transf->bipart ts)]
      (is (= [1 3 5 4 4] (map transf/transf-degree pbrs)))
      ;(is (every? transf/bipart-transf? pbrs))
      (is (= ts (map transf/bipart->transf pbrs))))))

(deftest pbr-full-ts-test
  (testing "Testing full transformation semigroups."
    (let [pow #(reduce * 1 (repeat %2 %1))]
      (doseq [n [1 2 3 4]]
        (is (= (pow n n)
               (count(sgp/sgp-by-gens (transf/full-ts-gens n) transf/mul))))))))

(deftest pbr-full-ts-test
  (testing "Testing full transformation semigroups."
    (let [pow #(reduce * 1 (repeat %2 %1))]
      (doseq [n [1 2 3 4]]
        (is (= (pow n n)
               (count(sgp/sgp-by-gens (transf/full-ts-gens n) transf/mul))))))))

(deftest pbr-transf-test
  (testing "Testing converting to image list and back."
    (let [pred #(= (transf/->transf (transf/transf-> %)) %)]
      (doseq [n [1 2 3 4]]
        (is (= true
               (every? pred
                       (sgp/sgp-by-gens (transf/full-ts-gens n) transf/mul))))))))

(deftest transf-inverse-test
  (testing "Inverses of bijective transformations in S7."
    (let [S7 (transf/sgp-by-gens (transf/symmetric-gens 7))]
      (is (every? #(and
                    (= [0 1 2 3 4 5] (transf/mul % (transf/inverse %)))
                    (= [0 1 2 3 4 5] (transf/mul (transf/inverse %) %)))
                  S7)))))

(deftest transf-inverse-test
  (testing "Conjugates."
    (is (= [4 3 0 3 1] (transf/conjugate [1 4 3 3 2] [2 0 1 3 4])))))

(deftest transf-conjrep-test
  (testing "Conjugacy class representatives, direct search vs. naive method."
    (let [S5 (transf/sgp-by-gens (transf/symmetric-gens 5))
          T5 (transf/sgp-by-gens (transf/full-ts-gens 5))]
      (is (every? #(and
                    (transf/conjrep %)
                    (conjugacy/conjrep transf/conjugate % S5))
                  T5)))))
