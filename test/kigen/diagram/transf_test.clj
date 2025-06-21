(ns kigen.diagram.transf-test
  (:require [clojure.test :refer :all]
            [kigen.sgp :refer [sgp-by-gens]]
            [kigen.diagram.transf :as t]))

(deftest transf-full-ts-test
  (testing "Testing full transformation semigroups."
    (let [pow #(reduce * 1 (repeat %2 %1))]
      (doseq [n [1 2 3 4]]
        (is (= (pow n n)
               (count (sgp-by-gens (t/full-ts-gens n) t/mul))))))))

(deftest transf-pts-test
  (testing "Testing partial transformation monoids."
    (let [pow #(reduce * 1 (repeat %2 %1))]
      (doseq [n [1 2 3 4]]
        (is (= (pow (inc n) n)
               (count (sgp-by-gens (t/pts-gens n) t/mul))))))))


(deftest transf-symmetric-group-test
  (testing "Testing symmetric groups."
    (let [factorial #(reduce * 1 (range 1 (inc %)))]
      (doseq [n [1 2 3 4 5 6]]
        (is (= (factorial n)
               (count (sgp-by-gens (t/symmetric-gens n) t/mul))))))))

(deftest transf-symmetric-inverse-monoid-test
  (testing "Testing symmetric inverse monoids."
    (is (= [2 7 34 209 1546]
           (map #(count (sgp-by-gens (t/sym-inv-gens %) t/mul))
                [1 2 3 4 5])))))

(deftest transf-inverse-test
  (testing "Inverses of bijective transformations in S7."
    (let [S7 (sgp-by-gens (t/symmetric-gens 7) t/mul)
          id7 (t/idmap 7)]
      (is (every? #(and
                    (= id7 (t/mul % (t/inverse %)))
                    (= id7 (t/mul (t/inverse %) %)))
                  S7)))))
