(ns kigen.semigroupoid.enumeration-test
  (:require [clojure.test :refer [deftest testing is]]
            [kigen.semigroupoid.enumeration
             :refer [semigroups-order-n
                     associativity?
                     all-composition-tables
                     semigroupoids-order-n
                     find-minimal-type-structure
                     type-degree]]
            [kigen.semigroupoid.homomorphism
             :refer [comptabs-up-to-morphisms]]))

(deftest sgp-enum-test
  (testing "Order 3 semigroups up iso and anti-iso morphisms."
    (is (= 18
           (count (comptabs-up-to-morphisms (semigroups-order-n 3)))))))

(deftest sgpoid-enum-test
  (testing "Size 1 and 2 semigroupoids testing logic versus brute force."
    (is (= (set (filter associativity? (all-composition-tables 1)))
           (set (semigroupoids-order-n 1))))
    (is (= (set (filter associativity? (all-composition-tables 2)))
           (set (semigroupoids-order-n 2))))))

(deftest type-inference
  (testing "Type inference for composition tables."
    (is (empty? (find-minimal-type-structure
                 [[:n 1 :n]
                  [:n :n :n]
                  [:n :n :n]])))
    (is (= 3 (type-degree
              [[0 1 :n]
               [:n :n :n]
               [:n :n :n]])))
    (is (= 3
           (type-degree
            [[:n 2 :n]
             [:n :n :n]
             [:n :n :n]])))
    (is (= 2 (type-degree
              [[:n :n :n]
               [:n :n :n]
               [:n :n :n]])))))