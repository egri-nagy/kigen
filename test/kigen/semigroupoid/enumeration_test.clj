(ns kigen.semigroupoid.enumeration-test
  (:require [clojure.test :refer [deftest testing is]]
            [clojure.core.logic :as l]
            [clojure.core.logic.fd :as fd]
            [kigen.logic :refer [ntho]]
            [clojure.math.combinatorics :refer [selections]]
            [kigen.multab-morphism :as mtm]
            [kigen.semigroupoid.enumeration :refer [semigroups-order-n]]
            [kigen.semigroupoid.homomorphism :refer [sgps-up-to-morphisms]]))

(deftest sgp-enum-test
  (testing "Order 3 semigroups up iso and anti-iso morphisms."
    (is (= 18
           (count (sgps-up-to-morphisms (semigroups-order-n 3)))))))