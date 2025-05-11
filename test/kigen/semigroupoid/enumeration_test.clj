(ns kigen.semigroupoid.enumeration-test
  (:require [clojure.test :refer [deftest testing is]]
            [clojure.core.logic :as l]
            [clojure.core.logic.fd :as fd]
            [kigen.logic :refer [ntho]]
            [clojure.math.combinatorics :refer [selections]]
            [kigen.multab-morphism :as mtm]
            [kigen.semigroupoid.enumeration :refer [semigroups-order-n]]))

(def sgps-order-3
  (reduce
   (fn [sgps S]
     (if (some (fn [T]
                 (or (first (mtm/isomorphisms S T))
                     (first (mtm/isomorphisms (apply mapv vector S) T))))
               sgps)
       sgps
       (conj sgps S)))
   #{}
   (semigroups-order-n 3)))

(deftest sgp-enum-test
  (testing "Order 3 semigroups up iso and anti-iso morphisms."
    (is (= 18 (count sgps-order-3)))))