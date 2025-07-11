(ns kigen.semigroupoid.homomorphism-test
  (:require [clojure.test :refer [deftest testing is]]
            [clojure.core.logic :as l]
            [kigen.semigroupoid.homomorphism :refer [composo
                                                     compose
                                                     composable-pairs
                                                     homomorphism?
                                                     homomorphism?-by-comprel
                                                     isomorphisms
                                                     strict-isomorphisms
                                                     strict-homomorphisms
                                                     homomorphisms]]
            [kigen.semigroupoid.enumeration
             :refer [all-composition-tables]]))

(def S
  [[0 1 2 3 4 :n]
   [1 0 2 4 3 :n]
   [:n :n :n :n :n 2]
   [:n :n :n :n :n 2]
   [:n :n :n :n :n 2]
   [:n :n :n :n :n 5]])

(def T
  [[0 0 3 3 4 5 6 :n :n :n :n :n :n :n :n]
   [0 1 2 3 4 5 6 :n :n :n :n :n :n :n :n]
   [0 2 1 3 4 5 6 :n :n :n :n :n :n :n :n]
   [0 3 0 3 4 5 6 :n :n :n :n :n :n :n :n]
   [:n :n :n :n :n :n :n 0 3 4 4 5 5 6 6]
   [:n :n :n :n :n :n :n 0 3 4 5 5 6 4 6]
   [:n :n :n :n :n :n :n 0 3 4 6 5 4 5 6]
   [7 7 8 8 9 11 14 :n :n :n :n :n :n :n :n]
   [7 8 7 8 9 11 14 :n :n :n :n :n :n :n :n]
   [:n :n :n :n :n :n :n 7 8 9 9 11 11 14 14]
   [:n :n :n :n :n :n :n 7 8 9 10 11 12 13 14]
   [:n :n :n :n :n :n :n 7 8 9 11 11 14 9 14]
   [:n :n :n :n :n :n :n 7 8 9 12 11 13 10 14]
   [:n :n :n :n :n :n :n 7 8 9 13 11 10 12 14]
   [:n :n :n :n :n :n :n 7 8 9 14 11 9 11 14]])

(deftest composo-test
  (testing "Testing the goal composo"
     ;what pairs produce 3?
    (is (= [[0 3] [1 4]]
           (l/run*
            [p q]
            (composo S p q 3))))
    (is (= (set (composable-pairs S))
           (set (l/run*
                 [p q]
                 (l/fresh [r]
                          (l/membero r (range (count S)))
                          (composo S p q r))))))))

(deftest isomorphism-test
  (testing "Testing isomorphisms")
  ;; being isomorphic to should be a symmetric relation
  (is (= (set (filter
               (fn [T] ;; not true for isomorphisms - isomorphism is not symmetric?!?!
                 (not (empty? (strict-isomorphisms T [[0 :n] [:n :n]]))))
               (all-composition-tables 2)))
         (set (filter
               (fn [T]
                 (not (empty? (strict-isomorphisms [[0 :n] [:n :n]] T))))
               (all-composition-tables 2)))))
  (is (= (set (isomorphisms S S))
         #{[0 1 2 3 4 5] [0 1 2 4 3 5]})))

(deftest homomorphism-test
  (testing "Semigroupoid homomorphisms"
    (is (= (set (strict-homomorphisms S S))
           #{[0 0 2 2 2 5] [0 0 2 3 3 5] [0 0 2 4 4 5] [0 1 2 2 2 5] [0 1 2 3 4 5] [0 1 2 4 3 5]}))))