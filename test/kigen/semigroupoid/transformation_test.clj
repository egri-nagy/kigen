(ns kigen.semigroupoid.transformation-test
  (:require [clojure.test :refer :all]
            [kigen.semigroupoid.transformation :refer [sgpoid-by-gens]]))

; example transformation semigroupoid
; Example A.1 in https://arxiv.org/abs/2504.04660
(def dual-mode-counter-gens
  [{:s 0, :t 0, :m [1 0]}
   {:s 1, :t 1, :m [1 2 0]}
   {:s 0, :t 1, :m [0 0]}
   {:s 1, :t 0, :m [0 0 0]}])

(deftest dual-mode-counter-semigroupoid-from-generators
  (testing "Testing the closure algorithm for semigroupoid generators."
    (is (= 15 (count (sgpoid-by-gens dual-mode-counter-gens))))))

; Example 3.2 - original example missing arrow ef
(def two-objs-seven-arrows
  [{:s 0 :t 0 :m [1 0]} ;a
   ;{:s 0 :t 0 :m [0 1]} ;b
   {:s 0 :t 1 :m [0 1]} ;c
   ;{:s 0 :t 1 :m [1 0]} ;d
   {:s 0 :t 1 :m [0 0]} ;e
   {:s 1 :t 1 :m [1 1]} ;f
   ])

(deftest Ex3.2-semigroupoid-from-generators
  (testing "Example 3.2"
    (is (= 7 (count (sgpoid-by-gens two-objs-seven-arrows))))))