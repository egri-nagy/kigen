(ns kigen.semigroupoid.homomorphism-test
  (:require [clojure.test :refer [deftest testing is]]
            [clojure.core.logic :as l]
            [clojure.math.combinatorics :refer [selections]]
            [kigen.semigroupoid.homomorphism :refer [compfo
                                                     homomorphism?
                                                     homomorphism2?
                                                     homomorphisms]]))

(def S
  [[0 1 2 3 4 nil]
   [1 0 2 4 3 nil]
   [nil nil nil nil nil 2]
   [nil nil nil nil nil 2]
   [nil nil nil nil nil 2]
   [nil nil nil nil nil 5]])

(def T
  [[0 0 3 3 4 5 6 nil nil nil nil nil nil nil nil]
   [0 1 2 3 4 5 6 nil nil nil nil nil nil nil nil]
   [0 2 1 3 4 5 6 nil nil nil nil nil nil nil nil]
   [0 3 0 3 4 5 6 nil nil nil nil nil nil nil nil]
   [nil nil nil nil nil nil nil 0 3 4 4 5 5 6 6]
   [nil nil nil nil nil nil nil 0 3 4 5 5 6 4 6]
   [nil nil nil nil nil nil nil 0 3 4 6 5 4 5 6]
   [7 7 8 8 9 11 14 nil nil nil nil nil nil nil nil]
   [7 8 7 8 9 11 14 nil nil nil nil nil nil nil nil]
   [nil nil nil nil nil nil nil 7 8 9 9 11 11 14 14]
   [nil nil nil nil nil nil nil 7 8 9 10 11 12 13 14]
   [nil nil nil nil nil nil nil 7 8 9 11 11 14 9 14]
   [nil nil nil nil nil nil nil 7 8 9 12 11 13 10 14]
   [nil nil nil nil nil nil nil 7 8 9 13 11 10 12 14]
   [nil nil nil nil nil nil nil 7 8 9 14 11 9 11 14]])

(deftest compfo-test
  (testing "Testing the goal compfo"
     ;what pairs produce 3?
    (is (= [[0 3] [1 4]]
           (l/run*
            [p q]
            (compfo S p q 3))))))


(count (homomorphisms S T))

;; (group-by (fn [[a b]] (compf S a b))
;;           (composable-pairs S))



;; (count (homomorphisms S))

;; ;quick to get the homomorphisms
(count (filter (partial homomorphism2? S T)
                (map vec (selections (range 15) 6))))

;; (homomorphism? S (vec (repeat 6 0)))
