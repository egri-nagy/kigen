(ns kigen.semigroupoid.homomorphism-test
  (:require [clojure.test :refer [deftest testing is]]
            [clojure.core.logic :as l]
            [clojure.core.logic.fd :as fd]
            [kigen.logic :refer [ntho]]
            [clojure.math.combinatorics :refer [selections]]
            [kigen.multab-morphism :as mtm]
            [kigen.semigroupoid.homomorphism :refer [compfo
                                                     compf
                                                     composable-pairs
                                                     homomorphism?
                                                     homomorphism?-by-comprel
                                                     isomorphisms
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
            (compfo S p q 3))))
    (is (= (set (composable-pairs S))
           (set (let [S2 (mapv
                          (partial mapv #({nil (count S)} % %))
                          S)]
                  (l/run*
                         [p q]
                         (l/fresh [r]
                                  (fd/in r (fd/interval 0 (dec (count S2))))
                                  (compfo S2 p q r)))))))))

(l/run*
 [q]
 (l/fresh [a b c]
          (l/everyg
           (fn [r]
             (l/conde
              [(l/nilo r)]
              [(l/== r 1)]))
           [a b c])
          (l/== q [a b c])))

(def S3
  (reduce
   (fn [sgps S]
     (if (some (fn [T]
                 (or (first (mtm/isomorphisms S T))
                     (first (mtm/isomorphisms (apply mapv vector S) T))))
               sgps)
       sgps
       (conj sgps S)))
   #{}
   ))



(def X [[1 2 0] [2 0 1] [0 1 2]])
(def Y [[1 1 1] [0 1 2] [2 1 2]])

(first (homomorphisms Y 
                     Y))

(apply map vector Y)

(count (isomorphisms X Y))

(selections [:A :b :c] 3)

(mtm/isomorphisms X X)

(first #{})

(apply mapv vector [[1 2 ] [3 4]])

;(count (homomorphisms S T))

;; (group-by (fn [[a b]] (compf S a b))
;;           (composable-pairs S))



;; (count (homomorphisms S))

;; ;quick to get the homomorphisms
;(count (filter (partial homomorphism? S T)
;                (map vec (selections (range 15) 6))))

;; (homomorphism? S (vec (repeat 6 0)))
