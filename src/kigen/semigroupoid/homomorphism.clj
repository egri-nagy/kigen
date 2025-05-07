(ns kigen.semigroupoid.homomorphism
  "Enumearing all homomorphisms of a semigroupoid using relational
   programming."
  (:require [clojure.core.logic :as l]
            [clojure.core.logic.fd :as fd]
            [kigen.logic :refer [ntho]]
            [clojure.math.combinatorics :refer [selections]]))

(defn compf
  "Composition function for the given composition table S."
  [S a b]
  (nth (nth S a) b))

(defn compfo
  "This goal succeds if a composed with b is ab in the composition table S."
  [S a b ab]
  (l/fresh
   [row]
   (ntho S a row) ;the row of a
   (ntho row b ab))) ;the bth entry in that row should be ab

(defn composable-pairs
  "All the composable pairs of elements of semigroupoid S given as a composition
   table."
  [S]
  (let [n (count S)]
    (for [a (range n)
          b (range n)
          :when (compf S a b)]
      [a b])))

(defn homomorphic?
  "ab - an element of S
   pairs - all the a,b pairs such that ab=ab"
  [S phi ab pairs]
  (every?
   (fn [[a b]] (= (phi ab) (compf S (phi a) (phi b))))
   pairs))

(defn homomorphico
  "Goal succeeds if all pairs compose to ab in S."
  [S ab pairs]
  (l/everyg (fn [[a b]]
              (compfo S a b ab))
            pairs))

(defn homomorphism?
  "another test for homomorphisms, trying to reformulate constrains"
  [S phi]
  (let [ab2factors (group-by (fn [[a b]] (compf S a b))
                             (composable-pairs S))]
    (every? (fn [[ab pairs]]
              (homomorphic? S phi ab pairs))
            ab2factors)))

(defn homomorphism2?
  "S is a composition table of a semigroupoid
   phi is a vector representing the homomorphism
   just for checking how to do it functionally, before realtionally"
  [S phi]
  (every? (fn [[a b]]
            (= (phi (compf S a b))
               (compf S (phi a) (phi b))))
          (composable-pairs S)))

(defn homomorphisms
  "Logic search for all homomoprhisms of semigroupoid S given as a composition
   table."
  [S] ;given as a composition table
  (let [n (count S)
        phi (vec (repeatedly n l/lvar))
        elts (fd/interval 0 (dec n))
        ab2factors
        (-> (group-by (fn [[a b]] (compf S a b))
                      (composable-pairs S))
            (update-keys phi)
            (update-vals (partial map (partial map phi))))
        S2 (mapv
            (partial mapv #({nil n} % %)) ;replace nil with sg outside the fd
            S)]
    (l/run*
     [q]
     (l/everyg #(fd/in % elts) phi)
     (l/everyg (fn [[ab pairs]]
                 ;(println ab pairs)
                 (homomorphico S2 ab pairs))
               ab2factors)
     (l/== q phi))))

(def S
  [[0 1 2 3 4 nil]
   [1 0 2 4 3 nil]
   [nil nil nil nil nil 2]
   [nil nil nil nil nil 2]
   [nil nil nil nil nil 2]
   [nil nil nil nil nil 5]])

(count (homomorphisms S))

(group-by (fn [[a b]] (compf S a b))
          (composable-pairs S))

;what pairs produce 3?
(l/run*
 [p q]
 (compfo S p q 3))

(count (homomorphisms S))

;quick to get the homomorphisms
(count (filter (partial homomorphism? S)
               (map vec (selections (range 6) 6))))

(homomorphism? S (vec (repeat 6 0)))