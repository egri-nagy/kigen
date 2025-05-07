(ns kigen.semigroupoid.homomorphism
  "Enumearing all homomorphisms of a semigroupoid using relational
   programming."
  (:require [clojure.core.logic :as l]
            [clojure.core.logic.fd :as fd]
            [kigen.logic :refer [ntho]]))

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
  "ab - an element of T
   pairs - all the a,b pairs such that ab=ab"
  [T phi ab pairs]
  (every?
   (fn [[a b]] (= (phi ab) (compf T (phi a) (phi b))))
   pairs))

(defn homomorphico
  "Goal succeeds if all pairs compose to ab in T."
  [T ab pairs]
  (l/everyg (fn [[a b]]
              (compfo T a b ab))
            pairs))

(defn homomorphism?
  "another test for homomorphisms, trying to reformulate constrains"
  [S T phi]
  (let [ab2factors (group-by (fn [[a b]] (compf S a b))
                             (composable-pairs S))]
    (every? (fn [[ab pairs]]
              (homomorphic? T phi ab pairs))
            ab2factors)))

(defn homomorphism2?
  "S is a composition table of a semigroupoid
   phi is a vector representing the homomorphism
   just for checking how to do it functionally, before realtionally"
  [S T phi]
  (every? (fn [[a b]]
            (= (phi (compf S a b))
               (compf T (phi a) (phi b))))
          (composable-pairs S)))

(defn homomorphisms
  "Logic search for all homomoprhisms of semigroupoid S given as a composition
   table."
  [S T] ;given as composition tables
  (let [n (count S)
        phi (vec (repeatedly n l/lvar))
        elts (fd/interval 0 (dec (count T)))
        ab2factors
        (-> (group-by (fn [[a b]] (compf S a b))
                      (composable-pairs S))
            (update-keys phi)
            (update-vals (partial map (partial map phi))))
        T2 (mapv
            (partial mapv #({nil (count T)} % %)) ;replace nil with sg outside the fd
            T)]
    (l/run*
     [q]
     (l/everyg #(fd/in % elts) phi)
     (l/everyg (fn [[ab pairs]]
                 ;(println ab pairs)
                 (homomorphico T2 ab pairs))
               ab2factors)
     (l/== q phi))))