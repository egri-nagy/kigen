(ns kigen.semigroupoid.endomorphism
  "Enumearing all endomorphisms of a semigroupoid using relational
   programming."
  (:require [clojure.core.logic :as l]
            [clojure.core.logic.fd :as fd]
            [kigen.logic :refer [ntho]]))

(defn compf
  "Composition function for the given composition table S."
  [S a b]
  (nth (nth S a) b)) ;get is more tolerant than nth, gives nil when troubled

(defn compfo
  [S a b ab]
  (l/fresh
   [v]
   (ntho S a v)
   (ntho v b ab)))

(defn composable-pairs
  "All the composable pairs of elements of semigroupoid S given as a composition
   table."
  [S]
  (let [n (count S)]
    (for [a (range n)
          b (range n)
          :when (compf S a b)]
      [a b])))

(defn endomorphic?
  "ab - an element of S
   pairs - all the a,b pairs such that ab=ab"
  [S phi ab pairs]
  (every?
   (fn [[a b]] (= (phi ab) (compf S (phi a) (phi b))))
   pairs))

(defn endomorphico
  [S ab pairs]
  ;(println "ab:" ab)
  ;(println "pairs:" pairs)
  (l/everyg (fn [[a b]]
              (compfo S a b ab))
            pairs))

(defn endomorphism?
  "another test for endomorphisms, trying to reformulate constrains"
  [S phi]
  (let [ab2factors (group-by (fn [[a b]] (compf S a b))
                             (composable-pairs S))]
    (every? (fn [[ab pairs]]
              (endomorphic? S phi ab pairs))
            ab2factors)))

(defn endomorphism2?
  "S is a composition table of a semigroupoid
   phi is a vector representing the endomorphism
   just for checking how to do it functionally, before realtionally"
  [S phi]
  (every? (fn [[a b]]
            (= (phi (compf S a b))
               (compf S (phi a) (phi b))))
          (composable-pairs S)))

(defn endomorphisms
  "Logic search for all endomoprhisms of semigroupoid S given as a composition
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
        S2 (mapv (partial mapv #({nil n} % %)) S)]
    ;(println "phi:" phi)
    ;(println "ab2factors" ab2factors) (println)
    (l/run*
     [q]
     (l/everyg #(fd/in % elts) phi)
     (l/everyg (fn [[ab pairs]]
                 ;(println ab pairs)
                 (endomorphico S2 ab pairs))
               ab2factors)
     (l/== q phi))))

(def S
  [[0 1 2 3 4 nil]
   [1 0 2 4 3 nil]
   [nil nil nil nil nil 2]
   [nil nil nil nil nil 2]
   [nil nil nil nil nil 2]
   [nil nil nil nil nil 5]])

(count (endomorphisms S))

(group-by (fn [[a b]] (compf S a b))
          (composable-pairs S))

;what pairs produce 3?
(l/run*
 [p q]
 (compfo S p q 3))

(count (endomorphisms S))

;quick to get the endomorphisms
(count (filter (partial endomorphism? S)
               (map vec (endomorphisms S))))

(endomorphism? S (vec (repeat 6 0)))