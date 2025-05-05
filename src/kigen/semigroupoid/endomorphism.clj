(ns kigen.semigroupoid.endomorphism
  "Enumearing all endomorphisms of a semigroupoid using relational
   programming."
  (:require [clojure.core.logic :as l]
            [clojure.core.logic.fd :as fd]))

(defn compf
  "Composition function for the given composition table S." 
  [S a b]
  (nth (nth S a) b)) ;get is more tolerant than nth, gives nil when troubled

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
        phi (repeatedly n l/lvar)
        elts (fd/interval 0 (dec n))
        pairs (composable-pairs S)]
    (l/run*
     [q]
     (l/everyg #(fd/in % elts) phi)
     (l/== q phi))))

(def S
  [[0 1 2 3 4 nil]
   [1 0 2 4 3 nil]
   [nil nil nil nil nil 2]
   [nil nil nil nil nil 2]
   [nil nil nil nil nil 2]
   [nil nil nil nil nil 5]])

;quick to get the endomorphisms
(count (filter (partial endomorphic? S)
               (map vec (endomorphisms S))))

(endomorphic? S (vec (repeat 6 0)))

; for getting some simpler constraints
(group-by (fn [[a b]] (compf S a b)) (composable-pairs S))