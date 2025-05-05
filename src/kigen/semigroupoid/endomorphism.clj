(ns kigen.semigroupoid.endomorphism
  "Enumearing all endomorphisms of a semigroupoid using relational
   programming."
  (:require [clojure.core.logic :as l]
            [clojure.core.logic.fd :as fd]))

(defn compf
  "Composition function for the given composition table S." 
  [S a b]
  (get (get S a) b)) ;get is more tolerant than nth, gives nil when troubled

(defn endomorphic?
  "S is a composition table of a semigroupoid
   phi is a vector representing the endomorphism
   just for checking how to do it functionally, before realtionally"
  [S phi]
  (let [n (count S)
        pairs (for [a (range n) b (range n)] [a b])]
    (every? (fn [[a b]]
              (let [ab (compf S a b)]
                (or (nil? ab) ; noncomposable arrow can go to composable one!
                    (= (get phi (compf S a b))
                       (compf S (get phi a) (get phi b))))))
            pairs)))

(defn endomorphisms
  "Logic search for all endomoprhisms of semigroupoid S given as a composition
   table."
  [S] ;given as a composition table
  (let [n (count S)
        lvars (repeatedly n l/lvar)
        elts (fd/interval 0 (dec n))]
    (l/run*
     [q]
     (l/everyg #(fd/in % elts) lvars)
     (l/== q lvars))))

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

(endomorphic? S (vec (repeat 6 1)))