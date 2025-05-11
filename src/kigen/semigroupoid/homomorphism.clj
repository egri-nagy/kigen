(ns kigen.semigroupoid.homomorphism
  "Enumearing all homomorphisms of a semigroupoid using relational programming.
   Semigroupoids are represented as composition tables, a vector of vectors."
  (:require [clojure.core.logic :as l]
            [clojure.core.logic.fd :as fd]
            [kigen.logic :refer [ntho]]))

(defn compf
  "Composition function for arrows a and b in the given composition table S.
   Computing the composite is two vector lookups. "
  [S a b]
  (nth (nth S a) b)) ;get the row for a, then composite ab is the bth entry

(defn compfo
  "This goal succeeds if a composed with b is ab in the composition table S."
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
          :when (compf S a b)] ;when composition gives something not nil
      [a b])))

(defn composition-relation
  [S]
  (group-by (fn [[a b]] (compf S a b))
            (composable-pairs S)))

(defn substitute
  [comprel phi]
  (-> comprel
   (update-keys phi)
   (update-vals (partial map (partial map phi)))))

(defn compatible?
  "Checks the compatibility condition for the given element and the pairs. These
   should be in a special relationship: the pairs all compose to the given
   element.
   T - composition table
   ab - an element of T
   pairs - all the a,b pairs such that a compose b=ab"
  [T ab pairs]
  (every?
   (fn [[a b]] (=  ab (compf T  a  b)))
   pairs))

(defn homomorphism?-by-comprel
  "another test for homomorphisms, trying to reformulate constrains"
  [S T phi] 
  (every? (fn [[ab pairs]]
            (compatible? T ab pairs))
          (substitute (composition-relation S) phi)))

(defn homomorphism?
  "S is a composition table of a semigroupoid
   phi is a vector representing the homomorphism
   just for checking how to do it functionally, before relationally"
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
        constraints (substitute (composition-relation S) phi)
        T2 (mapv
            (partial mapv #({nil (count T)} % %)) ;replace nil with sg outside the fd
            T)]
    (l/run*
     [q]
     (l/everyg #(fd/in % elts) phi)
     (l/everyg (fn [[ab pairs]]
                 (l/everyg (fn [[a b]]
                             (compfo T2 a b ab))
                           pairs))
               constraints)
     (l/== q phi))))

(defn isomorphisms
  "Logic search for all homomoprhisms of semigroupoid S given as a composition
   table."
  [S T] ;given as composition tables
  (let [n (count S)
        phi (vec (repeatedly n l/lvar))
        elts (fd/interval 0 (dec (count T)))
        constraints (substitute (composition-relation S) phi)
        T2 (mapv
            (partial mapv #({nil (count T)} % %)) ;replace nil with sg outside the fd
            T)]
    (l/run*
     [q]
     (l/everyg #(fd/in % elts) phi)
     (fd/distinct phi)
     (l/everyg (fn [[ab pairs]]
                 (l/everyg (fn [[a b]]
                             (compfo T2 a b ab))
                           pairs))
               constraints)
     (l/== q phi))))