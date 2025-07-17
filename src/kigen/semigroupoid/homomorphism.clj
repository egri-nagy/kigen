(ns kigen.semigroupoid.homomorphism
  "Finding all homomorphisms of a semigroupoid into another one by using
   relational programming.
   Semigroupoids are represented abstractly, as composition tables, a vector of
   vectors. For non-composable arrow pairs the corresponding entry is :n."
  (:require [clojure.core.logic :as l]
            [kigen.logic :refer [ntho lvar-vector]]))

(defn compose-NC
  "Composition function for arrows a and b in the given composition table S.
   Computing the composite is two vector lookups. Arrows a and b are assumed
   to be composable."
  [S a b]
  (nth (nth S a) b)) ;get the row for a, then composite ab is the bth entry

(defn compose
  "Safe composition, works for all `a`,`b` arrow pairs. Composing with :n,
   the non-defined value, yields :n."
  [S a b]
  (if (or (= :n a) (= :n b))
    :n
    (let [row (nth S a)]
      (if (= :n row)
        :n
        (nth row b)))))

(defn composo
  "Succeeds if `a` composed with `b` is `ab` in the composition table `S`."
  [S a b ab]
  (l/fresh
   [row]
   (ntho S a row) ;the row of a
   (ntho row b ab))) ;the bth entry in that row should be ab

(defn composition-relation
  "A hash-map with keys as the elements of S, and the values are pairs that
   compose to that key value. Composition turned backwards.
   It includes undefined value :n as a key."
  [S]
  (group-by (fn [[a b]]
              (compose S a b))
            (let [n (count S)]
              (for [a (range n) ;all a b pairs
                    b (range n)]
                [a b]))))

(defn composable-pairs
  "All the composable pairs of elements of semigroupoid S given as a composition
   table. In the abstract setting we do not have domains and codomains, we
   reason backwards, arrows are composable if the composite in the table."
  [S]
  (apply concat (vals (dissoc (composition-relation S) :n))))

(defn substitute
  "Transfer the composition relation through a morphism phi, i.e., converting
   all keys and values by phi."
  [comprel phi]
  (let [phi' (fn [x] (if (= x :n) :n (phi x)))] ;avoid indexing by :n
    (-> comprel
        (update-keys phi')
        (update-vals (partial map (partial map phi'))))))

(defn comprel?
  "Checks composition relation for the given element and the pairs:
   the pairs all compose to the given element. Used in homomorphism test.
   T - composition table
   ab - an element of T
   pairs - all the a,b pairs such that a composed with  b is ab"
  [T ab pairs]
  (every?
   (fn [[a b]] (=  ab (compose T a b)))
   pairs))

(defn homomorphism?-by-comprel
  "another test for homomorphisms, trying to reformulate constraints"
  [S T phi]
  (every? (fn [[ab pairs]]
            (comprel? T ab pairs))
          (substitute (composition-relation S) phi)))

(defn homomorphism?
  "S is a composition table of a semigroupoid
   phi is a vector representing the homomorphism
   just for checking how to do it functionally, before relationally"
  [S T phi]
  (every? (fn [[a b]]
            (= (phi (compose S a b))
               (compose T (phi a) (phi b))))
          (composable-pairs S)))


(defn morphism-search
  "Logic search for all homomorphisms of semigroupoid `S` to `T` given as
   composition tables. If `bijective?`, then only isomorphisms are enumerated.
   When `strict?`, non-composable arrow pairs cannot become composable."
  [S T bijective? strict?] ;given as composition tables
  (let [n (count S)
        phi (lvar-vector n)
        elts (range (count T))
        constraints (if strict?
                      (substitute (composition-relation S) phi) 
                      (substitute (dissoc (composition-relation S) :n) phi))]
    (l/run*
     [q]
     (l/== q phi)
     (l/everyg (fn [elt] (l/membero elt elts)) phi)
     (if bijective?
       (l/distincto phi)
       l/succeed)
     (l/everyg (fn [[ab pairs]]
                 (l/everyg (fn [[a b]]
                             (composo T a b ab))
                           pairs))
               constraints))))

(defn isomorphisms
  "Logic search for all isomorphisms from semigroupoid S to T given as
   composition tables."
  [S T]
  (morphism-search S T true false))

(defn strict-isomorphisms
  "Logic search for all isomorphisms from semigroupoid S to T given as
   composition tables."
  [S T]
  (morphism-search S T true true))

(defn homomorphisms
  "Logic search for all homomorphisms from semigroupoid S to T given as
   composition tables."
  [S T] ;given as composition tables
  (morphism-search S T false false))

(defn strict-homomorphisms
  "Logic search for all homomorphisms from semigroupoid S to T given as
   composition tables."
  [S T] ;given as composition tables
  (morphism-search S T false true))


(defn subtable
  "Extracts the subtable defined by `elts` from `T` and recodes it as a
   composition table. The subtable is assumed to be closed."
  [T elts]
  (let [v (vec (sort (distinct elts)))
        m (conj (zipmap v (range)) [:n :n])]
    (mapv
     (fn [a]
       (mapv (fn [b] (m (compose T a b))) v))
     v)))

(defn comptabs-up-to-morphisms
  "Given a collection of composition tables, it returns the isomorphism
   anti-isomorphism class representatives. Representative is the first one
   encountered. Anti-isomporphism is isomorphism to the transposed table."
  [sgps]
  (reduce
   (fn [reps S] ;representatives so far and the next semigroup
     (let [S' S]
       (if (some (fn [T]
                   (or (first (isomorphisms S' T))
                       (first (isomorphisms (apply mapv vector S') T))))
                 reps)
         reps ;if S is (anti-)isomorphic to something in reps already
         (conj reps S)))) ;otherwise keep it
   #{}
   sgps))

(def S
  [[0 1 2 3 4 :n]
   [1 0 2 4 3 :n]
   [:n :n :n :n :n 2]
   [:n :n :n :n :n 2]
   [:n :n :n :n :n 2]
   [:n :n :n :n :n 5]])

(isomorphisms S S)
