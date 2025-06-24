(ns kigen.semigroupoid.enumeration
  "Enumerating all semigroup(oid)s using relational programming.
   Semigroupoids are represented as composition tables, a vector of vectors."
  (:require [clojure.core.logic :as l]
            [clojure.core.logic.fd :as fd]
            [clojure.math.combinatorics :refer [selections]]
            [kigen.semigroupoid.homomorphism :refer [composo compose]]
            [kigen.logic :refer [lvar-table]]))

;; SEMIGROUPS ;;;;;;;;;;;;;;
(defn associativity?
  "Brute-force checking associativity for a composition table `S`.
   If the `triples` are not given, then all possible triples are checked."
  ([S] (associativity? S (selections (range (count S)) 3)))
  ([S triples]
   (every?
    (fn [[a b c]] (= (compose S a (compose S b c))
                     (compose S (compose S a b) c)))
    triples)))

(defn associativo
  "The goal for associativity for a given triple."
  [S a b c]
  (l/fresh
   [ab bc abc]
   (composo S ab c abc)
   (composo S a bc abc)
   (composo S a b ab)
   (composo S b c bc)))

(defn composable-triples
  "Returns all the composable triples for a composition table `S`.
   A pair is composable if the element corresponding to their composition
   in the table is an element of the semigroupoid, i.e., not nil or some
   bigger index integer outside of the table. Here we do not have the domains
   and codomains, so we need to infer composability backwards,
   from the results."
  [S]
  (let [n (count S)
        elts (set (range n))
        triples (selections elts 3)]
    ;todo: is there a better way finding all composable pairs and combine?
    (filter
     (fn [[a b c]]
       (and (elts (compose S a b)) ;checking set memberhsip
            (elts (compose S b c))))
     triples)))

(defn semigroupoid?
  "A composition table describes a semigroupoid if all composable triples
   satisfy associativity."
  [S]
  (associativity? S (composable-triples S)))

(defn all-composition-tables
  "Lazy, brute force enumeration of all nxn composition tables.
   For testing purposes only. n=4 is already not feasible!"
  [n]
  (map
   (fn [entries]
     (mapv vec (partition n entries)))
   (selections (range (inc n)) (* n n))))

(defn semigroups-order-n
  "Enumerating semigroups of order n by constructing all n by n composition
   tables.
   Logic variables: the entries of S, n^2 of them in total.
   Constraints: table entries should be in 0..n-1, all triples should satisy
   associativity."
  [n]
  (let [elt? (fn [x] (fd/in
                      x
                      (fd/interval 0 (dec n))))
        [S lvars] (lvar-table n n)
        triples (selections (range n) 3)]
    (l/run*
     [q]
     (l/== q S)
     (l/everyg elt? lvars) ;;valid semigroup element
     (l/everyg (fn [[a b c]]
                 (associativo S a b c))
               triples)))) ;;all triples associative

;; SEMIGROUPOIDS ;;;;;;;;;;;;;;;;;;
(defn composablo
  "The goal for associativity for a given pair."
  [S a b elts]
  (l/fresh
   [ab]
   (composo S a b ab)
   (fd/in ab elts)))

(defn semigroupoids-order-n
  "Enumerating semigroups of order n by constructing all n by n composition
   tables.
   Constraints: table entries should be in 0..n-1,  or n,
   and all composable triples should satisy associativity.
   The value n stands for nil, and represents undefined composition."
  [n]
  (let [elts (fd/interval 0 (dec n)) ;the real elements, the arrows of S
        valid? (fn [x] (fd/in
                        x
                        (fd/interval 0 n))) ;we include n itself, instead of nil
        [S lvars] (lvar-table n n)
        triples (selections (range n) 3)]
    (l/run*
     [q]
     (l/== q S)
     (l/everyg valid? lvars) ;all lvars should be valid table entries
     (l/everyg
      (fn [[a b c]]
        (l/conde ;we write down all possibilities
         [(composablo S a b elts) ;both composable and associative
          (composablo S b c elts)
          (associativo S a b c)]
         [(composablo S a b elts) ;a,b composable; b,c not
          (composo S b c n)]
         [(composablo S b c elts) ;b,c composable, a,b not
          (composo S a b n)]
         [(composo S a b n) ; neither composable
          (composo S b c n)]))
      triples))))