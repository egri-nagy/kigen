(ns kigen.semigroupoid.enumeration
  "Enumearing all semigroup(oid)s using relational programming.
   Semigroupoids are represented as composition tables, a vector of vectors."
  (:require [clojure.core.logic :as l]
            [clojure.core.logic.fd :as fd]
            [clojure.math.combinatorics :refer [selections]]
            [kigen.semigroupoid.homomorphism :refer [compfo compf]]))

(defn associativity?
  "Brute-force checking associativity for a composition table."
  ([S] (associativity? S (selections (range (count S)) 3)))
  ([S triples]
   (every?
    (fn [[a b c]] (= (compf S a (compf S b c))
                     (compf S (compf S a b) c)))
    triples)))

(defn composable-triples
  "Returns all the composable triples for a composition table.
   A pair is composable if the element corresponding to their composition
   in the table is an element of the semigroupoid, i.e., not nil or some
   bigger index integer outside of the table. Here we do not have the domains
   and codomains, so we need to infer composability backwards,
   from the results."
  [S]
  (let [n (count S)
        elts (set (range n))
        triples (selections elts 3)]
    ;todo: there is a better way finding all composable pairs and combine
    (filter
     (fn [[a b c]]
       (and (elts (compf S a b)) ;checking set memberhsip
            (elts (compf S b c))))
     triples)))

(defn semigroupoid?
  "A composition table is "
  [S]
  (associativity? S (composable-triples S)))

(defn associativo
  "The goal for associativity for a given triple."
  [S [a b c]]
  (l/fresh
   [ab bc abc]
   (compfo S ab c abc)
   (compfo S a bc abc)
   (compfo S a b ab)
   (compfo S b c bc)))

(defn semigroups-order-n
  "Enumerating semigroups of order n by constructing all n by n composition
   tables.
   Constraints: table entries should be in 0..n-1, all triples should satisy
   associativity."
  [n]
  (let [elts (fd/interval 0 (dec n))
        S (vec (repeatedly n (fn [] (vec (repeatedly n l/lvar)))))
        lvars (apply concat S)
        triples (selections (range n) 3)]
    (l/run*
     [q]
     (l/everyg #(fd/in % elts) lvars)
     (l/everyg (partial associativo S) triples)
     (l/== q S))))

(defn composablo
  "The goal for associativity for a given triple."
  [S a b elts]
  (l/fresh
   [ab]
   (compfo S a b ab)
   (fd/in ab elts)))

(defn semigroupoids-order-n
  "Enumerating semigroups of order n by constructing all n by n composition
   tables.
   Constraints: table entries should be in 0..n-1, all triples should satisy
   associativity."
  [n]
  (let [elts (fd/interval 0 (dec n))
        allvals (fd/interval 0 n) ;we include n itself, instead of nil
        S (vec (repeatedly n (fn [] (vec (repeatedly n l/lvar)))))
        lvars (apply concat S)
        triples (selections (range n) 3)]
    (l/run*
     [q]
     (l/everyg #(fd/in % allvals) lvars)
     (l/everyg (fn [[a b c]]
                 (l/conde
                  [(composablo S a b elts) ;both composable and associative
                   (composablo S b c elts)
                   (associativo S [a b c])]
                  [(composablo S a b elts) ;a,b composable; b,c not
                   (compfo S b c n)]
                  [(composablo S b c elts) ;b,c composable, a,b not
                   (compfo S a b n)]
                  [(compfo S a b n) ; neither composable
                   (compfo S b c n)])) triples)
     (l/== q S))))