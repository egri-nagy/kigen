(ns kigen.semigroupoid.enumeration
  "Enumearing all semigroup(oid)s using relational programming.
   Semigroupoids are represented as composition tables, a vector of vectors."
  (:require [clojure.core.logic :as l]
            [clojure.core.logic.fd :as fd]
            [clojure.math.combinatorics :refer [selections]]
            [kigen.semigroupoid.homomorphism :refer [compfo compf]]))

(defn associativity?
  "Brute-force checking associativity for a composition table."
  [S]
  (let [triples (selections (range (count S)) 3)]
    (every?
     (fn [[a b c]] (= (compf S a (compf S b c))
                      (compf S (compf S a b) c)))
     triples)))

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