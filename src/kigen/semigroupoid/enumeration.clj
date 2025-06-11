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

(defn semigroupoids-order-n
  "Enumerating semigroups of order n by constructing all n by n composition
   tables.
   Constraints: table entries should be in 0..n-1 or nil,
   and all triples should satisy associativity."
  [n]
  (let [elts (fd/interval 0 (dec n))
        S (vec (repeatedly n (fn [] (vec (repeatedly n l/lvar)))))
        lvars (apply concat S)
        triples (selections (range n) 3)]
    (l/run*
     [q]
     (l/everyg #(l/conde  ;nil or element
                 [(l/nilo %)]
                 [(fd/in % elts)])
               lvars)
     (l/everyg (fn [[a b c]]
                 (l/conda
                  [(compfo S a b nil)]
                  [(compfo S b c nil)]
                  [(associativo S [a b c]) ]))
               triples)
     (l/== q S))))