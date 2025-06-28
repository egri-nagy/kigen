(ns kigen.semigroupoid.enumeration
  "Enumerating all semigroup(oid)s using relational programming.
   Semigroupoids are represented as composition tables, a vector of vectors."
  (:require [clojure.core.logic :as l]
            [clojure.core.logic.fd :as fd]
            [clojure.math.combinatorics :refer [selections]]
            [kigen.semigroupoid.homomorphism :refer [composo compose compose-c]]
            [kigen.logic :refer [lvar-table]]))

;; SEMIGROUPS ;;;;;;;;;;;;;;
(defn associative-triple?
  [S [a b c]]
  (= (compose-c S a (compose-c S b c))
     (compose-c S (compose-c S a b) c)))

(defn associativity?
  "Brute-force checking associativity for a composition table `S`.
   If the `triples` are not given, then all possible triples are checked."
  ([S] (associativity? S (selections (range (count S)) 3)))
  ([S triples]
   (every? (partial associative-triple? S) triples)))

(defn associativo
  "The goal for associativity for a given triple."
  [S [a b c]]
  (l/fresh
   [ab bc abc]
   (composo S ab c abc)
   (composo S a bc abc)
   (composo S a b ab)
   (composo S b c bc)))

(defn associativo-diagonal
  "The goal for associativity for a given triple."
  [S a]
  (l/fresh
   [aa aaa]
   (composo S aa a aaa)
   (composo S a aa aaa)
   (composo S a a aa)))

(defn sgpoid-associativo ; this worked
  "The goal for associativity for a given triple."
  [S [a b c]]
  (l/fresh
   [ab bc]
   (composo S a b ab)
   (composo S b c bc)
   (l/conda
    [ (l/conde
       [(l/== ab :n) (composo S a bc :n)] 
       [(l/== bc :n) (composo S ab c :n)]
       [(l/== bc :n) (l/== ab :n)])]
    [(l/fresh
      [abc]
      (composo S ab c abc)
      (composo S a bc abc))])))

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
   (selections (concat (range n) [:n]) (* n n))))

(defn predefined
  [[lv & lvs] [cell & cells]]
  (if lv
    (l/all
     (if (= '- cell)
       l/succeed
       (l/== lv cell))
     (predefined lvs cells))
    l/succeed))

(defn semigroups-order-n
  "Enumerating semigroups of order n by constructing all n by n composition
   tables.
   Logic variables: the entries of S, n^2 of them in total.
   Constraints: table entries should be in 0..n-1, all triples should satisfy
   associativity."
  ([n] (semigroups-order-n n nil))
  ([n comptab]
   (let [elt? (fn [x] (l/membero x (range n)))
         [S lvars] (lvar-table n n)
         elts (range n) ; the semigroup elements
         triples (remove (partial apply =) ; triples with same values
                         (selections elts 3))]
     (l/run*
      [q]
      (l/== q S)
      (if comptab
        (predefined lvars comptab)
        l/succeed)
      (l/everyg elt? lvars) ;;valid semigroup element
      (l/everyg (partial associativo-diagonal S) elts) ; check diagonals
      (l/everyg (partial associativo S) triples))))) ;;all triples associative

;; SEMIGROUPOIDS ;;;;;;;;;;;;;;;;;;
(defn semigroupoids-order-n
  "Enumerating semigroupoids of order n by constructing all n by n composition
   tables.
   Constraints: table entries should be in 0..n-1,  or n for undefined,
   and all composable triples should satisfy associativity.
   The value n stands for nil, and represents undefined composition."
  ([n] (semigroupoids-order-n n nil))
  ([n partial-comptab]
   (let [elts (range n) ;the real elements, the arrows of S
         valid? (fn [x] (l/membero x (concat elts [:n]))) ;we include :n
         [S lvars] (lvar-table n n)
         triples (selections (range n) 3)]
     (l/run*
      [q]
      (l/== q S)
      (l/everyg valid? lvars) ;all lvars should be valid table entries
      (if partial-comptab
        (predefined lvars partial-comptab)
        l/succeed)
      (l/everyg (partial sgpoid-associativo S) triples)))))


(def S
  '[[:n 2 :n]
    [:n :n :n]
    [:n :n :n]])

(def T
  '[[:n :n :n]
    [:n :n 1]
    [:n :n :n]])

(def Q
  '[[:n :n :n]
    [:n :n :n]
    [:n :n :n]])
