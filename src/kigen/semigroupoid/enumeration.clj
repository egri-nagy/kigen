(ns kigen.semigroupoid.enumeration
  "Enumerating all semigroup(oid)s using relational programming.
   Semigroupoids are represented as composition tables, a vector of vectors."
  (:require [clojure.core.logic :as l]
            [clojure.core.logic.fd :as fd]
            [clojure.math.combinatorics :refer [selections]]
            [kigen.semigroupoid.homomorphism :refer [composo compose]]
            [kigen.logic :refer [lvar-table lvar-vector]]))

(defn associative-triple?
  "Checks whether the given triple is associative in `S`.
   It works with :n entries, thus compatible with semigroupoids."
  [S [a b c]]
  (= (compose S a (compose S b c))
     (compose S (compose S a b) c)))

(defn associativity?
  "Brute-force checking associativity for a composition table `S`.
   If the `triples` are not given, then all possible triples are checked.
   Compatible with semigroupoids."
  ([S] (associativity? S (selections (range (count S)) 3)))
  ([S triples]
   (every? (partial associative-triple? S) triples)))

(defn associativity-failures
  "Returns pairs where associativity fails in a composition table `S`.
   If the `triples` are not given, then all possible triples are checked.
   Compatible with semigroupoids."
  ([S] (associativity-failures S (selections (range (count S)) 3)))
  ([S triples]
   (remove (partial associative-triple? S) triples)))

;; SEMIGROUPS ;;;;;;;;;;;;;;
(defn associativo
  "The goal for associativity for a given triple when using semigroup
   composition."
  [S [a b c]]
  (l/fresh
   [ab bc abc]
   (composo S ab c abc)
   (composo S a bc abc)
   (composo S a b ab)
   (composo S b c bc)))

(defn associativo-diagonal
  "The goal for associativity for a given triple of same elements for
   typeless semigroup composition."
  [S a]
  (l/fresh
   [aa aaa]
   (composo S aa a aaa)
   (composo S a aa aaa)
   (composo S a a aa)))

(defn predefined
  "Sudoku-like hints for the composition table. Returns the goals for predefined
   values."
  [lvs cells]
  (l/and*
   (reduce
    (fn [goals [lv cell]]
      (if (not= '- cell)
        (conj goals (l/== lv cell))
        goals))
    ()
    (map vector lvs cells)))) ;pairs

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
(defn sgpoid-associativo ; this works, optimizations invariably fail
  "The goal for associativity for a given triple in a semigroupoid."
  [S [a b c]]
  (l/fresh
   [ab bc]
   (composo S a b ab)
   (composo S b c bc)
   (l/conda
    [(l/conde
      [(l/== ab :n) (composo S a bc :n)]
      [(l/== bc :n) (composo S ab c :n)]
      [(l/== bc :n) (l/== ab :n)])]
    [(l/fresh
      [abc]
      (composo S ab c abc)
      (composo S a bc abc))])))

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

(defn all-composition-tables
  "Lazy, brute force enumeration of all `n` by `n` composition tables.
   It can be filtered by [[associativity?]].
   For testing purposes only: n=3 is fine, but n=4 has 5^16 cases."
  [n]
  (map
   (fn [all-entries-in-a-row]
     (mapv vec (partition n all-entries-in-a-row)))
   (selections (concat (range n) [:n]) (* n n))))

(defn type-inference
  "Takes a composition table and a positive integer and tries to find type
   structures (domains and codomains for the arrows).
   Empty list is returned if not possible. That happens when to composition
   table is not a semigroupoid. It also gives results for tables that fail
   associativity, thus a solution does not guarantee semigroupoidness.
   `S` composition table
   `m` number of objects, 1 or greater"
  [S m]
  (let [n (count S) ;number of arrows
        composable (group-by (fn [[a b]] ; classify pairs by composability
                               (not= :n (compose S a b)))
                             (selections (range n) 2))
        doms (lvar-vector n)
        cods (lvar-vector n)
        lvars (into doms cods)
        types (range m)]
    (l/run*
     [q]
     (l/== q {:doms doms, :cods cods})
     ; logic variables should point to types
     (l/everyg (fn [lv] (l/membero lv types)) lvars)
     ; for all composable pairs, the codomain of the 1st match the dom of 2nd
     (l/everyg (fn [[a b]]
                 (l/== (cods a) (doms b)))
               (composable true))
     ; when not composable, they should be distinct
     (l/everyg (fn [[a b]]
                 (l/distincto [(cods a) (doms b)]))
               (composable false))
     ; the composed arrow should have the dom of the 1st, cod of the 2nd
     (l/everyg (fn [[a b]] (let [c (compose S a b)]
                             (l/all (l/== (doms a) (doms c))
                                    (l/== (cods b) (cods c)))))
               (composable true)))))

(defn typestruct2arrows
  "Converts the results of [[type-inference]] into a graph of the arrows.
   The arrows are in the same order as in the composition table."
  [{doms :doms cods :cods}]
  (mapv vector doms cods))

(defn find-minimal-type-structure
  "Starting from 1 we try to construct a type structure for composition
   table `S`. It returns nil if no such structure is found with 2n types.
   If inferencing is possible, it returns one such minimal construction."
  [S]
  (first (remove empty? ; nil is empty
                 (map
                  (fn [i] (first (type-inference S i)))
                  (range 1 (inc (* 2 (count S)))))))) ; 1..2n

(defn type-degree
  "Based on the result from [[find-minimal-type-structure]] this gives the
   minimum number of objects of an admissible type structure for composition
   table `S`."
  [S]
  (let [typestruct (find-minimal-type-structure S)]
    (when typestruct
      (inc (apply max (concat (:doms typestruct)
                              (:cods typestruct)))))))