(ns kigen.semigroupoid.enumeration
  "Enumerating all semigroup(oid)s using relational programming.
   Semigroupoids are represented as composition tables, a vector of vectors."
  (:require [clojure.core.logic :as l]
            [clojure.core.logic.fd :as fd]
            [clojure.math.combinatorics :refer [selections]]
            [kigen.semigroupoid.homomorphism :refer [composo compose compose-c]]
            [kigen.logic :refer [lvar-table lvar-vector]]))

(defn associative-triple?
  "Checks whether the given triple is associative in `S`.
   It works with :n entries, thus compatible with semigroupoids."
  [S [a b c]]
  (= (compose-c S a (compose-c S b c))
     (compose-c S (compose-c S a b) c)))

(defn associativity?
  "Brute-force checking associativity for a composition table `S`.
   If the `triples` are not given, then all possible triples are checked.
   Compatible with semigroupoids."
  ([S] (associativity? S (selections (range (count S)) 3)))
  ([S triples]
   (every? (partial associative-triple? S) triples)))

(defn associativity-failures
  "Brute-force checking associativity for a composition table `S`.
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


(defn composable-triples
  "Returns all the composable triples for a composition table `S`.
   A pair is composable if the element corresponding to their composition
   in the table is an element of the semigroupoid, i.e., not nil or some
   bigger index integer outside of the table. Here we do not have the domains
   and codomains, so we need to infer composability backwards,
   from the results."
  [S]
  (let [n (count S)
        elts (vec (range n))
        ; vector to set of arrows that can be post-composed
        composables (mapv
                     (fn [a]
                       (filter (fn [b] (not= :n (compose-c S a b))) elts))
                     elts)
        post-compose (fn [arrows]
                       (mapcat
                        (fn [arrow]
                          (map
                           (partial conj arrow)
                           (composables (last arrow))))
                        arrows))]
    (post-compose (post-compose (map vector elts)))))


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
  "Lazy, brute force enumeration of all nxn composition tables.
   It can be filtered by [[associativity?]].
   For testing purposes only: n=3 is fine, but n=4 has 5^16 cases."
  [n]
  (map
   (fn [all-entries-in-a-row]
     (mapv vec (partition n all-entries-in-a-row)))
   (selections (concat (range n) [:n]) (* n n))))

(defn type-inference
  "`S` composition table
   `m` number of objects, 1 or greater"
  [S m]
  (let [n (count S)
        composable (group-by (fn [[a b]]
                               (not= :n (compose-c S a b)))
                             (selections (range n) 2))
        doms (lvar-vector n)
        cods (lvar-vector n)
        lvars (into doms cods)
        objects (range m)]
    ;(println "composable " (composable true))
    ;(println "non-composable " (composable false))

    (l/run*
     [q]
     (l/== q {:doms doms, :cods cods})
     (l/everyg (fn [lv] (l/membero lv objects)) lvars)
     (l/everyg (fn [[a b]] (l/== (cods a) (doms b))) (composable true))
     (l/everyg (fn [[a b]] (let [c (compose-c S a b)]
                             (l/all (l/== (doms a) (doms c))
                                    (l/== (cods b) (cods c)))))
               (composable true))
     (l/everyg (fn [[a _ c :as triple]]
                 (let [d (reduce (partial compose-c S) triple)]
                   (l/all (l/== (doms a) (doms d))
                          (l/== (cods c) (cods d)))))
               (composable-triples S))
     (l/everyg (fn [[a b]] (l/distincto [(cods a) (doms b)])) (composable false)))))

(defn find-minimal-type-structure
  [S]
  (first
   (remove empty?
           (map
            (fn [i] (first (type-inference S i)))
            (range 1 (* 2 (count S)))))))

(def S
  '[[:n 2 :n]
    [:n :n :n]
    [:n :n :n]])

(type-inference S 3)
(find-minimal-type-structure S)

(def T
  '[[:n :n :n]
    [:n :n 1]
    [:n :n :n]])

(associativity? T)
(find-minimal-type-structure T)


(def Q
  '[[:n :n :n]
    [:n :n :n]
    [:n :n :n]])
(find-minimal-type-structure Q)
(type-inference Q 0)

(def S2
  [[0 1 2 3 4 :n]
   [1 0 2 4 3 :n]
   [:n :n :n :n :n 2]
   [:n :n :n :n :n 2]
   [:n :n :n :n :n 2]
   [:n :n :n :n :n 5]])
(find-minimal-type-structure S2)