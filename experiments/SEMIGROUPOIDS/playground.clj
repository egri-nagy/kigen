;; finding endomorphisms of an abstract semigroupoid
;; v25.06.xx
(require '[kigen.semigroupoid.homomorphism :as hom])
(require '[kigen.semigroupoid.transformation :as t])
(require '[kigen.semigroupoid.enumeration :as enum])
(require '[kigen.semigroupoid.viz :refer [DotSemigroupoid
                                          Dot2PDF]])
(require '[kigen.logic :refer [lvar-table]])
(require '[kigen.semigroup.conjugacy :refer :all])
(require '[kigen.semigroup.sgp :refer [sgp-by-gens]])
(require '[kigen.diagram.transf :as transf])


(require '[clojure.core.logic :as l])
(require '[clojure.core.logic.fd :as fd])
(require '[clojure.math.combinatorics :refer [selections]])

(l/run*
 [q r]
 (l/membero q ["linux" "windows" "mac" "android" ""])
 (l/conda
  [(l/membero q ["linux" "windows"]) (l/== r 1)]
  [(l/== q "mac") (l/== r 2)]
  [l/succeed (l/== q "") (l/== r 3)]))

(l/run*
 [q r]
 (l/conda
  [(l/membero q ["linux" "windows"]) (l/== r 1)]
  [(l/== q "mac") (l/== r 2)]
  [l/succeed (l/== q "") (l/== r 3)])
 (l/membero q ["linux" "windows" "mac" "android" ""]))

(defn types
  "`n` arrows, `m` types"
  [n m]
  (let [[arrows lvars] (lvar-table n 2)
        pairs (for [a arrows, b arrows] [a b])
        objects (range m)]
    (l/run*
     [q]
     (l/== q arrows)
     ;;arrows have valid doms/cods
     (l/everyg #(fd/in % (fd/interval 0 (dec m))) lvars)
     ;;every type is used 
     ;;(l/everyg #(l/membero % lvars) objects)
     (l/everyg (fn [[[da ca] [db cb]]]
                 (l/conde
                  [(l/distincto [ca db])]
                  [(l/membero [da cb] arrows)]))
               pairs))))
;(types 1 1)
;(count (set (types 2 2)))
;(count (set (types 3 1)))
;(count (set (types 3 3)))

(defn transitively-closed-arrow-set-BF?
  "Checks the given set of arrows (arrow types, domain-codomain pairs) whether
   they are transitively closed under composition. This is brute force, it
   enumerates all pairs, and checks for closure when they are composable.
   `arrowset` should be a set of arrows as we use contains? for set membership"
  [arrowset]
  (let [pairs (for [a arrowset, b arrowset] [a b])]
    (every?
     (fn [[[doma coda] [domb codb]]]
       (or (not= coda domb)
           (contains? arrowset [doma codb])))
     pairs)))

(defn transitively-closed-arrow-set?
  "Checks the given set of arrows (arrow types, domain-codomain pairs) whether
   they are transitively closed under composition. First finds all composable
   pairs.
   `arrowset` should be a set of arrows as we use contains? for set membership"
  [arrowset]
  (let [src2arrows (group-by first arrowset)
        trg2arrows (group-by second arrowset)
        objects (set (concat (keys src2arrows) (keys trg2arrows)))
        ;use objects to pin down composable pairs
        composable-pairs (mapcat (fn [o] (for [a (trg2arrows o)
                                               b (src2arrows o)
                                               :when (and a b)]
                                           [a b]))
                                 objects)]
    (every? (fn [[[doma _] [_ codb]]]
              (contains? arrowset [doma codb]))
     composable-pairs)))

(defn enum
  [n m]
  (let [S (sgp-by-gens (transf/symmetric-gens m) transf/mul)]
    (->>
     (selections (range m) (* 2 n)) ;n arrows, 2n entries
     (filter (comp (partial = m) count set)) ;has to mention all m objects
     (map (comp (partial apply sorted-set) ; into sets of arrows
                (partial mapv vec) ; convert to vectors
                (partial partition 2))) ;form the arrows
     (filter (comp (partial = n) ;exactly n arrows
                   count))
     (distinct) ;as sets they might be the same
     (filter transitively-closed-arrow-set?)
     (map (fn [t] (setconjrep transf/mul t S))) ;it is enough to permute
     (distinct)))) ;conjugacy class representatives might be the same

(doseq [n (range 1 5)]
  (doseq [m (range 1 (inc (* 2 n)))]
    (println n " arrows " m "objects: " (count (enum n m)))))

(enum 1 2)
(enum 7 3)

; KIGEN 25.07.02 Clojure 1.12.1 Java 24.0.1 Mac OS X 15.5 aarch64
;; 1  arrows  1 objects:  1
;; 1  arrows  2 objects:  1
;; 2  arrows  1 objects:  0
;; 2  arrows  2 objects:  3
;; 2  arrows  3 objects:  3
;; 2  arrows  4 objects:  1
;; 3  arrows  1 objects:  0
;; 3  arrows  2 objects:  1
;; 3  arrows  3 objects:  8
;; 3  arrows  4 objects:  8
;; 3  arrows  5 objects:  3
;; 3  arrows  6 objects:  1
;; 4  arrows  1 objects:  0
;; 4  arrows  2 objects:  1
;; 4  arrows  3 objects:  8
;; 4  arrows  4 objects:  23
;; 4  arrows  5 objects:  23
;; 4  arrows  6 objects:  11
;; 4  arrows  7 objects:  3
;; 4  arrows  8 objects:  1
;; 5  arrows  1 objects:  0
;; 5  arrows  2 objects:  0
;; 5  arrows  3 objects:  6
;; 5  arrows  4 objects:  34
;; 5  arrows  5 objects:  67
;; 5  arrows  6 objects:  64
;; 5  arrows  7 objects:  32
;; 5  arrows  8 objects:  11

;; Example 3.2 from Representation Independent Decompositions of Computation https://arxiv.org/abs/2504.04660
(def S
  [[0 1 2 3 4 :n]
   [1 0 2 4 3 :n]
   [:n :n :n :n :n 2]
   [:n :n :n :n :n 2]
   [:n :n :n :n :n 2]
   [:n :n :n :n :n 5]])

;; weak isomorphism not symmetric
(hom/isomorphisms [[0 :n] [:n :n]]  [[0 1] [1 0]])
(hom/isomorphisms [[0 1] [1 0]] [[0 :n] [:n :n]])

(def non-symmetric-iso
  (filter (fn [T]
            (and
             (not (empty? (hom/isomorphisms T S)))
             (empty? (hom/isomorphisms S T))))
          (enum/all-composition-tables 2)))

;weak isomorphism not even producing semigroupoids
(count non-symmetric-iso)
(count (filter enum/associativity? non-symmetric-iso))

(count (hom/homomorphisms S S))
(count (hom/strict-homomorphisms S S))


