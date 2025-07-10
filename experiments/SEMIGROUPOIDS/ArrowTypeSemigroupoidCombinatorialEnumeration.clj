;; Enumerating all arrow-type semigroupoids by a combinatorial brute force
;; method.
;; v25.07.xx
(require '[kigen.semigroup.conjugacy :refer [setconjrep]])
(require '[kigen.semigroup.sgp :refer [sgp-by-gens]])
(require '[kigen.diagram.transf :as transf])

(require '[clojure.math.combinatorics :refer [selections]])

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
     (map (partial concat '(0)) (selections (range m) (dec (* 2 n)))) ;n arrows, 2n entries, starting with 0
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

(enum 1 1)

;(count (enum 7 5))
;(count (enum 8 4))

;;(setconjrep transf/mul [[0 0] [0 1] [3 4]] (sgp-by-gens (transf/symmetric-gens 5) transf/mul))

;; the quick calculations
(doseq [n (range 1 5)]
  (doseq [m (range 1 (inc (* 2 n)))]
    (println n " arrows " m "objects: " (count (enum n m)))))


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