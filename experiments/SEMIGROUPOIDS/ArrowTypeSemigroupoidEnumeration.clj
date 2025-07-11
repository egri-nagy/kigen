;; Enumerating all arrow-type semigroupoids by a combinatorial brute force
;; method and then recursively adding one arrow by logic search.
;; kigen v25.07.11
(require '[kigen.semigroup.conjugacy :refer [setconjrep]])
(require '[kigen.semigroup.sgp :refer [sgp-by-gens]])
(require '[kigen.diagram.transf :as transf])
(require '[kigen.logic :refer [lvar-vector]])

(require '[clojure.core.logic :as l])
(require '[clojure.core.logic.fd :as fd])
(require '[clojure.math.combinatorics :refer [selections]])
(require '[clojure.java.io :refer [writer]])

;; THE COMBINATORIAL PART ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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

(defn composable-arrow-pairs
  [arrowset]
  (let [src2arrows (group-by first arrowset)
        trg2arrows (group-by second arrowset)
        objects (set (concat (keys src2arrows) (keys trg2arrows)))]
          ;use objects to pin down composable pairs
    (mapcat (fn [o] (for [a (trg2arrows o)
                          b (src2arrows o)
                          :when (and a b)]
                      [a b]))
            objects)))

(defn transitively-closed-arrow-set?
  "Checks the given set of arrows (arrow types, domain-codomain pairs) whether
   they are transitively closed under composition. First finds all composable
   pairs.
   `arrowset` should be a set of arrows as we use contains? for set membership"
  [arrowset]
  (every? (fn [[[doma _] [_ codb]]]
            (contains? arrowset [doma codb]))
          (composable-arrow-pairs arrowset)))

(defn arrows2comptab
  [arrows]
  (let [arrow2index (zipmap arrows (range))
        index2arrow (zipmap (range) arrows)
        n (count arrows)
        index-pairs (for [a (range n) b (range n)] [a b])]
    (mapv
     (fn [[a b]]
       (let [[doma coda] (index2arrow a)
             [domb codb] (index2arrow b)]
         (if (= coda domb)
           (arrow2index [doma codb])
           :n)))
     index-pairs)))

(defn  reps
  "m - number of objects, arrows"
  [m arrowsets]
  (let [S (sgp-by-gens (transf/symmetric-gens m) transf/mul)]
    (distinct
     (map (fn [t] (setconjrep transf/mul t S)) ;it is enough to permute
          arrowsets))))

(defn combinatorial-enumeration
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
     (reps m))))

;(count (enum 7 5))
 (doseq [[n m] (for [n [1 2 3 4 5]
                     m [1 2 3 4 5]]
                 [n m])]
   (let [result (sort (combinatorial-enumeration n m))]
     (println n "-" m ": " (count result))
     (when (not (zero? (count result)))
       (with-open [w (clojure.java.io/writer (str "a" n "o" m))]
         (doseq [arrows result]
           (.write w (prn-str arrows)))))))

; (sort (combinatorial-enumeration 4 5))

;; the quick calculations
(doseq [n (range 1 4)]
  (doseq [m (range 1 (inc (* 2 n)))]
    (println n " arrows " m "objects: "
             (count (combinatorial-enumeration n m)))))

;; THE LOGIC PART ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn one-more-arrow
  "Finds all the arrows that can be added without violating composition to
   the `arrows` when we have `m` objects available. When `added-object`,
   we need to connect the arrow to the last object."
  [arrows m added-object?]
  (let [[d c] (lvar-vector 2)
        extended (conj arrows [d c])]
    (l/run*
     [q]
     (l/== q [d c])
     ;(l/everyg #(fd/in % (fd/interval 0 (dec m))) [d c]) ;this does not work
     (l/membero d (range m))
     (l/membero c (range m))
     (if added-object?
       (l/conda [(l/== (dec m) d)]
                [(l/== (dec m) c)])
       l/succeed)
     (l/distincto extended)
     (l/everyg (fn [[dom cod]]
                 (l/conda ;;postcompose
                  [(l/distincto [cod d])]
                  [(l/membero [dom c] extended)]))
               extended)
     (l/everyg (fn [[dom cod]]
                 (l/conda ;;precompose
                  [(l/distincto [c dom])]
                  [(l/membero [d cod] extended)]))
               extended))))

(one-more-arrow [[0 0] [1 1]] 3 true)
(one-more-arrow [[0 1] [1 1]] 3 true)


(defn check
  "Comparing the combinatorial result with the recursive one."
  [n m]
  (let [sols1 (combinatorial-enumeration (dec n) m)
        sols2 (combinatorial-enumeration (dec n) (dec m))
        nres1 (->>
               (mapcat
                (fn [sol]
                  (map (partial conj sol) (one-more-arrow sol m false)))
                sols1)
               (reps m))
        nres2 (->>
               (mapcat
                (fn [sol]
                  (map (partial conj sol)
                       (one-more-arrow sol m true)))
                sols2)
               (reps m))
        nres (into nres1 nres2)
        combinatorial-sols (combinatorial-enumeration n m)]
    (= (set (map set combinatorial-sols))
       (set (map set nres)))))

(for [n [2 3 4 5]
      m [2 3 4 5]]
  [n m (check n m)])