;; Enumerating all arrow-type semigroupoids by a combinatorial brute force
;; method and then recursively adding one arrow by logic search.
;; kigen v25.07.14
(require '[kigen.semigroup.conjugacy :refer [setconjrep]])
(require '[kigen.semigroup.sgp :refer [sgp-by-gens]])

(require '[kigen.diagram.transf :as transf])
(require '[kigen.logic :refer [lvar-vector]])
(require '[kigen.digraph.transitivity :refer [transitive-closure]])
(require '[kigen.digraph.isomorphism :refer [digraph-isomorphisms
                                             iso-conj
                                             squash
                                             digraphs-up-to-morphisms]])
(require '[clojure.core.logic :as l])
(require '[clojure.core.logic.fd :as fd])
(require '[clojure.math.combinatorics :refer [selections]])
(require '[clojure.java.io :refer [writer]])

;;;;;;;;;; RECURSIVE ENUMERATION ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; the winning formula: just adding an arrow and transitively close the graph
;; very much like SubSemi for subsemigroup enumeration
;; db - database (hash-map)
;;  keys: [a,o] pairs
;;  vals: isomorphism class representatives
(defn n-arrow-graphs
  "Collects all graphs with `n` arrows from the database."
  [db n]
  (apply concat (map
                 db
                 (filter (comp (partial = n) first)
                         (keys db)))))

(defn m-vertex-graphs
  "Collects all graphs with `m` vertices (objects)."
  [db m]
  (apply concat (map
                 db
                 (filter (comp (partial = m) second)
                         (keys db)))))

(defn db-size
  "returns the number of entries in the database"
  [db]
  (reduce + (map (comp count db) (keys db))))

;; this prints the LaTeX table for the sgpoidsynth paper
(defn print-LaTeX-table
  [db n M]
  (when (not (zero? n)) ;; logging starts here (printing LaTeX table)
    (print n)
    (doseq [i (range 1 (inc M))]
      (print " &" (if (contains? db [n i])
                    (count (db [n i]))
                    "")))
    (println "  db size" (db-size db)))) ;; logging finishes

;; writes files to disc a<num of arrows>o<num of objects>
(defn write-anom-files
  [db n M]
  (doseq [i (range 1 (inc M))]
    (when (contains? db [n i])
      (with-open [w (clojure.java.io/writer (str "a" n "o" i))]
        (doseq [arrows (db [n i])]
          (.write w (prn-str (vec (sort arrows)))))))))

(defn add-arrow-and-close
  "adds an arrow from `arrows` that is not already in `graph` and computes
   the transitive closure, returns only distinct ones"
  [graph arrows]
  (let [narrows (remove (set graph) arrows)] ;; only add arrows that are new
    (distinct (map (fn [arr] (squash (transitive-closure (conj graph arr))))
                   narrows))))

(defn all-type-arrow-semigroupoids
  "Enumerates all arrow-type semigroupoids with maximum `M` number of objects."
  [M]
  (let [arrows (map vec (selections (range M) 2)) ;we have M^2 arrows
        register (fn [db graph]
                   (let [n (count graph) ;num of arrows
                         m (count (set (apply concat graph)))] ;num of objects
                     (if (contains? db [n m])
                       (update db [n m] (fn [class] (iso-conj class graph)))
                       (conj db [[n m] #{graph}]))))]
    (reduce
     (fn [db n]
       (print-LaTeX-table db n M) ;side-effecting stuff!
       (write-anom-files db n M) ;side-effecting stuff!
       (let [all-n-arrow-graphs (n-arrow-graphs db n) ;extending n-arrow graphs
             new-graphs (mapcat (fn [G] (add-arrow-and-close G arrows))
                                all-n-arrow-graphs)]
         (println "Extending" (count all-n-arrow-graphs) "graphs yielded"
                  (count new-graphs))
         (reduce register db new-graphs)))
     {[0 0] #{[]}};; we need to start somewhere, though not a valid example
     (range (inc (* M M))))))

;;timing runs and printing extra information about the totals
;; takes 6 seconds for m=4, half an hour for m=5 on an M1 Pro MacBook
(time
 (let [m 6
       db (all-type-arrow-semigroupoids m)]
   (doseq [n (range 1 (inc (* m m)))]
     (println n "arrows" (count (n-arrow-graphs db n))))
   (doseq [m' (range 1 (inc m))]
     (println m' "objects" (count (m-vertex-graphs db m'))))))

;; THE COMBINATORIAL PART ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn transitively-closed-arrow-set-BF?
  "Checks the given set of arrows (arrow types, domain-codomain pairs) whether
   they are transitively closed under composition. This is brute force, it
   enumerates all pairs, and checks for closure when they are composable.
   `arrowset` should be a set of arrows as we use contains? for set membership."
  [arrowset]
  (let [pairs (for [a arrowset, b arrowset] [a b])]
    (every?
     (fn [[[doma coda] [domb codb]]]
       (or (not= coda domb)
           (contains? arrowset [doma codb])))
     pairs)))

(defn composable-arrow-pairs
  "Returns the set of composable arrow pairs."
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
   `arrowset` should be a set of arrows as we use contains? for set membership."
  [arrowset]
  (every? (fn [[[doma _] [_ codb]]]
            (contains? arrowset [doma codb]))
          (composable-arrow-pairs arrowset)))

(defn arrows2comptab
  "Converts a set of arrows to a composition table."
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

(defn minimal-class-representatives
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
     (minimal-class-representatives m)
     ;(digraphs-up-to-morphisms) ;an alternative way to get representatives
     )))

;; the enumeration call for generating files
(doseq [[n m] (for [n [1 2 3 4]
                    m [1 2 3 4]]
                [n m])]
  (let [result (sort (combinatorial-enumeration n m))]
    (println n "-" m ": " (count result))
    (when (not (zero? (count result)))
      (with-open [w (clojure.java.io/writer (str "a" n "o" m))]
        (doseq [arrows result]
          (.write w (prn-str arrows)))))))

;; the quick calculations
(doseq [n (range 1 5)]
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
  "Comparing the combinatorial result with the logic-based one."
  [n m]
  (let [sols1 (combinatorial-enumeration (dec n) m)
        sols2 (combinatorial-enumeration (dec n) (dec m))
        nres1 (->>
               (mapcat
                (fn [sol]
                  (map (partial conj sol) (one-more-arrow sol m false)))
                sols1)
               (minimal-class-representatives m))
        nres2 (->>
               (mapcat
                (fn [sol]
                  (map (partial conj sol)
                       (one-more-arrow sol m true)))
                sols2)
               (minimal-class-representatives m))
        nres (into nres1 nres2)
        combinatorial-sols (combinatorial-enumeration n m)]
    (= (set (map set combinatorial-sols))
       (set (map set nres)))))

;;case 2,4 will fail
(for [n [2 3 4 5]
      m [2 3 4 5]]
  [n m (check n m)])