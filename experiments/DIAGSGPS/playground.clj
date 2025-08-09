(require '[kigen.semigroup.genmorph :refer :all])
(require '[kigen.semigroup.sgp :refer [sgp-by-gens]])
(require '[kigen.diagram.transf :as t])
(require '[kigen.diagram.transf-conj :as t-c])
(require '[kigen.semigroup.conjugacy :as c])
(require '[kigen.semigroup.genmorph :refer [sgp-embeddings-by-gens]])
(require '[kigen.canonical-labeling :refer [can-set-seq can-seq]])
(require '[clojure.math.combinatorics :refer [selections]])
(require '[taoensso.timbre :as timbre])
(require '[clojure.set :refer [union]])

(timbre/set-min-level! :trace)

(def S5 (sgp-by-gens (t/symmetric-gens 5) t/mul))
;; (def T5 (sgp-by-gens (t/full-ts-gens 5) t/mul))

;; (def S4 (sgp-by-gens (t/symmetric-gens 4) t/mul))
;; (def T4 (sgp-by-gens (t/full-ts-gens 4) t/mul))

(def S2 (sgp-by-gens (t/symmetric-gens 2) t/mul))

(def S3 (sgp-by-gens (t/symmetric-gens 3) t/mul))
(def T3 (sgp-by-gens (t/full-ts-gens 3) t/mul))

;; (def S6 (sgp-by-gens (t/symmetric-gens 6) t/mul))
;; (def T6 (sgp-by-gens (t/full-ts-gens 6) t/mul))

;; checking number of conjreps
(map
 #(count (into #{} (map t-c/conjrep (selections (range %) %))))
 (range 1 8))

(t-c/single-maps [1 0 2])
(map vector [2 2] [0 0])
(map vector [0 1] [1 0])

(def t [0 0 0])


(pre-images [1 2 0])
(pre-images [0 0 0 0])

(pre-images {0 0,1 0, 2 0})

;; merging hash-map perms
(defn hmpm
  [p q]
  (reduce
   (fn [m k]
     (conj m [k (into (p k) (q k))]))
   {}
   (keys p)))

(hmpm {3 #{0}, 2 #{1}, 1 #{2}} {3 #{0}, 1 #{1}, 2 #{2}})

(defn realize-a-mapping
  "Given a mapping m and a desired mapping d, we try to turn the mapping m
  into d by extending a partial permutation p represented as a hashmap.
  This may fail if we already had a map to that point, or we end up mapping
  a single point to two images.
  An extended hashmap is returned if it is possible, otherwise nil."
  [m d p]
  ;(print "" m "|->" d "p" p)
  (let [nmappings (distinct (map vector m d))] ;[a b],[c d] |-> [a c] [b d]
    ;(println "nmappings" nmappings)
    (when (and
           (apply distinct? (map second nmappings)) ;any contradicting maps?
           (every? (fn [[a b]]
                     ;(println a b)
                     (or
                      ;or none should map to b
                      (empty? (filter #(contains? % b) (vals p)))
                      ;or a is the only source, and maps to it
                       ;(= [a] (filter #(contains? % b) (vals p)))
                      (contains? (p a) b) ;if we have it, it should match
                      ))
                   nmappings))
      (reduce (fn [pp [a b]]
                (assoc pp a (into #{b} (pp a))))
              p
              nmappings))))

(repconjugators [2 0 1])

(realize-a-mapping [0 2] [3 2] {0 #{1}, 1 #{0}})

(defn all-realizations
  "All realizations of a desired map `target` using available mappings in
   `sources`, compatible with the partial permutation p. Just systematically
   trying to realize all mappings. It returns a vector of remaining sources -
   extended permutation pairs."
  [sources p target]
  ;(println "aiming" target "sources" sources "p" p)
  (reduce
   (fn [psols m]
     (let [res (realize-a-mapping m target p)]
       (if (nil? res)
         psols
         (conj psols [(disj sources m) res]))))
   []
   ;(sort sources)
   sources)) ;sorting just for the display

(defn merge-psols
 [psols]
 (let [grouped (group-by first psols)]
   ;(println "grouped" grouped)
   (map
    (fn [waiting]
      (if (= 1 (count (grouped waiting)))
        (first (grouped waiting))
        [waiting (reduce hmpm (map second (grouped waiting)))]))
    (keys grouped))))

(repconjugators [3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3])

(defn repconjugators
  [t]
  (let [results (reduce
                 (fn [psols k]
                          ;(println "k" k "#psols" (count psols) "vs")
                   ;(let [hm (group-by first  psols)]
                    ; (doseq [key (keys hm)]
                     ;  (println key ":" (map second (hm key)))))
                   ;; we try to match partial solutions to [k 0], [k 1], ...
                   (loop [targets (for [i (range (count t))] [k i])]
                     (let [target (first targets)
                           results (mapcat (fn [[sources p]]
                                             (all-realizations sources p target))
                                           psols)]
                       (if (empty? results)
                         (recur (rest targets))
                         (merge-psols results)))))
                 [[(set (t-c/single-maps t)) {}]]
                 (range (count t)))]
    ;; (let [hm (group-by first  results)]
    ;;   (doseq [key (keys hm)]
    ;;     (println key ":" (map second (hm key)))))
    ;(map (comp t-c/hash-map2perm second) results)
    (second (first results))))

(repconjugators [2 2 2 3 3 0 0])

(defn hash-map2perm2
  "Turning a permutation represented as a hash-map into a vector, the
   canonical transformation representation."
  [m]
  (println m)
  (let [singletons (filter #(= 1 (count (m %))) (keys m))
        nonsingletons (remove (set singletons) (keys m))
        ;apperantly there can be imgs contradicting singleton maps todo: why?
        singletonimgs (reduce into #{} (map m singletons))
        m' (reduce (fn [mm k]
                     (update mm k (comp set (partial remove singletonimgs))))
                   m
                   nonsingletons)]
    (println m')
    (first (reduce
            (fn [[p used] i]
              (let [img (first (remove used (m' i)))]
                [(conj p img) (conj used img)]))
            [[] #{}]
            (range (count m))))))
(def m {2 #{0 1 2}, 0 #{0 1 2}, 1 #{0 1 2}})
(reduce into (map m #{}))

(hash-map2perm2 {0 #{0}, 7 #{3}, 1 #{0 1}, 4 #{7}, 6 #{3}, 3 #{4 6 3 5}, 2 #{2}, 9 #{9 10}, 5 #{7}, 10 #{9 10}, 8 #{4 6 3 5}})

(defn conjrep
  [t]
  (t/conjugate t (hash-map2perm2  (repconjugators t))))

(conjrep [1 0 0 7 7 6 8 6  3 4 5 ])
(repconjugators [1 0 0 7 7 6 8 6  3 4 5])

(def troub (filter (fn [t] (not= (conjrep t) (t-c/conjrep t)))
                   (selections (range 3 ) 3)))

(repconjugators [2 0 1])
(t-c/conjrep [2 0 1])
(t-c/repconjugators [1 0 2])
(t-c/conjrep [1 0 2])
(conjrep [2 0 1])

(map (fn [t] (t/conjugate [2 0 1] (t-c/hash-map2perm t))) [{1 0, 0 1, 2 2} {0 0, 2 1, 1 2} {2 0, 1 1, 0 2}])

(map   t-c/hash-map2perm  [{1 0, 0 1, 2 2} {0 0, 2 1, 1 2} {2 0, 1 1, 0 2}])


(def sets [[[1 2 1 2 1] [3 3 1 2 0] [4 1 3 0 2]]
           [[3 3 1 2 0] [4 1 3 0 2]]
           [[1 2 1 2 1] [0 0 1 2 0] [4 1 3 0 2] [1 2 1 2 1] [0 1 0 1 0]]])

(t-c/setconjrep (nth sets 0))
(c/setconjrep t/conjugate (nth sets 0) S5)
(c/setconjrep t/mul (nth sets 0) S5)
(can-set-seq (nth sets 0))

(t-c/conjrep [1 0 0])
(c/conjrep-by-minimum t/conjugate [1 0 0] S3)


(t-c/conjrep [1 0 1])
(t-c/conjrep_old [1 0 1])
(t-c/conjrep [1 0 1 1])
(t-c/conjrep_old [1 0 1 1])

(filter #(not= (t-c/conjrep %) (t-c/conjrep_old %)) T3)


(conjrep [1 0 0])
(conjrep [2 2 2])
(conjrep [3 3 3 3])
(conjrep [4 4 4 4 4])
(conjrep [5 5 5 5 4 5])
(conjrep [6 1 6 6 6 6 6 6 6 6 2 5 3 4 2 6 1])
(conjrep [4 3 2 1 0])
(t-c/conjrep [4 3 2 1 0])
(c/conjrep-by-minimum t/conjugate [4 3 2 1 0] S5)


(defn targets
  [n i]
  (map (fn [j] [j i]) (range n)))

(defn conjrep-backtrack
  "e"
  [s all?]
  (let [N (count s) ;; te number of points
        ]
    ;(println "mappings" mappings)
    (loop [n 0 ;the number of mappings matched so far
           pperms [{}]
           sources [(set (t-c/single-maps s))]
           next-targets [[0 0]]
           solutions []]
      (println "n" n "pperms" pperms "tgs" tgs)
      (if (empty? pperms) ;when bactracked too far, return the solutions
        solutions
        ;; we try the next target
        (if-not (< (first (peek next-targets)) N)
          (recur (dec n) ;backtrack when no more targets, decrease n,
                 (pop pperms) ;popping vectors
                 (pop next-targets)
                 (pop next-targets)
                 solutions) ;solution just carried over
          (let [pperm (peek pperms)
                ntg (first (peek tgs))
                np (t-c/realize-a-mapping (sources n) ntg pperm)]
            (println "got" np)
            (cond
              (nil? np) (recur n
                               pperms
                               (conj (pop tgs) (rest (peek tgs)))
                               solutions)
                ;if it is a solution, just return it or collect it
              (= n N)
              (if all?
                (recur n ;collecting all solutions, so move on
                       pperms
                       (conj (pop tgs) (rest (peek tgs)))
                       (conj solutions np))
                [np]) ;the first solution
                ;not a solution, but good, so add a new generator
              :else (recur (inc n)
                           (conj pperms np)
                           (conj (conj (pop tgs) (rest (peek tgs))) (targets N (inc n)))
                           solutions))))))))
(conjrep-backtrack [2 2 2] true)

