(require '[kigen.semigroup.genmorph :refer :all])
(require '[kigen.semigroup.sgp :refer [sgp-by-gens]])
(require '[kigen.diagram.transf :as t])
(require '[kigen.diagram.transf-conj :as t-c])
(require '[kigen.semigroup.conjugacy :as c])
(require '[kigen.semigroup.genmorph :refer [sgp-embeddings-by-gens]])
(require '[kigen.canonical-labeling :refer [can-set-seq can-seq]])
(require '[clojure.math.combinatorics :refer [selections]])
(require '[taoensso.timbre :as timbre])

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

(= (count (map can-seq T3))
   (count (map t-c/conjrep T3)))

(let
 [tmul t/mul
  tconj t/conjugate]

  (sgp-embeddings-by-gens (t/full-ts-gens 2) tmul
                          (t/full-ts-gens 3) tmul
                          (c/conjugation-fn-bundle tmul S3)))

(def sets [[[1 2 1 2 1] [3 3 1 2 0] [4 1 3 0 2]]
           [[3 3 1 2 0] [4 1 3 0 2]]
           [[1 2 1 2 1] [0 0 1 2 0] [4 1 3 0 2] [1 2 1 2 1] [0 1 0 1 0]]])

(t-c/setconjrep (nth sets 0))
(c/setconjrep t/conjugate (nth sets 0) S5)
(c/setconjrep t/mul (nth sets 0) S5)
(can-set-seq (nth sets 0))

(t-c/conjrep [1 0])
(c/conjrep-by-minimum t/conjugate [1 0] S2)

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
           pperms [ {} ]
           sources [ (set (t-c/single-maps s))]
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
                [ np ]) ;the first solution
                ;not a solution, but good, so add a new generator
              :else (recur (inc n)
                           (conj pperms np)
                           (conj (conj (pop tgs) (rest (peek tgs))) (targets N (inc n)))
                           solutions))))))))
(conjrep-backtrack [2 2 2] true)

(defn calc-maps
  ""
  [sources pperm targets]
  (reduce
   (fn [r tg]
     (into r (reduce (fn [coll src]
                       (let [np (t-c/realize-a-mapping src tg pperm)]
                         (if np
                           (conj coll [(disj sources src) np])
                           coll)))
                     []
                     sources)))
   []
   targets))

(reduce
 (fn [coll [sources pperm]]
   (into coll (calc-maps sources pperm (targets 3 0))))
 []
 [ [(set (t-c/single-maps [1 2 0])) {}] ])

(reduce
 (fn [coll [sources pperm]]
   (into coll (calc-maps sources pperm (targets 3 1))))
 []
 [[#{[1 2] [0 1]} {2 1, 0 0}]
  [#{[2 0] [0 1]} {1 1, 2 0}]
  [#{[2 0] [1 2]} {0 1, 1 0}]
  [#{[1 2] [0 1]} {2 2, 0 0}]
  [#{[2 0] [0 1]} {1 2, 2 0}]
  [#{[2 0] [1 2]} {0 2, 1 0}]])

(map #(map % [1 2 0])
 (map second
      (reduce
       (fn [coll [sources pperm]]
         (into coll (calc-maps sources pperm (targets 3 2))))
       []
       [[#{[0 1]} {2 1, 0 0, 1 2}]
        [#{[2 0]} {1 1, 2 0, 0 2}]
        [#{[1 2]} {0 1, 1 0, 2 2}]
        [#{[1 2]} {2 2, 0 0, 1 1}]
        [#{[0 1]} {1 2, 2 0, 0 1}]
        [#{[2 0]} {0 2, 1 0, 2 1}]] )))

(let [m {0 2, 1 0, 2 1}
      n (count m)]
  (mapv m (range n)))

(t-c/conjrep [2 2 2 2])
(count (t-c/conjugators [5 5 5 5 5 5] [0 0 0 0 0 0 ]))
(t-c/conjugators [2 2 2] [0 1 0])

(t-c/conjrep [1 1 0])

(t-c/conjrep [9 9 9 9 9 9 9 9 9 9])

(calc-maps (set (t-c/single-maps [1 2 0])) {} (targets 3 0))


(conjrep-recur #{[1 2] [0 1]} {2 1, 0 0} (targets 3 1))
(conjrep-recur #{[2 0] [0 1]} {1 1, 2 0} (targets 3 1))

(targets 3 1)

(t-c/conjrep [2 2 2])
