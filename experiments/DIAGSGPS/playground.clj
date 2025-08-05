(require '[kigen.semigroup.genmorph :refer :all])
(require '[kigen.semigroup.sgp :refer [sgp-by-gens]])
(require '[kigen.diagram.transf :as transf])
(require '[kigen.semigroup.conjugacy :as c])

;(deftest gentab)

(def T2gens (transf/full-ts-gens 2))
(def T3gens (transf/full-ts-gens 3))

(call-embedding T2gens transf/mul T3gens transf/mul)


(count (embedding-backtrack T2gens
                            transf/mul
                            (index-period-matched T2gens transf/mul T3gens transf/mul)
                            transf/mul true))

(count (sgp-embeddings-by-gens T2gens
                               transf/mul
                               T3gens
                               transf/mul))


(def r
  (let [T1gens (transf/full-ts-gens 1)
        T4gens (transf/full-ts-gens 4)
        S3 (sgp-by-gens (transf/symmetric-gens 3) transf/mul)
        S4 (sgp-by-gens (transf/symmetric-gens 4) transf/mul)
        tmul transf/mul
        tconj transf/conjugate]
    (sgp-embeddings-by-gens T2gens tmul
                            T3gens tmul
                            (c/conjugation-fn-bundle tconj S3))))


(map (fn [gens] (sgp-by-gens (map second gens) transf/mul)) r)

(def S3 (sgp-by-gens (transf/symmetric-gens 3) transf/mul))

(defn onpt
  [pt p]
  (p pt))

(c/seqconjrep onpt [1 2 1]  S3)

(c/conj-conj onpt [[] S3] 2)

(reductions
 (fn [X pt]
   (c/conj-conj onpt X pt))
 [[] S3]
 [2 2 1 0 1 2])


(defn canonical-labeling
  "Canonical labeling of a transformation."
  ([t] (canonical-labeling [{} 0] t))
  ([CL t]
   (reduce
    (fn [[perm available] point]
      (if (contains? perm point)
        [perm available]
        [(conj perm [point available]) (inc available)]))
    CL
    t)))


(defn seq-canonical-labeling
  "Canonical labeling of a sequence of transformations."
  [ts]
  (let [[p _] (reduce canonical-labeling
                      (canonical-labeling (first ts))
                      (rest ts))]
    [(mapv (partial mapv p) ts)
     p]))

;; the idea: get the minimal one(s) and extend
;; data structure [ processed remaining pperm]
(defn set-canonical-labeling
  [ts]
  (let [CLs (map canonical-labeling ts)]))

(defn boo
  "processing one partially created conjrep
   `remaining` - the set of unprocessed transformations"
  [[processed remaining CL]]
  (let [ts2CL (zipmap remaining
                      (map (partial canonical-labeling CL) remaining))
        tts2ts (group-by (fn [t] (mapv (first (ts2CL t)) t)) remaining)
        minrep (first (sort compare (keys tts2ts)))]
    (zipmap (tts2ts minrep) (map  ts2CL (tts2ts minrep)))
    (for [t (tts2ts minrep)]
      [(conj processed minrep)
       (remove #{t} remaining)
       (ts2CL t)])))

(boo [[] ts [{} 0]])

(loop [labelings [ [[] ts [{} 0]] ]]
  (if (empty? (second (first labelings)))
    (map (fn [[ rep _ p]] [rep p]) labelings)
    (let [nlabelings (mapcat boo labelings)
          latest2labs (group-by (comp peek first) nlabelings)
          minrep (first (sort compare (keys latest2labs)))]
      (recur (latest2labs minrep)))))


(def ts [[0 0 0]
         [1 1 1]
         [2 2 2]])

(zipmap ts
        (map (partial canonical-labeling) ts))

(boo0 ts)
(boo [[] ts [{} 0]])

(map canonical-labeling ts)