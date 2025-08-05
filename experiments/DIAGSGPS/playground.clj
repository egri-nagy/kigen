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

