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

;; (def S5 (sgp-by-gens (t/symmetric-gens 5) t/mul))
;; (def T5 (sgp-by-gens (t/full-ts-gens 5) t/mul))

;; (def S4 (sgp-by-gens (t/symmetric-gens 4) t/mul))
;; (def T4 (sgp-by-gens (t/full-ts-gens 4) t/mul))

(def S3 (sgp-by-gens (t/symmetric-gens 3) t/mul))
(def T3 (sgp-by-gens (t/full-ts-gens 3) t/mul))

;; (def S6 (sgp-by-gens (t/symmetric-gens 6) t/mul))
;; (def T6 (sgp-by-gens (t/full-ts-gens 6) t/mul))

(= (count (map can-seq T3))
   (count (map t-c/conjrep T3)))

(let
 [ tmul t/mul
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

(can-set-seq [[1 1 2] [2 1 2]])
(can-set-seq [[2 2 3] [3 2 3]])
         
(time (def r1 (group-by #(c/setconjrep t/mul % S6) (selections T6 1))))
(count r1)

(time (def r2 (group-by can-set-seq (selections T6 1))))
(count r2)