(require '[kigen.semigroup.sgp :refer [sgp-by-gens index-period]])
(require '[kigen.diagram.transf :as t])

(def T7 (sgp-by-gens (t/full-ts-gens 7) t/mul))
;(def S7 (sgp-by-gens (t/symmetric-gens 7) t/mul))


(sort (map count (vals (group-by (partial index-period t/mul) T7))))

(doseq [[ [i p] c]
        (sort-by second (seq (update-vals (group-by (partial index-period t/mul) T7) count)))]
  (println "$(" i "," p ")$ & " c "\\\\"))