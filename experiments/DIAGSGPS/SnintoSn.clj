;; embedding the symmetric into the symmetric group
(require '[kigen.transf :as t]
         '[kigen.genmorph :as gmorph])

(defn SnintoSm [n m]
  (let [Sngens (t/symmetric-gens n)
        Smgens (t/symmetric-gens m)
        targets (gmorph/index-period-matched Sngens t/mul
                                             Smgens t/mul)]
    (gmorph/embeddings-distinct Sngens t/mul targets t/mul
                                t/conjrep t/conj-conj t/setconjrep)))

(defn Sn->Sm-table [n]
  (let [pairs (for [i (range 1 (inc n))
                    j (range 1 (inc i))]
                [j i])]
    (map (fn [[m n]]
           [[m n] (count (SnintoSm m n))])
         pairs)))

(println (Sn->Sm-table 6))
