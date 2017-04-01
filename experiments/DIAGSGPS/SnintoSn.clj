;; embedding the symmetric into the symmetric group

(defn SnintoSm [n m]
  (let [Sngens (t/symmetric-gens n)
        Snips (map #(sgp/index-period % t/mul) Sngens)
        Sm (t/sgp-by-gens (t/symmetric-gens m))
        targets (map
                 #(filter
                   (fn [x] (= % (sgp/index-period x t/mul))) Sm)
                 Snips)]
    (gmorph/embeddings-conj Sngens t/mul targets t/mul
                            t/conjrep t/conj-conj t/setconjrep)))


(defn Sn->Sm-table [n]
  (let [pairs (for [i (range 1 (inc n)) j (range 1 (inc i)) ] [j i])]
    (map (fn [[m n]] [[m n] (count (SnintoSm m n))]) pairs)))


