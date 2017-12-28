;; counting embeddings of full transformation semigroups into full ts'
(require '[kigen.transf :as t]
         '[kigen.genmorph :as gmorph])

(defn TnintoTm [n m]
  (let [Tngens (t/full-ts-gens n)
        Tmgens (t/full-ts-gens m)
        targets (gmorph/index-period-matched Tngens t/mul
                                             Tmgens t/mul)]
    (gmorph/embeddings-distinct Tngens t/mul targets t/mul
                                t/conjrep t/conj-conj t/setconjrep)))

(defn Tn->Tm-table [n]
  (let [pairs (for [i (range 1 (inc n))
                    j (range 1 (inc i))]
                [j i])]
    (map (fn [[m n]]
           [[m n] (count (TnintoTm m n))])
         pairs)))

(println (Tn->Tm-table 5))
