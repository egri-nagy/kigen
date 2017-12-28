;; counting embeddings of full transformation semigroups into full ts'
(require '[kigen.transf :as t]
         '[kigen.genmorph :as gmorph])

(defn Dn-into-Dm [gf n m]
  (let [Tngens (gf n)
        Tmgens (gf m)
        targets (gmorph/index-period-matched Tngens t/mul
                                             Tmgens t/mul)]
    (gmorph/embeddings-distinct Tngens t/mul targets t/mul
                                t/conjrep t/conj-conj t/setconjrep)))

(defn Dn->Dm-table [gf n]
  (let [pairs (for [i (range 1 (inc n))
                    j (range 1 (inc i))]
                [j i])]
    (map (fn [[m n]]
           [[m n] (count (Dn-into-Dm gf m n))])
         pairs)))

(println (Dn->Dm-table t/full-ts-gens 5))
