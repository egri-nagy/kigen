;; counting embeddings of full diagram semigroups into other full diagram
;; semigroups of the same type
;; generic code for diagram semigroups using the transformation representation
(require '[kigen.transf :as t]
         '[kigen.genmorph :as gmorph]
         '[clojure.pprint :refer [pprint]])

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
    	   (let [r (count (Dn-into-Dm gf m n))]
             (println \# m n r)
             [[m n] r]))
         pairs)))

(def result (Dn->Dm-table t/full-ts-gens 8))

(count result)

(pprint result)