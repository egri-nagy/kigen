;; counting embeddings of full diagram semigroups into other full diagram
;; semigroups of the same type
;; generic code for diagram semigroups using the transformation representation
(require '[kigen.diagram.transf :as t]
         '[kigen.semigroup.conjugacy :as c]
         '[kigen.diagram.transf-conj :as t-c]
         '[kigen.semigroup.genmorph :as gmorph]
         '[clojure.pprint :refer [pprint]]
         '[taoensso.timbre :as timbre])

(timbre/set-min-level! :trace)

(defn Dm-into-Dn [gf m n]
  (let [Dngens (gf n)
        Dmgens (gf m)
        targets (gmorph/index-period-matched Dmgens t/mul
                                             Dngens t/mul)]
    (gmorph/embeddings-distinct
     Dmgens
     t/mul
     targets
     t/mul
     (c/->ConjugationFunctionBundle t-c/conjrep
                                    t-c/setconjrep
                                    t-c/conj-conj))))

(defn Dm->Dn-table [gf n]
  (let [pairs (for [i (range 1 (inc n))
                    j (range 1 (inc i))]
                [j i])]
    (map (fn [[m n]]
           (let [r (count (Dm-into-Dn gf m n))]
             (println \# m n r)
             [[m n] r]))
         pairs)))

(pprint (Dm->Dn-table t/full-ts-gens  5))