;; counting embeddings of full diagram semigroups into other full diagram
;; semigroups of the same type
;; generic code for diagram semigroups using the transformation representation
(require '[kigen.transf :as t]
         '[kigen.conjugacy :as c]
         '[kigen.transf-conj :as t-c]
         '[kigen.genmorph :as gmorph]
         '[clojure.pprint :refer [pprint]]
         '[taoensso.timbre :as timbre])

(timbre/merge-config! {:level :info})

(defn Dn-into-Dm [gf n m]
  (let [Tngens (gf n)
        Tmgens (gf m)
        targets (gmorph/index-period-matched (gmorph/->Sgp Tngens t/mul)
                                             Tmgens t/mul)]
    (gmorph/embeddings-distinct Tngens t/mul targets t/mul
                                ;;(c/conjugation-fn-bundle
                                ;;  t/conjugate
                                ;;  (t/sgp-by-gens (t/symmetric-gens m))))))
                                (c/->ConjugationFunctionBundle t-c/conjrep
                                                               t-c/setconjrep
                                                               t-c/conj-conj))))

(defn Dn->Dm-table [gf n]
  (let [pairs (for [i (range 1 (inc n))
                    j (range 1 (inc i))]
                [j i])]
    (map (fn [[m n]]
           (let [r (count (Dn-into-Dm gf m n))]
             (println \# m n r)
             [[m n] r]))
         pairs)))

(binding [orbit.extension/*task-size* 1]
  (def result (Dn-into-Dm t/full-ts-gens 5 6)))

(println (count result))

;;(pprint result)
