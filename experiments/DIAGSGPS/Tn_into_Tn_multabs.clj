;; Brute-force enumeration of all T_n into T_m embeddings.
;; Very slow - only for checking purposes.
;; working with 25.06.xx
(require '[kigen.semigroup.sgp :refer [sgp-by-gens]]
         '[kigen.diagram.transf :as t]
         '[kigen.table.multab :as multab]
         '[kigen.table.multab-morphism :as morph])

(defn mtTn [n]
  (multab/multab (sgp-by-gens (t/full-ts-gens n) t/mul)
                 t/mul))

;; up to T4 the computation should be possible on any computer (TODO check T_4 -> T_4)
(doseq [i (range 1 5)
        j (range 1 (inc i)) ]
  (println "T" j "->" "T" i)
  (println (count (morph/isomorphisms (mtTn j) (mtTn i)))))