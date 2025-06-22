(require '[kigen.semigroup.sgp :refer [sgp-by-gens]]
         '[kigen.diagram.transf :as t]
         '[kigen.table.multab :as multab]
         '[kigen.morphism :as morphism])

(defn mtTn [n]
  (multab/multab (sgp-by-gens (t/full-ts-gens n) t/mul)
                 t/mul))

(doseq [i (range 1 5) j (range 1 (inc i)) ]
  (println "T" j "->" "T" i)
  (println (count (morphism/isomorphisms (mtTn j) (mtTn i)))))