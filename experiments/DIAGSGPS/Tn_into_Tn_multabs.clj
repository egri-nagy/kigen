(require '[kigen.transf :as t]
         '[kigen.multab :as multab]
         '[kigen.morphism :as morphism])

(defn mtTn [n]
  (multab/multab
   (t/sgp-by-gens (t/full-ts-gens n))
   t/mul))

(doseq [i (range 1 5) j (range 1 (inc i)) ]
  (println "T" j "->" "T" i)
  (println (count (morphism/isomorphisms (mtTn j) (mtTn i)))))
