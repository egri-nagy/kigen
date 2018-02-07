(use '[kigen.multab :as multab])
(use '[kigen.transf :as t])

(def mtS5
  (multab/multab (t/sgp-by-gens (t/symmetric-gens 5))
                 t/mul))

(println "Parallel S5")

(binding [orbit.extension/*task-size* 32]
  (time (count (multab/psubsgps mtS5))))

(println "Sequential S5")

  (time (count (multab/subsgps mtS5)))


(println)


