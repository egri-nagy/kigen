(use '[kigen.multab :as multab])
(use '[kigen.transf :as t])

(def mtT
  (multab/multab (t/sgp-by-gens [[0,1,0,0,0] [1,3,0,4,0]])
                 t/mul))

(println "Parallel T")

(binding [orbit.extension/*task-size* 32]
  (time (count (multab/psubsgps mtT))))

(println "Sequential T")

  (time (count (multab/subsgps mtT)))


(println)


