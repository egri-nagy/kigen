(use '[criterium.core])
(use '[clojure.data.int-map :as i])
(use '[orbit.core :as orbit])
(use '[kigen.multab :as multab])
(use '[kigen.transf :as t])

(def mtS5
  (multab/multab (t/sgp-by-gens (t/symmetric-gens 5))
                 t/mul))

(println "Single S5")

(binding [criterium.core/*sample-count* 2]
  (bench
   (multab/subsgps mtS5)))



(println "Parallel 32")

(binding [orbit.extension/*task-size* 32
          criterium.core/*sample-count* 2]
  (bench
   (multab/psubsgps mtS5)))

(println)

; (time (count (multab/subsgps (multab/multab (t/sgp-by-gens [[0,1,0,0,0] [1,3,0,4,0]]) t/mul))))
