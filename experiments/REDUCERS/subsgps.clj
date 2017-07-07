(use '[criterium.core])
(use '[clojure.data.int-map :as i])
(use '[orbit.core :as orbit])

(def mtS
  (multab/multab (t/sgp-by-gens [[1 3 0 3] [2 2 1 3] [1 1 2 2]])
                 t/mul))

(defn subsgps-single
  "All subsemigroups of an abstract semigroup given by its multiplication
  table"
  [mt]
  (let [elts (multab/elts mt)]
    (orbit/full-orbit-single [(i/int-set)] (partial multab/min-extensions mt elts))))

  (defn subsgps-parallel
    "All subsemigroups of an abstract semigroup given by its multiplication
  table"
    [mt]
    (let [elts (multab/elts mt)]
      (orbit/full-orbit-parallel [(i/int-set)] (partial min-extensions mt multab/elts))))
  

(println "Single")



  
  (bench
   (subsgps-single mtS))


(println "Parallel")

  (bench
   (subsgps-parallel mtS))

  
(println "Parallel 32")

(binding [orbit.core/*task-size* 32]
  (bench
   (subsgps-parallel mtS)))

(println)
