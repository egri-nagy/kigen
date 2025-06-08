;; counting the embeddings of T3 into Br8
(require '[kigen.transf :as t]
         '[kigen.pbr :as pbr]
         '[kigen.brauer :as br]
         '[kigen.sgp :as sgp]
         '[kigen.genmorph :as gmorph]
         '[clojure.pprint :refer [pprint]]
         '[taoensso.timbre :as timbre])

(timbre/set-min-level! :trace)

(def T3gens [[0 0 2]
             [1 2 0]
             [1 0 2]])


(binding [orbit.extension/*task-size* 64]
 (println (first
           (gmorph/call-embedding T3gens t/mul
                                  (br/brauer-gens 6) br/mul))))

