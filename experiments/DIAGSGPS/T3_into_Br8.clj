;; counting the embeddings of T3 into Br8
(require '[kigen.diagram.transf :as t]
         '[kigen.diagram.pbr :as pbr]
         '[kigen.diagram.brauer :as br]
         '[kigen.semigroup.sgp :as sgp]
         '[kigen.semigroup.genmorph :as gmorph]
         '[clojure.pprint :refer [pprint]]
         '[taoensso.timbre :as timbre])

(timbre/set-min-level! :trace)

(def T3gens [[0 0 2]
             [1 2 0]
             [1 0 2]])


;; this gives nil after a week or so
;; (binding [orbit.extension/*task-size* 64]
;;  (println (first
;;            (gmorph/sgp-embeddings-by-gens T3gens t/mul
;;                                   (br/brauer-gens 8) br/mul))))

(binding [orbit.extension/*task-size* 64]
  (println (first
            (gmorph/call-embedding [[1 2 0] [0 0 2]] t/mul
                                   (br/brauer-gens 5) br/mul))))
