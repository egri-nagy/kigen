;; counting the embeddings of T3 into Br8
(require '[kigen.transf :as t]
	 '[kigen.pbr :as pbr]
	 '[kigen.sgp :as sgp]
         '[kigen.genmorph :as gmorph]
         '[clojure.pprint :refer [pprint]]
         '[taoensso.timbre :as timbre])

(timbre/set-min-level! :trace)

(def result (gmorph/sgp-embeddings-by-gens (t/full-ts-gens 3) t/mul
                                           (pbr/brauer-gens 6) pbr/mul))

(println (count result))

