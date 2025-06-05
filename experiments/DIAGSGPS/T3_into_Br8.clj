;; counting the embeddings of T3 into Br8
(require '[kigen.transf :as t]
         '[kigen.pbr :as pbr]
         '[kigen.sgp :as sgp]
         '[kigen.genmorph :as gmorph]
         '[clojure.pprint :refer [pprint]]
         '[taoensso.timbre :as timbre])

(timbre/set-min-level! :trace)

(def T3gens [[0 0 2]
             [1 2 0]
             [1 0 2] ])


(binding [orbit.extension/*task-size* 64]
  (println (first
            (gmorph/sgp-embeddings-by-gens T3gens t/mul
                                           (pbr/brauer-gens 5) pbr/mul))))

;; T3->B7 {[0 0 2] {7 #{6}, 1 #{3}, 4 #{11}, :cod #{13 12 11 9 14 10 8}, 13 #{10}, 6 #{7}, 3 #{1}, 12 #{8}, 2 #{5}, 11 #{4}, 9 #{14}, 5 #{2}, 14 #{9}, 10 #{13}, :dom #{7 1 4 6 3 2 5}, 8 #{12}}, [1 2 0] {7 #{10}, 1 #{9}, 4 #{11}, :cod #{13 12 11 9 14 10 8}, 13 #{2}, 6 #{8}, 3 #{12}, 12 #{3}, 2 #{13}, 11 #{4}, 9 #{1}, 5 #{14}, 14 #{5}, 10 #{7}, :dom #{7 1 4 6 3 2 5}, 8 #{6}}, [1 0 2] {7 #{8}, 1 #{14}, 4 #{11}, :cod #{13 12 11 9 14 10 8}, 13 #{3}, 6 #{10}, 3 #{13}, 12 #{2}, 2 #{12}, 11 #{4}, 9 #{5}, 5 #{9}, 14 #{1}, 10 #{6}, :dom #{7 1 4 6 3 2 5}, 8 #{7}}}


