(require '[clojure.math.combinatorics :as combo])
(require '[kigen.transducer.common :refer :all])
(require '[kigen.transducer.flexible :as f])
(require '[kigen.transducer.from-trajectories :as ft])
(require '[taoensso.timbre :as timbre])
(require '[kigen.transducer.viz :refer [DotTransducer Dot2PDF]])
(require '[kigen.transducer.trie :as tr])
(require '[kigen.transducer.minimal :refer [minimize-transducer]])

(timbre/set-min-level! :info)

(def parity
  [[[0 0] 0]
   [[0 1] 1]
   [[1 0] 1]
   [[1 1] 0]])
(def parity_transducer (first (f/transducer parity 2)))
(Dot2PDF (DotTransducer parity parity_transducer) "parity")

(def sl-9-3
  [[[1 0 0  0 0 0  0 0 0] 1]
   [[0 1 0 0 0 0 0 0 0] 1]
   [[0 0 1 0 0 0 0 0 0] 1]
   [[0 0 0 1 0 0 0 0 0] 2]
   [[0 0 0 0 1 0 0 0 0] 2]
   [[0 0 0 0 0 1 0 0 0] 2]
   [[0 0 0 0 0 0 1 0 0] 3]
   [[0 0 0 0 0 0 0 1 0] 3]
   [[0 0 0 0 0 0 0 0 1] 3]])
(Dot2PDF (DotTransducer sl-9-3 (partial-transducer sl-9-3 (first (f/transducer sl-9-3 5)))) "sl-9-3")
;(experiment "sl-9-3 flexible" sl-9-3 4 (comp first ft/transducer))
;(def ft-sl-9-3 (ft/transducer sl-9-3 5)) too much
(minimize-transducer (tr/transducer sl-9-3))

(def sl-8-4
  [[[1 0 0 0 0 0 0 0] :1]
   [[0 1 0 0 0 0 0 0] :1]
   [[0 0 1 0 0 0 0 0] :2]
   [[0 0 0 1 0 0 0 0] :2]
   [[0 0 0 0 1 0 0 0] :3]
   [[0 0 0 0 0 1 0 0] :3]
   [[0 0 0 0 0 0 1 0] :4]
   [[0 0 0 0 0 0 0 1] :4]])
(Dot2PDF (DotTransducer sl-8-4 (partial-transducer sl-8-4 (first (f/transducer sl-8-4 6)))) "sl-8-4")
;(experiment "sl-9-3 flexible" sl-9-3 4 (comp first ft/transducer))
;(def ft-sl-9-3 (ft/transducer sl-9-3 5)) too much
(Dot2PDF (DotTransducer sl-8-4 (minimize-transducer (tr/transducer sl-8-4))) "sl-8-4classic")

;; deciding whether there are more zeroes or ones, or equal
(defn zof
  [n]
  (mapv (fn [l]
          [(vec l) (let [ones (count (filter #{1} l))
                         zeroes (count (filter #{0} l))]
                     (cond
                       (< zeroes ones) :more1s
                       (= zeroes ones) :eq
                       :else :more0s))])
        (mapcat #(combo/selections [0 1] %) (range 1 (inc n)))))

(def zo4 (zof 4))
;(experiment "more zeroes or more ones flexible - max 5"
;            zo5 6 f/transducer)
(Dot2PDF (DotTransducer zo4 (partial-transducer zo4 (first (f/transducer zo4 5)))) "zo4")

(Dot2PDF (DotTransducer zo4 (partial-transducer zo4 (minimize-transducer (tr/transducer zo4)))) "zo4classic")