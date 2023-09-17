(require '[clojure.math.combinatorics :as combo])
(require '[kigen.transducer.common :refer :all])
(require '[kigen.transducer.flexible :as f])
(require '[kigen.transducer.from-trajectories :as ft])
(require '[taoensso.timbre :as timbre])
(require '[kigen.transducer.viz :refer [DotTransducer Dot2PDF]])

;; levels: :warn, :info, :debug
(timbre/set-min-level! :info)


;; deciding whether there are more zeroes or ones, or equal
(defn zof
  [n]
  (mapv (fn [l]
          [(vec l) (let [ones (count (filter #{1} l))
                         zeroes (count (filter #{0} l))]
                     (cond
                       (< zeroes ones) :moreones
                       (= zeroes ones) :eq
                       :else :morezeros))])
        (mapcat #(combo/selections [0 1] %) (range 1 (inc n)))))

(def zo3 (zof 3))
(experiment "more zeroes or more ones flexible - max 3"
            zo3 5 f/transducer)

;interestingly this is the same
(def zo4 (zof 4))
(experiment "more zeroes or more ones flexible - max 4"
            zo4 5 f/transducer)

;long calculation - shuffling helps to ease memory consumption - yes,
; the order of the input-output pairs does matter
;(def zo5 (shuffle (zof 5)))
;(experiment "more zeroes or more ones flexible - max 5"
;            zo5 6 f/transducer)

