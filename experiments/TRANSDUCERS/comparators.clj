(require '[clojure.math.combinatorics :as combo])
(require '[kigen.transducer.common :refer :all])
(require '[kigen.transducer.flexible :as f])
(require '[kigen.transducer.from-trajectories :as ft])
(require '[kigen.transducer.trie :as trie])
(require '[taoensso.timbre :as timbre])
(require '[kigen.transducer.viz :refer [DotTransducer Dot2PDF]])

;; levels: :warn, :info, :debug
(timbre/set-min-level! :info)

;; comparators
;; deciding whether there are more zeroes or ones, or equal
;; obvious solution: we need to count how many more ones we saw, or
;; how many more zeroes we saw.
;; Due to finiteness, we don't need to count more than half of the input length

(defn compared-bitstrings
  "Returns all bitstrings up to length n, labeled according to the
   comparisons of zeroes and ones."
  [n]
  (mapv (fn [l]
          [(vec l) (let [ones (count (filter #{1} l))
                         zeroes (count (filter #{0} l))]
                     (cond
                       (< zeroes ones) :moreones
                       (= zeroes ones) :eq
                       :else :morezeros))])
        (mapcat #(combo/selections [0 1] %) (range 1 (inc n)))))

(def zo3 (compared-bitstrings 3))
(experiment "more zeroes or more ones flexible - max 3"
            zo3 (comp first #(f/transducer % 5)))

;interestingly this is the same, because we need to count up to two anyway
(def zo4 (compared-bitstrings 4))
(experiment "more zeroes or more ones flexible - max 4"
            zo4 (comp first #(f/transducer % 5)))

(def zo5 (shuffle (compared-bitstrings 5)))
;long calculation - shuffling helps to ease memory consumption - yes,
; the order of the input-output pairs does matter
;(experiment "more zeroes or more ones flexible - max 5"
;            zo5 6 f/transducer)
(experiment "more zeroes or more ones max 5 - trie"
            zo5 trie/transducer)

(trajectories zo5 (trie/transducer zo5))
(check zo5 (partial-transducer zo5 (trie/transducer zo5)))



