(require '[clojure.core.logic :as l])
(require '[clojure.core.logic.fd :as fd])
(require '[kigen.logic :as kl])
(require '[clojure.math.combinatorics :as combo])
(require '[kigen.transducer :refer [construct-transducer process-word]])
(require '[taoensso.timbre :as timbre])

;;to see trace messages by construct-transducer
(timbre/merge-config! {:min-level :trace})

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;TEST cases
;; states are nonnegative integers, zero is the initial state
;; rows are input symbols, so one row is a transformation induced by the input

;;counting ones in 01-sequences: length of input word + 1 states needed
(count (construct-transducer
        (map (fn [l]
               [l (count (filter #{1} l))])
             (combo/selections [0 1] 5)) 6))

;; deciding whether there are more zeroes or ones, or equal
;; not easy, for 4 inputs minimum 9 states needed
(comment (first (construct-transducer
                 (map (fn [l]
                        [l (let [ones (count (filter #{1} l))
                                 zeroes (count (filter #{0} l))]
                             (cond
                               (< zeroes ones) 0
                               (= zeroes ones) 1
                               :else 2))])
                      (combo/selections [0 1] 4)) 9))
         )

;;signal locators
(def signal-locator-io
  [[[1 0 0] 1]
   [[0 1 0] 2]
   [[0 0 1] 3]])
(count (construct-transducer signal-locator-io 4))

(def signal-locator-io2
  [[[1 0 0  0 0 0 ] 1]
   [[0 1 0 0 0 0 ] 1]
   [[0 0 1 0 0 0 ] 2]
   [[0 0 0 1 0 0 ] 2]
   [[0 0 0 0 1 0 ] 3]
   [[0 0 0 0 0 1 ] 3]])
;(count (construct-transducer signal-locator-io2 5))

(def signal-locator-io3
  [[[1 0 0  0 0 0  0 0 0] 1]
   [[0 1 0 0 0 0 0 0 0] 1]
   [[0 0 1 0 0 0 0 0 0] 1]
   [[0 0 0 1 0 0 0 0 0] 2]
   [[0 0 0 0 1 0 0 0 0] 2]
   [[0 0 0 0 0 1 0 0 0] 2]
   [[0 0 0 0 0 0 1 0 0] 3]
   [[0 0 0 0 0 0 0 1 0] 3]
   [[0 0 0 0 0 0 0 0 1] 3]])
;(construct-transducer signal-locator-io3 6)

;; T has 5 states and 6 input symbols
(def T [[ 0 4 1 3 2] ;states transformed by input symbol 0
        [ 1 2 0 3 3]
        [ 1 2 3 4 0]
        [ 1 0 2 3 4]
        [ 0 1 0 1 0]
        [ 2 3 3 2 1]])

(def i-o-pairs
  (for [w (repeatedly 6 (fn [] (vec (repeatedly 6 (partial rand-int 6)))))]
    [w (process-word T 5 0 w)]))

(first (construct-transducer i-o-pairs 5))

; does this fully determine T? try this on bigger machine:
;; (first (construct-transducer
;;         [[[1 5 0 2 0 0] 1]
;;          [[1 0 3 3 1 5] 2]
;;          [[2 0 1 3 5 5] 3]
;;          [[4 4 5 4 3 3] 0]
;;          [[3 4 5 1 1 0] 3]
;;          [[3 3 3 2 3 1] 0]
;;          [[2 5 0 0 2 1] 3]
;;          [[4 4 0 1 2 5] 3]
;;          [[4 2 0 4 0 2] 1]
;;          [[4 5 1 0 0 2] 1]
;;          [[1 3 0 2 2 1] 0]
;;          [[5 4 5 4 5 5] 3]
;;          [[5 0 3 0 1 0] 4]
;;          [[1 3 4 3 5 2] 4]
;;          [[2 1 5 3 3 5] 2]
;;          [[4 2 5 0 3 2] 4]
;;          [[0 2 5 4 2 5] 3]
;;          [[5 2 3 4 3 3] 1]
;;          [[5 2 4 3 0 2] 1]
;;          [[0 5 5 0 2 2] 0]],5))