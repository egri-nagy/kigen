(require '[clojure.core.logic :as l])
(require '[clojure.core.logic.fd :as fd])
(require '[kigen.logic :as kl])
(require '[clojure.math.combinatorics :as combo])
(require '[kigen.transducer :refer [construct-transducer process-word]])
(require '[taoensso.timbre :as timbre])
(require '[kigen.transf-conj :as t-c])

;;to see trace messages by construct-transducer
(timbre/merge-config! {:min-level :trace})


;;counting ones in 01-sequences: length of input word + 1 states needed
 (construct-transducer
        (map (fn [l]
               [l (count (filter #{1} l))])
             (combo/selections [0 1] 4)) 5)

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
(construct-transducer signal-locator-io 4)

(def signal-locator-io2
  [[[1 0 0  0 0 0 ] 1]
   [[0 1 0 0 0 0 ] 1]
   [[0 0 1 0 0 0 ] 2]
   [[0 0 0 1 0 0 ] 2]
   [[0 0 0 0 1 0 ] 3]
   [[0 0 0 0 0 1 ] 3]])
 (construct-transducer signal-locator-io2 5)

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

;; T has 4 states and 3 input symbols
(def T [[ 0 3 1 3 ] ;states transformed by input symbol 0
        [ 1 2 0 3 ] 
        [ 1 0 2 3 ]])

(def i-o-pairs
  (for [w (repeatedly 25
                      (fn [] (vec (repeatedly 4
                                              (partial rand-int 3)))))] 
    [w (process-word T 4 0 w)]))

;is it uniquely determined?
(first (construct-transducer i-o-pairs 4))
