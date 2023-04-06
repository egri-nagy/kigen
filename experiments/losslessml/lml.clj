(require '[clojure.core.logic :as l])
(require '[clojure.core.logic.fd :as fd])
(require '[clojure.math.combinatorics :as combo])
(require '[kigen.transducer :refer :all])
(require '[taoensso.timbre :as timbre])
;(require '[kigen.transf-conj :as t-c])


;;to see trace messages by construct-transducer
;(timbre/merge-config! {:min-level :trace})
(timbre/set-min-level! :info)

;;signal locators
(def signal-locator-io
  [[[1 0 0] :first]
   [[0 1 0] :second]
   [[0 0 1] :third]])
(format-flexible signal-locator-io
                 (first (flexible-output-transducer signal-locator-io 4)))

(def signal-locator-io2
  [[[1 0 0  0 0 0] :1]
   [[0 1 0 0 0 0] :2]
   [[0 0 1 0 0 0] :3]
   [[0 0 0 1 0 0] :4]
   [[0 0 0 0 1 0] :5]
   [[0 0 0 0 0 1] :6]])
(format-flexible
 signal-locator-io2
 (first (flexible-output-transducer signal-locator-io2 6)))

(def signal-locator-io3
  [[[1 0 0  0 0 0  0 0 0] :1]
   [[0 1 0 0 0 0 0 0 0] :1]
   [[0 0 1 0 0 0 0 0 0] :1]
   [[0 0 0 1 0 0 0 0 0] :2]
   [[0 0 0 0 1 0 0 0 0] :2]
   [[0 0 0 0 0 1 0 0 0] :2]
   [[0 0 0 0 0 0 1 0 0] :3]
   [[0 0 0 0 0 0 0 1 0] :3]
   [[0 0 0 0 0 0 0 0 1] :3]])
(first (flexible-output-transducer signal-locator-io3 5))

;; T has 4 states and 3 input symbols
(def T [[0 3 1 3] ;states transformed by input symbol 0
        [1 2 0 3]
        [1 0 2 3]])

(def i-o-pairs
  (for [w (repeatedly 25
                      (fn [] (vec (repeatedly 4
                                              (partial rand-int 3)))))]
    [w (process-word T 0 w)]))

;is it uniquely determined?
(first (fixed-output-transducer i-o-pairs 4))

;;counting ones in 01-sequences: length of input word + 1 states needed
(flexible-output-transducer
 (map (fn [l]
        [l (count (filter #{1} l))])
      (combo/selections [0 1] 3)) 4)

;; deciding whether there are more zeroes or ones, or equal
;; not easy, for 4 inputs minimum 9 states needed - better with flexible output?
(def zo
  (mapv (fn [l]
          [(vec l) (let [ones (count (filter #{1} l))
                         zeroes (count (filter #{0} l))]
                     (cond
                       (< zeroes ones) :moreones
                       (= zeroes ones) :eq
                       :else :morezeros))])
        (mapcat #(combo/selections [0 1] %) [2 3 4])))

(def zosol (first (flexible-output-transducer zo 6)))
(format-flexible zo zosol)
(trajectories-flexible zo zosol)

;;old method - seven states
(def zo2
  (mapv (fn [l]
          [(vec l) (let [ones (count (filter #{1} l))
                         zeroes (count (filter #{0} l))]
                     (cond
                       (< zeroes ones) 1
                       (= zeroes ones) 2
                       :else 3))])
        (combo/selections [0 1] 4)))

(def zo2sol (first (fixed-output-transducer zo2 7)))
(check-fixed zo2 zo2sol)
(trajectories-fixed zo2 zo2sol)

(def binary
  [[[0 0 0] :0]
   [[0 0 1] :1]
   [[0 1 0] :2]
   [[0 1 1] :3]
   [[1 0 1] :5]
   [[1 1 0] :6]
   [[1 1 1] :7]])
(def binarysol  (first (flexible-output-transducer binary 8)))
(trajectories-flexible binary binarysol)
(check-flexible binary binarysol)