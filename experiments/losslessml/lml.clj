(require '[clojure.core.logic :as l])
(require '[clojure.core.logic.fd :as fd])
(require '[kigen.logic :as kl])
(require '[clojure.math.combinatorics :as combo])
(require '[kigen.transducer :refer :all])
(require '[taoensso.timbre :as timbre])
(require '[kigen.transf-conj :as t-c])
(require '[kigen.position :refer [index]])

;;to see trace messages by construct-transducer
;(timbre/merge-config! {:min-level :trace})

(timbre/set-min-level! :info)

(defn output-fn
  "Returns all collected output symbols appearing in the input-output
   pairs without repetition. Returned as a vector, the indices can be used
   to refer to the symbols. There is no promise on the order, since the output
   values can be of any type, not even guaranteed to be comparable."
  [io-pairs]
  (vec (distinct (map second io-pairs))))

(defn flexible-output-transducer
  "Given the input-output pairs, and the number of states, this attempts to
  construct a suitable transducer.
   It produces all solutions lazily, so if only a solution is needed, first
   can be used."
  [io-pairs n]
  (let [num-of-inputs (count (distinct (mapcat first io-pairs)))
        output-symbols (output-fn io-pairs)
        output-generator   num-of-inputs
        modded-io-pairs (for [[input output] io-pairs]
                          [(vec (concat input [ output-generator]))
                           (index output-symbols output)])
        outputs (fd/interval 0 (dec (count output-symbols)))
        states (fd/interval 0 (dec n))

        A  (vec (repeatedly (inc num-of-inputs)
                            (fn [] (vec (repeatedly n l/lvar)))))
        state-lvars (apply concat (butlast A))
        output-lvars (last A)]
    (timbre/info (count state-lvars) "logic variables for"
             n "states"
             num-of-inputs "symbols"
             modded-io-pairs \newline
             (count (apply concat A)) \newline
             output-lvars
             \newline A
             \newline outputs output-symbols states)
    (l/run* [q]
            (l/everyg #(fd/in % states) state-lvars)
            (l/everyg #(fd/in % outputs) output-lvars)
            (l/everyg (fn [[input output]]
                        (process-wordo A n 0 input output))
                      modded-io-pairs)
            (l/== q A))))

(defn display 
  [output output-map]
  [(butlast output) (map output-map (last output))])

;;signal locators
(def signal-locator-io
  [[[1 0 0] :first]
   [[0 1 0] :second]
   [[0 0 1] :third]])
(display
 (first (flexible-output-transducer signal-locator-io 4))
 (output-fn signal-locator-io))

(def signal-locator-io2
  [[[1 0 0  0 0 0 ] :1]
   [[0 1 0 0 0 0 ] :2]
   [[0 0 1 0 0 0 ] :3]
   [[0 0 0 1 0 0 ] :4]
   [[0 0 0 0 1 0 ] :5]
   [[0 0 0 0 0 1 ] :6]])
(flexible-output-transducer signal-locator-io2 6)

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

;;counting ones in 01-sequences: length of input word + 1 states needed
 (flexible-output-transducer
  (map (fn [l]
         [l (count (filter #{1} l))])
       (combo/selections [0 1] 3)) 4)

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
                      (combo/selections [0 1] 4)) 9)))

