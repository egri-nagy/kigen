(require '[clojure.core.logic :as l])
(require '[clojure.core.logic.fd :as fd])
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
   to refer to the symbols. The order of the symbols defined by the order
   of their appeareance in the io-pairs (through distinct)."
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
    (timbre/info ;bit of information about the processed input
     (+ (count state-lvars) (count output-lvars))
     "logic variables for"
     n "states"
     num-of-inputs "input symbols"
     (count output-symbols) "output symbols")
    (timbre/debug ;debug information about the modified input
     "modified io pairs" modded-io-pairs)
    (l/run* [q]
            (l/everyg #(fd/in % states) state-lvars)
            (l/everyg #(fd/in % outputs) output-lvars)
            (l/everyg (fn [[input output]]
                        (process-wordo A n 0 input output))
                      modded-io-pairs)
            (l/== q A))))

(defn check
  [io-pairs delta omega]
  (println "enetering check")
  (map ;we are going through all input-out pairs
   (fn [[input output]] ;representing one trajectory in a string
     (timbre/info input "->" output)
     (let [trajectory (reductions
                       (fn [q i] (nth (nth delta i) q)) ;state transition
                       0
                       input)
           final (omega (last trajectory))]
       (apply str (concat (map (fn [q i] (str q " "
                                              ;"(" (omega q) ") "
                                              "·" i " "))
                               trajectory
                               input)
                          [(last trajectory) " = " final
                           (if (= output final)
                             " ✔"
                             " ✘")]))))
   io-pairs))


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
;; not easy, for 4 inputs minimum 9 states needed - better with flexible output?
(def zo 
  (mapv (fn [l]
         [(vec l) (let [ones (count (filter #{1} l))
                  zeroes (count (filter #{0} l))]
              (cond
                (< zeroes ones) :moreones
                (= zeroes ones) :eq
                :else :morezeros))])
       (combo/selections [0 1] 4)))

(def zosol (first (flexible-output-transducer zo 4)))
(def zoout (output-fn zo))
 [zo zosol zoout]
(check
 zo
 zosol
 (mapv zoout (last zosol)))

(def binary
  [[[0 0 0] :0]
   [[0 0 1] :1]
   [[0 1 0] :2]
   [[0 1 1] :3]
   [[1 0 0] :4]
   [[1 0 1] :5]
   [[1 1 0] :6]
   [[1 1 1] :7]])
(def binarysol  (first (flexible-output-transducer binary 8)))
(def binaryout (output-fn binary))
[binary binarysol binaryout]
(check
 binary
 binarysol
 binaryout)