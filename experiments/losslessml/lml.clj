;; Lossless machine learning: constructing a single symbol output transducer
;; from input word, output symbol pairs by logic programming.
;; In other words, constructing a Moore-machine. https://en.wikipedia.org/wiki/Moore_machine
;; Internally both the states and the input symbols are represented as nonnegative
;; integers, for the ease of handling by the logic engine through the finite domain
;; package.
(require '[clojure.core.logic :as l])
(require '[clojure.core.logic.fd :as fd])
(require '[kigen.logic :as kl])
(require '[clojure.math.combinatorics :as combo])

;; relational code is after the functional one to see the connection
(defn pos
  "It gives the position of the new state in the state-transition table
   (a flat vector) for the given state and input symbol."
  [num-of-states state input]
  (+ (* num-of-states input) state))

(defn poso
  "Relation version of pos. Succeeds if the computed coordinate
   is the correct one."
  [num-of-states state input result]
  (l/fresh [prod coord]
           (fd/* num-of-states input prod)
           (fd/+ prod state coord)
           (l/== result coord)))

(defn state-transition
  "Given the state transition table as a flat vector, the number of
   states (row length), and a state-input pair it computes the next state
   by a simple lookup."
  [A n state input]
  (nth A (pos n state input)))

(defn state-transitiono
  "The relational version of state-transition."
  [A n state input next-state]
  (l/fresh [coord]
           (poso n state input coord)
           (kl/ntho A coord next-state)))

(defn process-word
  "Processes an input word (sequence of input symbols) by an automaton A starting
   from the given initial state. It returns the resulting state."
  [A n initial-state input-word]
  (reduce
   (partial state-transition A n)
     initial-state
     input-word))

(defn process-wordo
  "The relational version of process-word."
  [A n initial-state input-word output]
  (kl/reduceo (partial state-transitiono A n) initial-state input-word output))

(defn construct-transducer
  "Given the the input-output pairs, and the number of states, this attempts to
  construct a suitable transducer.
   It produces all solutions lazily, so if only a solution is needed, first
   can be used."
  [io-pairs n]
  (let [input-symbols (distinct (mapcat first io-pairs))
        ;output-symbols (distinct (map second io-pairs)) 
        statesfd (fd/interval 0 (dec n))
        state-transitions  (repeatedly (* n (count input-symbols)) l/lvar)]
    (println (count state-transitions) "logic variables for"
             n "states"
             (count input-symbols) "symbols")
    (l/run* [q]
           (l/everyg #(fd/in % statesfd) state-transitions)
           (l/everyg (fn [[input output]]
                       (process-wordo state-transitions n 0 input output))
                     io-pairs)
           (l/== q state-transitions))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;TEST cases
;; states are nonnegative integers, zero is the initial state
;; rows are input symbols, so one row is a transformation induced by the input

;;edge case - 1 state, 1 input symbol
(first (construct-transducer [[[0] 0]] 1))

;;two resets
(first (construct-transducer [[[0] 0]
                              [[1] 1]
                              [[1 1] 1]
                              [[1 0] 0]] 2))

;;flip-flop
(first (construct-transducer [[[0] 0]
                              [[1] 0]
                              [[2] 1]
                              [[2 0] 1]
                              [[2 1] 0]
                              [[2 2] 1]] 2))


;;parity checker - no counting needed
(first (construct-transducer [[[0 0 0] 0]
                              [[0 0 1] 1]
                              [[0 1 0] 1]
                              [[0 1 1] 0]
                              [[1 0 0] 1]
                              [[1 0 1] 0]
                              [[1 1 0] 0]
                              [[1 1 1] 1]] 2))

;;same for 10 bits (and in general)
(first (construct-transducer
        (map (fn [l]
               [l (let [i (count (filter #{1} l))]
                    (if (even? i) 0 1))])
             (combo/selections [0 1] 10)) 2))


;;signal locators
(def signal-locator-io
  [[[1 0 0] 1]
   [[0 1 0] 2]
   [[0 0 1] 3]])
;(count (construct-transducer signal-locator-io 5))

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
(def T [0 4 1 3 2 ;states transformed by input symbol 0
        1 2 0 3 3
        1 2 3 4 0
        1 0 2 3 4
        0 1 0 1 0
        2 3 3 2 1])

(def i-o-pairs
  (for [w (repeatedly 5 (fn [] (vec (repeatedly 3 (partial rand-int 6)))))]
    [w (process-word T 5 0 w)]))

;(construct-transducer i-o-pairs 5)
