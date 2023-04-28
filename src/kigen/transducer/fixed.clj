(ns kigen.transducer.fixed
  " FIXED OUTPUT TRANSDUCER CONSTRUCTION ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
this is the first working version of transducer synthesis so it is somewhat of
a legacy code. The input-output pairs need to be given in the internal representation
(nonnegative integers) thus the output of the Moore automaton is hardcoded.
There is also the issue of using the initial state 0 as an output which may be a too
rigid constraint."
  (:require
   [clojure.core.logic :as l]
   [clojure.core.logic.fd :as fd]
   [taoensso.timbre :refer [info debug]] 
   [kigen.transducer.common :refer [process-wordo process-word input-symbols-fn trajectory]]))

(defn fixed-output-transducer
  "Given the the input-output pairs, and the number of states, this attempts to
  construct a suitable transducer. Both the inputs and the outputs are represented as
  nonnegative integers. The output is delta, the first is the transformation realized by
   input symbol 0, second for input symbol 1, and so on.
  It produces all solutions lazily, so if only a solution is needed, first
  can be used."
  [io-pairs n]
  (let [input-symbols (input-symbols-fn io-pairs)
        statesfd (fd/interval 0 (dec n))
        delta (vec
               (repeatedly (count input-symbols)
                           (fn [] (vec (repeatedly n l/lvar)))))
        lvars (apply concat delta)]
    (info (count lvars) "logic variables for"
          n "states"
          (count input-symbols) "symbols")
    (l/run* [q]
            (l/everyg #(fd/in % statesfd) lvars)
            (l/everyg (fn [[input output]]
                        (process-wordo delta 0 input output))
                      io-pairs)
            (l/== q delta))))

(defn check-fixed
  "Returns true if the given automaton (defined by delta, state transition function)
   will indeed produce the output values given in the io-pairs."
  [io-pairs delta]
  (every? (fn [[input output]]
            (= output (process-word delta 0 input)))
          io-pairs))

(defn trajectories-fixed
  "Creates string representations of all trajectories by the io-pairs."
  [io-pairs delta]
  (map ;we are going through all input-out pairs
   (fn [[input output]] ;representing one trajectory in a string
     (let [trj (trajectory delta 0 input)
           final (last trj)]
       (apply str (concat (map (fn [q i]
                                 (str q " "
                                      "·" i " "))
                               trj
                               input)
                          [(last trj) " = " final
                           (if (= output final)
                             " ✔"
                             " ✘")]))))
   io-pairs))