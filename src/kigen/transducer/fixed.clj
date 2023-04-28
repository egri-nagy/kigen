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
    (map (fn [solution]
           {:delta solution
            :omega identity})
         (l/run* [q]
                 (l/everyg #(fd/in % statesfd) lvars)
                 (l/everyg (fn [[input output]]
                             (process-wordo delta 0 input output))
                           io-pairs)
                 (l/== q delta)))))