(ns kigen.transducer.fixed
  "FIXED OUTPUT TRANSDUCER CONSTRUCTION
This is the first working version of transducer synthesis so it is somewhat of
a legacy code. The input-output pairs need to be given in the internal representation
(nonnegative integers) thus the output of the Moore automaton is hardcoded.
Consequently, there is the issue of using the initial state 0 as an output which may be a too
rigid constraint."
  (:require
   [clojure.core.logic :as l]
   [clojure.core.logic.fd :as fd]
   [taoensso.timbre :refer [info debug]]
   [kigen.transducer.common :refer [result-stateo input-symbols-fn]]
   [kigen.logic :refer [lvar-table]]))

(defn fixed-output-transducer
  "Given the the input-output pairs, and the number of states, this attempts to
  construct a suitable transducer. Both the inputs and the outputs are represented as
  nonnegative integers. The output delta is a vector of transformations.
  The first vector is the transformation realized by
  input symbol 0, second for input symbol 1, and so on.
  It produces all solutions lazily, so if only a solution is needed, the first
  one can be used. The output function is the identity."
  [io-pairs n]
  (let [i (count (input-symbols-fn io-pairs))
        statesfd (fd/interval 0 (dec n))
        [delta lvars] (lvar-table i n)]
    (info (count lvars) "logic variables for"
          n "states"
          i "symbols")
    (debug lvars)
    (map (fn [solution]
           {:delta solution
            :omega identity
            :n n})
         (l/run* [q]
                 (l/everyg #(fd/in % statesfd) lvars)
                 (l/everyg (fn [[input output]]
                             (result-stateo delta 0 input output))
                           io-pairs)
                 (l/== q delta)))))