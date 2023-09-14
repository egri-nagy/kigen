(ns kigen.transducer.flexible
  "FLEXIBLE INPUT OUTPUT TRANSDUCER CONSTRUCTION
 input words can be sequences of any type of distinct entities
 similarly outputs can be of any types
 internal states are nonnegtaive integers"
  (:require
   [clojure.core.logic :as l]
   [clojure.core.logic.fd :as fd] 
   [taoensso.timbre :refer [info debug]]
   [kigen.transducer.common :refer [result-stateo
                                    input-symbols-fn
                                    output-symbols-fn
                                    modded-io-pairs]]))

(defn transducer
  "Given the input-output pairs, and the number of states, this attempts to
  construct a suitable transducer.
   It produces all solutions lazily, so if only a solution is needed, first
   can be used."
  [io-pairs n]
  (let [input-symbols (input-symbols-fn io-pairs)
        num-of-inputs (count input-symbols)
        output-symbols (output-symbols-fn io-pairs)
        ;;to make the io-pairs work for the fixed engine:
        ;;append an extra symbol for readout and replace the output
        ;;and input symbols with their indices
        m-io-pairs (modded-io-pairs io-pairs)
        ;;the finite domains for the search
        outputs (fd/interval 0 (dec (count output-symbols)))
        states (fd/interval 0 (dec n))
        ;;preparing the logic variables
        lvars  (vec (repeatedly (inc num-of-inputs)
                                (fn [] (vec (repeatedly n l/lvar)))))
        state-lvars (apply concat (butlast lvars))
        output-lvars (last lvars)]
    (info ;bit of information about the processed input
     (+ (count state-lvars) (count output-lvars))
     "logic variables for"
     n "states"
     num-of-inputs "input symbols"
     (count output-symbols) "output symbols")
    (debug ;debug information about the modified input
     "modified io pairs" m-io-pairs
     input-symbols)
    (map
     (fn [solution]
       {:delta (into {} ;formatting a solution into a map with keys :delta :omega (see transducer def.)
                     (map
                      (fn [symbol transformation]
                        [symbol transformation])
                      input-symbols
                      (butlast solution)))
        :omega (mapv (fn [i] (when (int? i) (output-symbols i))) ;defending against nil
                     (last solution))})
     (l/run* [q]
             ;this can make the result partial, but makes the search longer
             ;(l/everyg #(l/conde [(l/nilo %)] [(fd/in % states)]) state-lvars)
             ;this fails when creating :omega at the end
             ;(l/everyg #(l/conde [(l/nilo %)] [(fd/in % outputs)]) output-lvars)
             (l/everyg #(fd/in % states) state-lvars)
             (l/everyg #(fd/in % outputs) output-lvars)
             (l/everyg (fn [[input output]]
                         (result-stateo lvars 0 input output))
                       m-io-pairs)
             (l/== q lvars)))))
