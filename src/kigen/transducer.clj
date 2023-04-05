(ns kigen.transducer
  "Lossless machine learning: constructing a single symbol output transducer  from input word, output symbol pairs by logic programming.
  In other words, constructing a Moore-machine. https://en.wikipedia.org/wiki/Moore_machine
 Internally both the states and the input symbols are represented as nonnegative
 integers, for the ease of handling by the logic engine through the finite domain
 package."
  (:require
   [clojure.core.logic :as l]
   [clojure.core.logic.fd :as fd]
   [kigen.logic :as kl]
   [taoensso.timbre :refer [info debug]]
   [kigen.position :refer [index]]))

(defn trajectory
  "Processes an input word (sequence of input symbols) by an automaton described by the delta state transition function (as vector of vectors) starting from the given initial state.
   The whole trajectory (initital, all intermittent states and final state) is returned."
  [delta initial-state input-word]
  (reductions
   (fn [state input]
     (nth (nth delta input) state)) ;state transition
   0 ;default start state TODO: this may need to change later
   input-word))

(defn process-word
  "Processes an input word (sequence of input symbols) by an automaton described by the delta state transition function (as vector of vectors) starting from the given initial state. It returns the resulting state."
  [delta initial-state input-word]
  (reduce
   (fn [state input]
     (nth (nth delta input) state))
   initial-state
   input-word))

;; relational code is after the functional one to see the connection

(defn process-wordo
  "The relational version of process-word."
  [delta initial-state input-word output]
  (kl/reduceo (fn [state input next-state]
                (l/fresh [v]
                         (kl/ntho delta input v)
                         (kl/ntho v state next-state)))
              initial-state
              input-word
              output))

(defn fixed-output-transducer
  "Given the the input-output pairs, and the number of states, this attempts to
  construct a suitable transducer.
   It produces all solutions lazily, so if only a solution is needed, first
   can be used."
  [io-pairs n]
  (let [input-symbols (distinct (mapcat first io-pairs))
        statesfd (fd/interval 0 (dec n))
        delta  (vec (repeatedly (count input-symbols)
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
  [io-pairs delta]
  (every? (fn [[input output]]
            (= output (process-word delta 0 input)))
          io-pairs))

(defn output-symbols-fn
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
        output-symbols (output-symbols-fn io-pairs)
        output-generator   num-of-inputs ; the extra input symbol
        ;;to make the io-pairs work for the fixed engine:
        ;;append an extra symbol for readout and replace the output
        ;;symbols with their indices
        modded-io-pairs (for [[input output] io-pairs]
                          [(vec (concat input [output-generator]))
                           (index output-symbols output)])
        ;;the finite domains for the search
        outputs (fd/interval 0 (dec (count output-symbols)))
        states (fd/interval 0 (dec n))
        ;;preparing the logic variables, we return the augmented matrix
        ;;as the solution
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
     "modified io pairs" modded-io-pairs)
    (l/run* [q]
            (l/everyg #(fd/in % states) state-lvars)
            (l/everyg #(fd/in % outputs) output-lvars)
            (l/everyg (fn [[input output]]
                        (process-wordo lvars 0 input output))
                      modded-io-pairs)
            (l/== q lvars))))

(defn check-flexible
  [io-pairs solution]
  (let [delta (butlast solution)
        out-f (output-symbols-fn io-pairs)
        omega (mapv out-f (last solution))]
    (every? (fn [[input output]]
              (= output (omega (process-word delta 0 input))))
            io-pairs)))