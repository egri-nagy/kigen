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
     ((delta input) state)) ;state transition - more flexible without nth
   0 ;default start state TODO: this may need to change later
   input-word))

(defn process-word
  "Processes an input word (sequence of input symbols) by an automaton described by the delta state transition function (as vector of vectors) starting from the given initial state. It returns the resulting state."
  [delta initial-state input-word]
  (reduce
   (fn [state input]
     ((delta input) state)) ;not using nth to be more flexible i.e. maps
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



;; FLEXIBLE OUTPUT TRANSDUCER CONSTRUCTION
(defn output-symbols-fn
  "Returns all collected output symbols appearing in the input-output
   pairs without repetition. Returned as a vector, the indices can be used
   to refer to the symbols. The order of the symbols defined by the order
   of their appeareance in the io-pairs (through distinct)."
  [io-pairs]
  (vec (distinct (map second io-pairs))))

(defn input-symbols-fn
  "Returns all collected input symbols appearing in the input-output
   pairs without repetition. Returned as a vector, the indices can be used
   to refer to the symbols. The order of the symbols defined by the order
   of their appeareance in the io-pairs (through distinct)."
  [io-pairs]
  (vec (distinct (mapcat first io-pairs))))

(defn transducer
  "Given the input-output pairs, and the number of states, this attempts to
  construct a suitable transducer.
   It produces all solutions lazily, so if only a solution is needed, first
   can be used."
  [io-pairs n]
  (let [input-symbols (input-symbols-fn io-pairs)
        num-of-inputs (count input-symbols)
        output-symbols (output-symbols-fn io-pairs)
        output-generator   num-of-inputs ; the extra input symbol
        ;;to make the io-pairs work for the fixed engine:
        ;;append an extra symbol for readout and replace the output
        ;;symbols with their indices
        modded-io-pairs (for [[input output] io-pairs]
                          [(vec (concat (map (partial index input-symbols) input)
                                        [output-generator]))
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
     "modified io pairs" modded-io-pairs
     input-symbols)
    (map
     (fn [solution]
       {:delta (into {} ;redoing delta into a map from input symbol to transformation
                     (map
                      (fn [symbol transformation]
                        [symbol transformation])
                      input-symbols
                      (butlast solution)))
        :omega (mapv (output-symbols-fn io-pairs) (last solution))})
     (l/run* [q]
             (l/everyg #(fd/in % states) state-lvars)
             (l/everyg #(fd/in % outputs) output-lvars)
             (l/everyg (fn [[input output]]
                         (process-wordo lvars 0 input output))
                       modded-io-pairs)
             (l/== q lvars)))))


(defn check
  "Returns true if the given automaton (defined by solution, state transition function
   and output function) will indeed produce the output values given in the io-pairs.
   It uses format-flexible for processing the raw solution."
  [io-pairs {delta :delta omega :omega}]
  (every? (fn [[input output]]
            (= output (omega (process-word delta 0 input))))
          io-pairs))

(defn trajectories
  "Creates string representations of all trajectories by the io-pairs."
  [io-pairs {delta :delta omega :omega}]
  (map ;we are going through all input-out pairs
   (fn [[input output]] ;representing one trajectory in a string 
     (let [trj (trajectory delta 0 input)
           final (omega (last trj))]
       (apply str (concat (map (fn [q i] (str q " "
                                              ;"(" (omega q) ") "
                                              "·"  i " "))
                               trj
                               input)
                          [(last trj) " = " final
                           (if (= output final)
                             " ✔"
                             " ✘")]))))
   io-pairs))

;; FIXED OUTPUT TRANSDUCER CONSTRUCTION
;; this is the first working version of transducer synthesis, so it is somewhat of
;; a legacy code. The input-output pairs need to be given in the internal representation
;; (nonnegative integers), thus the output of the Moore automaton is hardcoded.
;; There is also the issue of using the initial state 0 as an output, which may be a too
;; rigid constraint.
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