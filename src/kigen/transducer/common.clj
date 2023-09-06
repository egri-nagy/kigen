(ns kigen.transducer.common
  "Lossless machine learning: constructing a single symbol output transducer  from input word,
   output symbol pairs by logic programming.
   In other words, constructing a Moore-machine. https://en.wikipedia.org/wiki/Moore_machine
   There are several implementations for representing the transducer. These are the common functions.
   delta - the state transition table is a nested associative data structure (map or vector)
   (delta input) gives a transformation of the state set"
  (:require
   [clojure.core.logic :as l]
   [kigen.logic :refer [reduceo ntho]]
   [kigen.position :refer [index]]))

(defn trajectory
  "Processes an input word (sequence of input symbols) by an automaton described by the delta state transition table
   (nested associative data structure, mapping inputs to transformations of the state set).
  It returns the sequence of states visited starting from the given initial state.
  Same as result-state, but the whole trajectory (initital, all intermittent states and final state) is returned."
  [delta initial-state input-word]
  (reductions
   (fn [state input]
     ((delta input) state)) ;state transition - more flexible without nth, works for maps too
   initial-state
   input-word))

(defn result-state
  "Processes an input word (sequence of input symbols) by an automaton described by the delta state transition table
   (nested associative data structure, mapping inputs to transformations of the state set) starting from the given initial state.
   It returns the resulting state."
  [delta initial-state input-word]
  (reduce
   (fn [state input]
     ((delta input) state)) ;not using nth to be more flexible i.e. maps
   initial-state
   input-word))

;; relational code is after the functional one to see the connection
;; we have to use ntho explicitly (only works vectors internally) ;TODO why? used by fixed, flexible
(defn result-stateo
  "The relational version of result-state."
  [delta initial-state input-word output]
  (reduceo (fn [state input next-state]
                (l/fresh [v]
                         (ntho delta input v)
                         (ntho v state next-state)))
              initial-state
              input-word
              output))

(defn output-symbols-fn
  "Returns all collected output symbols appearing in the input-output
   pairs without repetition. Returned as a vector, the indices can be used
   to refer to the symbols. The order of the symbols defined by the order
   of their first appeareances in the io-pairs."
  [io-pairs]
  (vec (distinct (map second io-pairs))))

(defn input-symbols-fn
  "Returns all collected input symbols appearing in the input-output
   pairs without repetition. Returned as a vector, the indices can be used
   to refer to the symbols. The order of the symbols defined by the order
   of their first appeareances in the io-pairs."
  [io-pairs]
  (vec (distinct (mapcat first io-pairs))))

(defn check
  "Returns true if the given automaton (defined by solution, state transition function
   and output function) will indeed produce the output values given in the io-pairs.
   It uses format-flexible for processing the raw solution."
  [io-pairs {delta :delta omega :omega}]
  (every? (fn [[input output]]
            (= output (omega (result-state delta 0 input))))
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

(defn modded-io-pairs
  "Recodes the input symbols to natural numbers and adds an extra input at the end serving for the state readout."
  [io-pairs]
  (let [input-symbols (input-symbols-fn io-pairs)
        output-symbols (output-symbols-fn io-pairs)
        readout-symbol (count input-symbols)] ;the extra input symbol to trigger state readout
    (for [[input output] io-pairs]
      [;;the new encoded input word
       (vec
        (concat
         (map (partial index input-symbols) input)
         [readout-symbol]))
       ;;the new encoded output
       (index output-symbols output)])))

