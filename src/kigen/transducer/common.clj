(ns kigen.transducer.common
  "Lossless machine learning: constructing a single symbol output transducer  from input word,
   output symbol pairs by logic programming.
   In other words, constructing a Moore-machine. https://en.wikipedia.org/wiki/Moore_machine
   Internally both the states and the input symbols are represented as nonnegative
   integers, for the ease of handling by the logic engine through the finite domain
   package. State 0 is the initial state."
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
     ((delta input) state)) ;state transition - more flexible without nth, works for maps too
   initial-state
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
;; we have to use ntho explicitly (only works vectors internally)
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

