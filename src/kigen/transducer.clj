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
   [taoensso.timbre :refer [trace]]))

;; relational code is after the functional one to see the connection

(defn process-word
  "Processes an input word (sequence of input symbols) by an automaton A starting
   from the given initial state. It returns the resulting state."
  [A n initial-state input-word]
  (reduce
   (fn [state input]
     (nth (nth A input) state))
   initial-state
   input-word))

(defn process-wordo
  "The relational version of process-word."
  [A n initial-state input-word output]
  (kl/reduceo (fn [state input next-state]
                (l/fresh [v]
                         (kl/ntho A input v)
                         (kl/ntho v state next-state)))
              initial-state
              input-word
              output))

(defn construct-transducer
  "Given the the input-output pairs, and the number of states, this attempts to
  construct a suitable transducer.
   It produces all solutions lazily, so if only a solution is needed, first
   can be used."
  [io-pairs n]
  (let [input-symbols (distinct (mapcat first io-pairs))
        statesfd (fd/interval 0 (dec n))
        A  (vec (repeatedly (count input-symbols)
                            (fn [] (vec (repeatedly n l/lvar)))))
        lvars (apply concat A)]
    (trace (count lvars) "logic variables for"
             n "states"
             (count input-symbols) "symbols")
    (l/run* [q]
            (l/everyg #(fd/in % statesfd) lvars)
            (l/everyg (fn [[input output]]
                        (process-wordo A n 0 input output))
                      io-pairs)
            (l/== q A))))