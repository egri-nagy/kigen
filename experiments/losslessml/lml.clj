;; Lossless machine learning: constructing single symbol output transducers
;; from input word, output symbol pairs by logic programming.
(require '[clojure.core.logic :as l])
(require '[clojure.core.logic.fd :as fd])

;; here is an example of an automaton state transition function
(def T {:p {:a :p, :b :q}
        :q {:a :q, :b :p}})

(defn process-word
  "Processes an input word (sequence of input symbols) by an automaton A starting
   from the given initial state. It returns the resulting state."
  [A initial-state input-word]
  (reduce
   (fn [state input]
     ((A state) input)) ; get the maps for the state, then map the input
   initial-state
   input-word))

(defn construct-transducer
  "Given the the input-output pairs, and the number of states, this attempts to
  construct a suitable transducer."
  [io-pairs n]
  (let [num-of-output-symbols (count (distinct (map second io-pairs)))]
    (print n " " num-of-output-symbols)))

(construct-transducer [ [[:a :a ] :q] [ [:b :b] :p]] 4)