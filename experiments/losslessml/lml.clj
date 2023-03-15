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
     ((A state) input))
   initial-state
   input-word))

