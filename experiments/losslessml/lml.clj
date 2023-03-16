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
  (let [input-symbols (distinct (mapcat first io-pairs))
        output-symbols (distinct (map second io-pairs))
        states (range 0 n)
        statesfd (fd/interval 0 (dec n))
        state-transitions (reduce
                           (fn [m i]
                             (conj m [i (zipmap input-symbols
                                                (repeatedly n l/lvar))]))
                           {}
                           states)
        lvars (mapcat vals (vals state-transitions))]
    (l/run 1 [q]
           (l/== q state-transitions)
           (l/everyg #(fd/in % statesfd) lvars))))

(construct-transducer [ [[:a :a ] :q] [ [:b :b] :p] [[:a :b] :p]] 2)