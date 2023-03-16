;; Lossless machine learning: constructing single symbol output transducers
;; from input word, output symbol pairs by logic programming.
(require '[clojure.core.logic :as l])
(def T {:p {:a :p, :b :q}
        :q {:a :q, :b :p}})

;; here is an example of an automaton state transition function
(require '[clojure.core.logic.fd :as fd])

(l/defne reduceo
  "succeds if the reduction produces the result"
  [relo initial coll result]
  ([_ _ () _] (l/== initial result))
  ([_ _ [next-item . remaining] _]
   (l/fresh [result-so-far]
            (relo initial next-item result-so-far)
            (reduceo relo result-so-far remaining result))))

(defn state-transition
  [A state input]
  (get (get A state) input)) ; get the maps for the state, then map the input

(defn state-transitiono
  [A state input next-state]
  (l/fresh [table]
           (l/featurec A {state table})
           (l/featurec table {input next-state})))

(defn process-word
  "Processes an input word (sequence of input symbols) by an automaton A starting
   from the given initial state. It returns the resulting state."
  [A initial-state input-word]
  (reduce
   (partial state-transition A)
     initial-state
     input-word))

(defn process-wordo
  [A initial-state input output]
  (reduceo (partial state-transitiono A) initial-state input output))

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
           (l/everyg #(fd/in % statesfd) lvars)
           (l/everyg
            (fn [[input output]] (process-wordo q 0 input output))
            io-pairs))))

(construct-transducer [ [[:a :a ] :q] [ [:b :b] :p] [[:a :b] :p]] 1)