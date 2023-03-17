;; Lossless machine learning: constructing single symbol output transducers
;; from input word, output symbol pairs by logic programming.
(require '[clojure.core.logic :as l])
(require '[clojure.core.logic.fd :as fd])

;; here is an example of an automaton state transition function
;; states are nonnegative integers, zero is the initial state
(def T {0 {:a 0, :b 0}
        1 {:a 1, :b 0}})

(def T2 {0 {0 0, 1 0}
        1 {0 1, 1 0}})

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

(defn geto
  [m k v]
  (println m)
  (l/membero  [k v] (seq m)))

(defn state-transitiono
  [A state input next-state]
  (println A)
  (l/fresh [table x]
           (geto A state table)
           (l/== x table)
           (geto x input next-state)))

(defn process-word
  "Processes an input word (sequence of input symbols) by an automaton A starting
   from the given initial state. It returns the resulting state."
  [A initial-state input-word]
  (reduce
   (partial state-transition A)
     initial-state
     input-word))

(defn process-wordo
  [A initial-state input-word output]
  (reduceo (partial state-transitiono A) initial-state input-word output))

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
    ;(println state-transitions "----" lvars)
    (l/run 1 [q]
           (l/== q state-transitions)
           (l/everyg #(fd/in % statesfd) lvars)
           (l/everyg
            (fn [[input output]] (process-wordo q 0 input output))
            io-pairs))))

(construct-transducer [ [[:a :a ] 0] [ [:b :b] 1] [[:a :b] 2]] 3)