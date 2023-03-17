;; Lossless machine learning: constructing a single symbol output transducer
;; from input word, output symbol pairs by logic programming.
;; In other words, constructing a Moore-machine. https://en.wikipedia.org/wiki/Moore_machine
(require '[clojure.core.logic :as l])
(require '[clojure.core.logic.fd :as fd])

;; general relational code
(l/defne reduceo
  "Relational reduce, succeds if the reduction produces the result."
  [relo initial coll result]
  ([_ _ () _] (l/== initial result))
  ([_ _ [next-item . remaining] _]
   (l/fresh [result-so-far]
            (relo initial next-item result-so-far)
            (reduceo relo result-so-far remaining result))))

(defn ntho
  ([coll ?n ?x] (ntho coll ?n ?x 0))
  ([coll ?n ?x current]
   (l/fresh [head tail]
            (l/firsto coll head)
            (l/resto coll tail)
            (l/conde [(l/== head ?x) (l/== ?n current)]
                     [(ntho tail ?n ?x (inc current))]))))

;; Internally both the states and the input symbols are represented as nonnegative
;; integers, for the ease of handling by the logic engine through the finite domain
;; package.
;; here is an example of an automaton state transition function
;; states are nonnegative integers, zero is the initial state
;; rows are input symbols, so one row is a transformation induced by the input
(def T [0 0 1 ;states transformed by input symbol 0
        1 2 0])

(defn pos
  "It gives the position of the new state in the state-transition table
   for the current state and input symbol."
  [num-of-states row column]
  (+ (* num-of-states row) column))

(defn poso
  "Relation version of pos. Succeeds if the computed coordinate
   is the correct one."
  [num-of-states row column result]
  (l/fresh [prod coord]
           (fd/* num-of-states row prod)
           (fd/+ prod column coord)
           (l/== result coord)))

(defn state-transition
  [A n state input]
  (nth A (pos n state input)))

(defn state-transitiono
  [A n state input next-state]
  (l/fresh [coord]
           (poso n state input coord)
           (ntho A coord next-state)))

(defn process-word
  "Processes an input word (sequence of input symbols) by an automaton A starting
   from the given initial state. It returns the resulting state."
  [A n initial-state input-word]
  (reduce
   (partial state-transition A n)
     initial-state
     input-word))

(defn process-wordo
  [A n initial-state input-word output]
  (l/log "hey")
  (reduceo (partial state-transitiono A n) initial-state input-word output))

(defn construct-transducer
  "Given the the input-output pairs, and the number of states, this attempts to
  construct a suitable transducer."
  [io-pairs n]
  (let [input-symbols (distinct (mapcat first io-pairs))
        ;output-symbols (distinct (map second io-pairs)) 
        statesfd (fd/interval 0 (dec n))
        state-transitions  (repeatedly (* n (count input-symbols)) l/lvar)]
    (println state-transitions "----")
    (l/run* [q]
           (l/== q state-transitions)
           (l/everyg #(fd/in % statesfd) q)
           (l/everyg (fn [[input output]] (process-wordo q n 0 input output)) io-pairs))))

(construct-transducer [  [[1] 1] [[0 0] 0] [[0 1] 2] [[1 0 1] 1]]  4)

(construct-transducer [[[1 0 1 2 1 2] 1]
                       [[0 0 1 1 2 2] 0]
                       [[0 1 2 0 1 2] 2]
                       [[1 0 1 0 1 1] 0]
                       [[2 1 2 1 2] 1]
                       [[2 1 2 1 1] 0]] 3)