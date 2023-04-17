(require '[clojure.math.combinatorics :as combo])
;(require '[kigen.transducer :refer :all])
(require '[taoensso.timbre :as timbre])
(require '[tangle.core :as tangle])
(require '[clojure.java.io :refer [copy file]])

(require '[kigen.logic :as kl])

(require '[clojure.core.logic :as l])
(require '[clojure.core.logic.fd :as fd])
(require '[taoensso.timbre :refer [info debug]])
(require '[kigen.position :refer [index]])

; levels: :warn, :info, :debug
(timbre/set-min-level! :debug)

(def OGS :_OGS) ;;DO-NOT-USE-THIS-AS INPUT!

(defn prepare-logic-variables
  "This gives the shape of the solution."
  [input-symbols num-of-outputs num-of-states]
  (let [states (range num-of-states)
        state-input-pairs (for [q states
                                i input-symbols]
                            [q i])
        delta (zipmap state-input-pairs (repeatedly l/lvar))]
    {:delta delta
     :omega (vec (map delta (for [q states] [q OGS])))}))

(prepare-logic-variables [:a :b OGS] 2 3)

(defn trajectory
  "Processes an input word (sequence of input symbols) by an automaton described by the delta state transition function (as vector of vectors) starting from the given initial state.
   The whole trajectory (initital, all intermittent states and final state) is returned."
  [delta initial-state input-word]
  (reductions
   (fn [state input]
     ((delta [state input]))) ;state transition - more flexible without nth, works for maps too
   initial-state
   input-word))

(defn process-word
  "Processes an input word (sequence of input symbols) by an automaton described by the delta state transition function (as vector of vectors) starting from the given initial state. It returns the resulting state."
  [delta initial-state input-word]
  (reduce
   (fn [state input]
     (delta [state input])) ;not using nth to be more flexible i.e. maps
   initial-state
   input-word))

(defn process-word2
  [delta state input-word]
  (if (empty? input-word)
    state
    (process-word2 delta (delta [state (first input-word)]) (rest input-word))))

(process-word {[0 :a] 1, [1 :a] 1} 0 [:a :a])
(process-word2 {[0 :a] 1, [1 :a] 1} 0 [:a :a])

;; relational code is after the functional one to see the connection
;; we have to use ntho explicitly (only works vectors internally)
(defn process-wordo
  "The relational version of process-word."
  [delta initial-state input-word output] 
  (kl/reduceo (fn [state input next-state]
                (l/fresh [x]
                         (l/project [delta state input]
                                    
                                    (l/== x (delta [state input]))
                                    (l/== x next-state))))
              initial-state
              input-word
              output))

(defn process-wordo2
  [delta state input-word output]
  (l/conde
   [(l/emptyo input-word) (l/== state output)]
   [(l/fresh [input r nst]
             (l/resto input-word r)
             (l/firsto input-word input)
             ;(l/project [delta state input] (l/== nst (delta [state input])))
             (l/featurec delta {[state input] nst})
             (process-wordo2 delta nst r output))]))

;; this is fine, we leave the values unspecified, but give constraints so the values found 
;; without projecting it does not work
(l/run 1 [q]
       (l/== q {:a (l/lvar) :b (l/lvar) :c (l/lvar)})
       (l/project [q] (l/== 10 (q :a)))
       (l/project [q] (l/== 8 (q :c)))
       (l/project [q] (l/== (q :c) (q :b))))

(l/run 1 [q]
       (l/fresh [a b c d]
                (l/== q {[0 :a] a [0 :b] b [1 :a] c [1 :b] d})
                (l/everyg #(fd/in % (fd/domain 0 1 2)) [a b c d])
                (process-wordo q 0 [:a] 2) ;; working
                (process-wordo2 q 0 [:b :a] 1) ;; not working
                ))


(l/run 1 [q]
       (l/fresh [m]
                (l/== q {[0 :a] 1, [1 :a] m, [0 :b] 1, [1 :b] 2})
                (l/project [m] (process-wordo q 0 [:a :a :b :b] 2))
                (fd/in m (fd/domain 0 1 2))))

(l/run 1 [q r]
       (fd/in q (fd/domain 0 1 2))
       (fd/in r (fd/domain 0 1 2))
       (process-wordo {[0 :a] q, [1 :a] 0, [0 :b] q, [1 :b] 2} 0 [:a :a :b :b] 2)
       ;(process-wordo {[0 :a] 1, [1 :a] r, [0 :b] 1, [1 :b] 2} 0 [:a :a :b :b] 2)
       )


;; FLEXIBLE INPUT OUTPUT TRANSDUCER CONSTRUCTION ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; input words can be sequences of any type of distinct entities
;; similarly, outputs can be of any types
;; internal states are nonnegtaive integers

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
        output-generator OGS; the extra input symbol to trigger state readout
        ;;to make the io-pairs work for the fixed engine:
        ;;append an extra symbol for readout and replace the output
        ;;and input symbols with their indices
        modded-io-pairs (for [[input output] io-pairs]
                          [(vec (concat  input
                                         [output-generator]))
                           (index output-symbols output)])
        ;;the finite domains for the search
        outputs (fd/interval 0 (dec (count output-symbols)))
        states (fd/interval 0 (dec n))
        ;;preparing the logic variables: we build a map [state input] -> state
        ;;and a vector for the output mapping
        result  (prepare-logic-variables (conj input-symbols OGS) (count output-symbols) n) 
        output-lvars (:omega result)
        state-lvars (remove (set output-lvars) (vals (:delta result)))]
    (info ;bit of information about the processed input
     (+ (count state-lvars) (count output-lvars))
     "logic variables for"
     n "states"
     num-of-inputs "input symbols"
     (count output-symbols) "output symbols")
    (debug ;debug information about the modified input
     "modified io pairs" modded-io-pairs)
    (debug "shape of result" result)
    (debug "state lvars" state-lvars "output lvars" output-lvars)
    (debug output-symbols)
    (map
     (fn [solution]
       ;;just recoding the output
       (update solution :omega (partial map output-symbols)))
     (l/run* [q]
             (l/everyg #(fd/in % states) state-lvars)
             (l/everyg #(fd/in % outputs) output-lvars)
             (l/everyg (fn [[input output]]
                         (process-wordo (:delta result) 0 input output))
                       modded-io-pairs)
             (l/== q result)))))

(def sl-3-3
  [["|__" :first]
   ["_|_" :second]
   ["__|" :third]])
(def sl-3-3sol (first (transducer sl-3-3 3)))
(println (trajectories sl-3-3 sl-3-3sol))

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