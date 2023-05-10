; trajectory based transducer construction
; this way we can naturally do partial automata, but it does not
; scale well due to the large number of lvars (total sum of length of
; all trajectories)

(require '[kigen.logic :as kl])
(require '[clojure.core.logic :as l])
(require '[clojure.core.logic.fd :as fd])
(require '[taoensso.timbre :refer [info debug set-min-level! *config* merge-config!]])
(require '[kigen.position :refer [index]])
(require '[clojure.pprint :refer [pprint]])
(require '[clojure.math.combinatorics :as combo])
(require '[kigen.transducer.common :refer :all])
(require '[kigen.transducer.flexible :refer [modded-io-pairs]])

; levels: :warn, :info, :debug
(set-min-level! :info)
(defn simple-logger
  [m]
  (str (:min-level (:config m)) " "
       (:vargs m)))
(merge-config! {:output-fn simple-logger})

(defn trajectory-logic-variables
  "It prepares the trajectory logic variables based on the modified
   input-output pairs. The first state is 0 by default, the last one is the output, everything in between new logic variables."
  [modded-io-pairs]
  (for [[inputs output] modded-io-pairs]
    (let [n (count inputs)] 
      (concat [0] (repeatedly (dec n) l/lvar) [output]))))

(defn mappings-from-trajectories
  "It extracts the individual mappings from the trajectories and
   the corresponding input symbol. Returns a map with input symbols
   as keys and list of pairs (from,to) defining individual maps."
  [inputs trajectories]
  (letfn [(builder [input trajectory]
            (partition 2 (interleave input
                                     (partition 2 1 trajectory))))]
    (update-vals
     (group-by first
               (distinct
                (mapcat builder
                        inputs
                        trajectories)))
     (partial map second))))

(l/defne compatiblo
  "This goal succeeds if the two mappings are compatible, i.e.
   they can be in the same transformation, they define a function."
  [m1 m2]
  ([[x1  y1] [x2  y2]]
   (l/conde
    [(l/== x1 x2)(l/== y1 y2)] ;the images are the same TODO: why do we need == x1 x2? investigate!
    [(l/!= x1 x2)]))) ;the preimages are different

(l/run*
 [q]
 (compatiblo [0 0] [1 0])
 (compatiblo [0 0] [1 1]))

(l/run* [q] (compatiblo [0 0] [0 1]))
(l/run* [q] (compatiblo [2 0] [2 1]))

(l/run* [q]
        (l/membero q [0 1 2])
        (compatiblo [1 2] [q 2]))

(l/run* [q]
        (l/membero q [0 1 2])
        (compatiblo [1 2] [1 q]))

(let [X (range 3)]
  (l/run* [q] (l/fresh [a b]
                       (l/== q [a b])
                       (l/membero a X)
                       (l/membero b X)
                       (compatiblo [:x 0] q))))

(let [X (range 3)]
  (l/run* [q] (l/fresh [a b]
                       (l/== q [a b])
                       (l/membero a X)
                       (l/membero b X)
                       (compatiblo [1 0] q))))


(l/defne compatible-with-collo
  "Succeeds if the given mapping is compatible with all the mappings
   in the collection."
  [m coll]
  ([_ ()] l/succeed) ;nothing to contradict
  ([m [f . r]]
   (compatiblo m f)
   (compatible-with-collo m r)))

(l/run* [q] (compatible-with-collo [0 0] [[0 0] [1 0] [1 1]]))
; collection itself can be not compatible
(l/run* [q] (compatible-with-collo [0 0] [[1 0] [1 1]]))

(l/run* [q] (compatible-with-collo [0 0] [[2 1] [1 0] [0 1]]))

(l/defne compatible-collo
  "Succeeds if all the mappings in the collection are compatible
   with each other. O(n^2)"
  [coll] 
  ([()] l/succeed) ;nothing to contradict
  ([[f . r]]
   (compatible-with-collo f r) ;first is compatible with rest
   (compatible-collo r))) ;rest is compatible in itself

(l/run* [q]
       (l/== q [[(l/lvar) (l/lvar)] [(l/lvar) (l/lvar)]])
       (compatible-collo q))
;todo, interpret this
(l/run* [q]
        (l/fresh [a b c d e f]
                 (l/== q [[a b] [c d] [e f]])
                 (compatible-collo q)))
(l/run* [q]
        (l/fresh [a b c d e f]
                 (l/everyg (fn [x] (fd/in x (fd/domain 1 2 3)))
                           [a b c d e f])
                 (l/== q [[a b] [c d] [e f]])
                 (l/distincto q)
                 (compatible-collo q)))

(defn transducer3 
  [io-pairs n]
  (let [output-symbols (output-symbols-fn io-pairs)
        input-symbols (input-symbols-fn io-pairs)
        readout-symbol (count input-symbols)
        m-io-pairs (modded-io-pairs io-pairs)
        m-inputs (map first m-io-pairs)
        trajectories (trajectory-logic-variables m-io-pairs)
        mappings (mappings-from-trajectories m-inputs trajectories)
        output-lvars (map first (mappings readout-symbol))
        state-lvars (remove (set output-lvars)
                            (distinct
                             (filter l/lvar? (apply concat trajectories))))]
    (info "#lvars:" (+ (count output-lvars) (count state-lvars)))
    (info "search space size:"
          (str n "^" (count state-lvars)
               "*" (count output-symbols) "^" (count output-lvars)))
    (debug "mappings:" mappings)
    (debug "trajectories:" trajectories)
    (debug "outputlvars:" output-lvars)
    (debug "statelvars:" state-lvars)
    (map
     (fn [solution]
       (let [ts ; input symbol (internal) -> transformation
             (update-vals (mappings-from-trajectories m-inputs solution)
                          (fn [mappings] ;building transformation t  
                            (reduce
                             (fn [t [from to]]
                               (assoc t from to))
                             (vec (repeatedly n (constantly nil)))
                             mappings)))]
         {:delta (update-keys (dissoc ts readout-symbol)
                              input-symbols)
          :omega (mapv output-symbols  (ts readout-symbol))}))
     (l/run* [q]
             (l/== q trajectories)
             (l/everyg (fn [x] (fd/in x (apply fd/domain (range n))))
                       state-lvars)
             (l/everyg (fn [x] (fd/in x (apply fd/domain (range (count output-symbols)))))
                       output-lvars)
             (l/everyg compatible-collo
                       (vals mappings))))))

(def sl-3-3
  [["|__" :first]
   ["_|_" :second]
   ["__|" :third]])

(trajectories sl-3-3 (first (transducer3 sl-3-3 3)))

(def sl-3-3b
  [["|__" :first]
   ["_|_" :second]
   ["__|" :third]
   ["___" :none]])

(check sl-3-3b (first (transducer3 sl-3-3b 4)))

;; non-partial output
(def ex1
  [["aaba" :foo]
   ["bb" :bar]
   ["bababc" :foobar]
   ["bba" :foo]
   ["c" :bar]
   ["cac" :foo]
   ["ccaabb" :foobar]
   ])

(trajectories ex1 (first (transducer3 ex1 3)))