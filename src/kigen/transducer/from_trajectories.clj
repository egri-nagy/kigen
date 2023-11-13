(ns kigen.transducer.from-trajectories
  "To find partially defined transducers, this method has the states in all
   trajectories as the logic variables. Then the individual maps are extracted
   and used to define the partial transformations.
   This method is inefficient as complexity of the search grows with the number 
   and the length of the input words."
(:require
 [clojure.core.logic :as l]
 [clojure.core.logic.fd :as fd]
 [taoensso.timbre :refer [info debug]]
 [kigen.transducer.common :refer [input-symbols-fn
                                  output-symbols-fn
                                  modded-io-pairs]]))

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
    [(l/== x1 x2) (l/== y1 y2)] ;the images are the same TODO: why do we need == x1 x2? investigate!
    [(l/!= x1 x2)]))) ;the preimages are different

(l/defne compatible-with-collo
  "Succeeds if the given mapping is compatible with all the mappings
   in the collection."
  [m coll]
  ([_ ()] l/succeed) ;nothing to contradict
  ([m [f . r]]
   (compatiblo m f)
   (compatible-with-collo m r)))

(l/defne compatible-collo
  "Succeeds if all the mappings in the collection are compatible
   with each other. O(n^2)"
  [coll]
  ([()] l/succeed) ;nothing to contradict
  ([[f . r]]
   (compatible-with-collo f r) ;first is compatible with rest
   (compatible-collo r))) ;rest is compatible in itself

(defn transducer
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
                             (filter l/lvar? (apply concat trajectories))))
        state-dom (apply fd/domain (range n))
        output-dom (apply fd/domain (range (count output-symbols)))]
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
          :omega (mapv (fn [i] (when (int? i) (output-symbols i))) ;defending against nil
                       (ts readout-symbol))
          :n n}))
     (l/run* [q]
             (l/== q trajectories)
             (l/everyg #(fd/in % state-dom) state-lvars)
             (l/everyg #(fd/in % output-dom) output-lvars)
             (l/everyg compatible-collo (vals mappings))))))