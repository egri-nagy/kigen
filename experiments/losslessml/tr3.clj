; trajectory based transducer construction
(require '[kigen.logic :as kl])
(require '[clojure.core.logic :as l])
(require '[clojure.core.logic.fd :as fd])
(require '[taoensso.timbre :refer [info debug set-min-level!]])
(require '[kigen.position :refer [index]])
(require '[clojure.pprint :refer [pprint]])
(require '[clojure.math.combinatorics :as combo])
(require '[kigen.transducer :refer :all])

; levels: :warn, :info, :debug
(set-min-level! :debug)


(defn modded-io-pairs
  "Recodes the input symbols to natural numbers and adds an extra input at
   the end serving for the state readout."
  [io-pairs]
  (let [input-symbols (input-symbols-fn io-pairs)
        readout-symbol (count input-symbols)]
    (for [[input output] io-pairs]
      [(vec (concat (map
                     (partial index input-symbols)
                     input)
                    [readout-symbol]))
       (index (output-symbols-fn io-pairs) output)])))

(defn prepare-logic-variables
  " "
  [modded-io-pairs]
  (for [[inputs output] modded-io-pairs]
    (let [n (count inputs)]
      [inputs
       (concat [0] (repeatedly (dec n) l/lvar) [ output])])))

(defn extracting-dominoes
  " "
  [[inputs trajectory]]
;  (println ">" inputs (partition 2 1 trajectory))
  (partition 2 (interleave inputs (partition 2 1 trajectory))))

(def sl-3-3
  [["|__" :first]
   ["_|_" :second]
   ["__|" :third]])

(output-symbols-fn sl-3-3)
(input-symbols-fn sl-3-3)
(def m-sl-3-3 (modded-io-pairs sl-3-3))
(prepare-logic-variables m-sl-3-3)
(map second (prepare-logic-variables m-sl-3-3))
(def m
  (group-by first
            (mapcat extracting-dominoes (prepare-logic-variables m-sl-3-3))))
(into {} (map (fn [[ k vs]] [k (map second vs)]) m))

;we can get groups of dominoes 
;for each group  finding the distinct elements should be not more than 

(l/defne counto
  [coll n]
  ([() n] (l/== n 0))
  ([[_ . r] n] (counto r (dec n))))

(l/run 1 [q]
       (counto q 22))

(defn smallo
  [coll n]
  (l/fresh [size]
           (counto coll size)
           (l/project [size n] (l/== size n))))

(l/run 1 [q]
       (l/distincto q 2))

(l/run* [a b] (l/membero a [1 2]) (l/membero b [1 2]) (l/distincto [a b]))


(l/defne compatiblo
  "This goal succeeds if the two mappings are compatible, i.e.
   they can be in the same transformation."
  [m1 m2]
  ([[x1  y1] [x2  y2]]
   (l/conde
    [(l/== x1 x2)(l/== y1 y2)] ;the images are the same TODO: why do we need == x1 x2? investigate!
    [(l/!= x1 x2)]))) ;the preimages are different

(l/run*
 [q]
 (l/membero q [0 1 2])
 (compatiblo [1 2] [1 q]))

(l/run*
 [q]
 (l/membero q [0 1 2])
 (compatiblo [1 2] [q 2]))


(let [X (range 3)]
  (l/run* [q]
          (l/fresh [a b]
                   (l/== q [a b])
                   (l/membero a X)
                   (l/membero b X)
                   (compatiblo [:x 0] q))))


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

(l/run* [q]
       (l/== q [[(l/lvar) (l/lvar)] [(l/lvar) (l/lvar)]])
       (compatible-collo q))

(defn transducer3 
  [io-pairs n]
  (let [output-symbols (output-symbols-fn io-pairs)
        input-symbols (input-symbols-fn io-pairs)
        prepped (prepare-logic-variables
                 (modded-io-pairs io-pairs))
        lvars (map second prepped)
        m (group-by first
                    (mapcat extracting-dominoes prepped))
        dominoes (into {} 
                       (map
                        (fn [[k vs]] [k (map second vs)])
                        m))]
  ;(pprint prepped)
  ;(pprint lvars)
  ;(pprint dominoes)
    (map
     (fn [solution]
       (let [x (update-vals
                (group-by first
                          (distinct
                           (mapcat extracting-dominoes
                                   (map vector
                                        (map first prepped)
                                        solution))))
                (partial map second)) 
             xx (update-vals x ;identity
                             (fn [l]
                               (let [v (vec (repeatedly n (constantly nil)))] 
                                 (reduce
                                  (fn [vv [s d]]
                                    (assoc vv s d))
                                  v
                                  l))))
             ;xxx (update-keys xx (input-symbols-fn io-pairs))
             ]
         (pprint xx) (pprint output-symbols)
         {:delta (update-keys (dissoc xx  (count input-symbols))
                              input-symbols)
          :omega (mapv output-symbols  (xx (count input-symbols)))
          }))
     (l/run* [q]
            (l/== q lvars)
            (l/everyg (fn [x] (fd/in x (apply fd/domain (range n))))
                      (apply concat lvars))
            (l/everyg compatible-collo
                      (vals dominoes))))))

(check sl-3-3 (first (transducer3 sl-3-3 3)))