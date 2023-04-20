; trajectory based transducer construction
(require '[kigen.logic :as kl])
(require '[clojure.core.logic :as l])
(require '[clojure.core.logic.fd :as fd])
(require '[taoensso.timbre :refer [info debug set-min-level!]])
(require '[kigen.position :refer [index]])
(require '[clojure.pprint :refer [pprint]])


; levels: :warn, :info, :debug
(set-min-level! :debug)

;;unchanged from library code ;;;;;;;;;;;;;;;;;
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
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def OGS :_OGS) ;;DO-NOT-USE-THIS-AS INPUT!

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

(defn compatiblo
  [m1 m2]
  (l/conde
  [(l/fresh [x1 x2]
            (l/firsto m1 x1)
            (l/firsto m2 x2)
            (l/!= x1 x2))]
   [(l/fresh [y1 y2]
             (kl/ntho m1 2 y1)
             (kl/ntho m2 2 y2)
             (l/== y1 y2))]))

(l/defne compatiblo2
  [m1 m2]
  ([[x1  y1] [x2  y2]]
   (l/conde
    [(l/== y1 y2)]
    [(l/!= x1 x2)])))

(l/run* [q]
       (l/fresh [a b]
                (l/== q [a b])
                (l/membero a [0 1 2])
                (l/membero b [0 1 2])
                (compatiblo [0 0] q)))

(l/defne compatible-with-collo
  [m coll]
  ([_ ()] l/succeed)
  ([m [f . r]]
   (compatiblo2 m f)
   (compatible-with-collo m r)))

(l/defne compatible-collo
  [coll] 
  ([()] l/succeed)
  ([[f . r]]
   (compatible-with-collo f r)
   (compatible-collo r)))

(l/run* [q]
       (l/== q [[(l/lvar) (l/lvar)] [(l/lvar) (l/lvar)]])
       (compatible-collo q))

(let [prepped (prepare-logic-variables (modded-io-pairs sl-3-3))
      lvars (map second prepped)
      m (group-by first
                  (mapcat extracting-dominoes (prepare-logic-variables m-sl-3-3)))
      dominoes (into {} (map (fn [[k vs]] [k (map second vs)]) m))]
  (pprint dominoes)
  (l/run 1 [q]
         (l/== q lvars)
         (l/everyg (fn [x] (fd/in x (fd/domain 0 1 2))) (apply concat lvars))
         (l/everyg (fn [[_ ds]] (compatible-collo ds))
                   dominoes)))
