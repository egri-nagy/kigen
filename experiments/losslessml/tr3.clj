; trajectory based transducer construction
(require '[kigen.logic :as kl])
(require '[clojure.core.logic :as l])
(require '[clojure.core.logic.fd :as fd])
(require '[taoensso.timbre :refer [info debug]])
(require '[kigen.position :refer [index]])

; levels: :warn, :info, :debug
(timbre/set-min-level! :debug)

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
(count (modded-io-pairs sl-3-3))

;we can get groups of dominoes 
;for each group  finding the distinct elements should be not more than 

(let [prepped (prepare-logic-variables (modded-io-pairs sl-3-3))
      lvars (map second prepped)
      dominoes (mapcat extracting-dominoes prepped)]
  (l/run 1 [q]
         (l/== q lvars)
         (l/everyg (fn [x] (fd/in x (fd/domain 0 1 2))) (apply concat lvars))))
