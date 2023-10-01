(require '[kigen.transducer.trie :refer :all])
(require '[kigen.transducer.minimal :refer [minimize-transducer]])
(require '[kigen.transducer.common :refer :all])

;;automata related functions

(defn experiment2
  [io-pairs]
  (let [T (transducer io-pairs)
        minT (minimize-transducer T)]
    (println (:n T) "->"  (:n minT))
    (println "works?" (check io-pairs minT) )))