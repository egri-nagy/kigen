(require '[kigen.transducer.trie :refer :all])
(require '[kigen.transducer.minimal :refer [minimize-transducer]])
(require '[kigen.transducer.common :refer :all])
(require '[clojure.set :refer [difference intersection union]])

;;automata related functions


(def i-o [["" :mixed]
          ["aa" :as]
          ["bb" :bs]
          ["ab" :mixed]
          ["ba" :mixed]])

(def suffs [["kisnyuszi" :ok]
            ["nagynyuszi" :ok]
            ["nyuszimuszi" :ok]
            ["farkas" :nem]
            ["roka" :nem]])

(def ex3 [["aaa" :1]
          ["baa" :1]
          ["abb" :2]])

(def ex4 [["aaa" :1]
          ["baa" :1]])

(def A {:delta {\a [1 0 4 4 4 5] \b [2 3 5 5 5 5]}
        :omega [:reject :reject :accept :accept :accept :reject]
        :n 6})

(def HU {:delta {\a [1 6 0 2 7 2 6 6] \b [5 2 2 6 5 6 4 2]}
         :omega [:reject :reject :accept :reject :reject :reject :reject :reject]
         :n 8})



(defn experiment2
  [io-pairs]
  (let [T (transducer io-pairs)
        minT (minimize-transducer T)]
    (println (:n T) "->"  (:n minT))
    (println "works?" (check io-pairs minT) )))

(experiment2 suffs)