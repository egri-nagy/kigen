(require '[rolling-stones.core :as sat :refer [! at-least at-most exactly]])

(defn CNF [mt]
  (let [n (count mt)
        elts (range n)]
    (for [i elts j elts]
      [(- (inc i)) (- (inc j)) (inc (multab/at mt i j))])))

;;(map (partial filter pos?) (sat/solutions mt))

(defn betterCNF [gens]
  (let [gt (gentab/gentab gens t/mul)
        m (count (:gens gt))
        n (count (:tab gt))
        tab (:tab gt)]
    (for [i (range n) j (range m)]
      [(- (inc i)) (- (inc j)) (inc (multab/at  tab i j))])))

