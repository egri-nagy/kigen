(require '[rolling-stones.core :as sat :refer [! at-least at-most exactly]])

(defn multabCNF [mt]
  (let [n (count mt)
        elts (range n)]
    (for [i elts j elts]
      [(- (inc i)) (- (inc j)) (inc (multab/at mt i j))])))

;;(map (partial filter pos?) (sat/solutions mt))

;; this slows down the SAT-solver
(defn gentabCNF [gens]
  (let [gt (gentab/gentab gens t/mul)
        m (count (:gens gt))
        n (count (:tab gt))
        tab (:tab gt)]
    (for [i (range n) j (range m)]
      [(- (inc i)) (- (inc j)) (inc (multab/at  tab i j))])))

(defn conjCNF [gens]
  (let [S (t/sgp-by-gens gens)
        vS (vec (sort S))
        mtvS (multab/multab vS t/mul)
        n (count vS)
        indices (vec (range n))
        t2i (clojure.set/map-invert (zipmap (range n) vS))
        ;; mapping indices to the index of the conjugacy rep
        conjreps (zipmap indices
                         (map (fn [x] (t2i (t/conjrep (vS x))))
                              indices))
        cjr2cjcl (group-by conjreps indices)
        r2l (into {} (map (fn [[k vs]] [(inc k) (mapv inc vs)]) cjr2cjcl))
        orderedks (vec (sort (keys r2l)))]
    orderedks))
