(ns kigen.gentab
  "Table for constant time right multiplication by generators."
  (:require [kigen.sgp :refer [sgp-by-gens]]))

;;TODO this has to be spec-ed; why putting the generators in the front?

(defn gentab
  "Right generation table for semigroup given by generator elements and
  multiplication."
  [gens mul]
  (let [S (sgp-by-gens gens mul)
        elts (vec (concat gens (remove (set gens) S)))
        indices (zipmap elts (range (count elts)))
        gt (vec (pmap
                 (fn [x] (->> gens
                              (map #(mul x %))
                              (map indices)
                              (vec)))
                 elts))]
    {:gens (range (count gens))
     :tab gt
     :elts elts
     :indices indices
     :mul (fn [x y] ((gt x) y))}))
