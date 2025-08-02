(ns kigen.table.gentab
  "Generator tables - multiplication tables defined only for multiplying by
   generators."
  (:require [kigen.semigroup.sgp :refer [sgp-by-gens ->Sgp]]))

(defn gentab
  "Right generation table for semigroup given by generator elements and
  multiplication. Returns the generators (integers 0,..,n-1) and the
   multiplication by generators function."
  [gens mul]
  (let [S (sgp-by-gens gens mul)
        elts (vec (concat gens (remove (set gens) S))) ;generators first
        indices (zipmap elts (range)) ;elts -> indices
        gt (vec (pmap ; targeting big semigroups
                 (fn [x] (->> gens
                              (map (partial mul x))
                              (mapv indices)))
                 elts))]
    (->Sgp (range (count gens)) ;generators
           (fn [x y] ((gt x) y))))) ;multiplication
