(ns kigen.matrix
  "Simple matrix computations.")

(defn dot-product
  "Computes the dot product of the vectors `v1` and `v2`."
  [v1 v2]
  (reduce + (map * v1 v2)))

(defn mat-mul
  "Matrix multiplication (assuming correct dimensions)."
  [A B]
  (let [coords (range (count A))]
    (mapv
     (fn [i]
       (mapv (fn [j] (dot-product (nth A i)
                                  (map (fn [v] (nth v j)) B)))
             coords))
     coords)))

(defn mat-square
  "Squaring a nxn matrix."
  [A]
  (mat-mul A A))