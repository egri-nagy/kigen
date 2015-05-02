;; Various diagram semigroup elements constructed as partitioned binary relations.
(ns diags)

(defn transformation->pbr
"Creates a partitioned binary relation from a transformation
  given by the list of images.
  Transformations index point from 1, unlike the vector indices."
[imagelist]
(let [emptyset #{}
      n (count imagelist)
      pbr {:dom (set (range 1 (inc n)))
           :cod (set (range (inc n) (inc (* 2 n))))}
      edges (into {} (map
                      #(vector % (set [(+ n (nth imagelist (dec %)))]))
                      (:dom pbr)))
      non-edges (into {} (map
                          #(vector % emptyset)
                          (:cod pbr)))]
  (reduce into pbr (concat [edges non-edges]))))
