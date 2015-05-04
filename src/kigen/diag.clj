;; Various diagram semigroup elements constructed as partitioned binary relations.
(ns kigen.diag)

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

(defn symmetric-gens
  "Generators of the symmetric group of degree n."
  [n]
  (let [transposition (concat [2 1] (range 3 (inc n)))
        cycle (concat (range 2 (inc n)) [1])]
    (map transformation->pbr [transposition cycle])))


(defn full-ts-gens
  "Generators of the full transformation semigroup of degree n."
  [n]
  (let [collapse (concat [1 1] (range 3 (inc n)))]
    (concat (symmetric-gens n) [(transformation->pbr collapse)])))
