;; Various diagram semigroup elements constructed as partitioned binary relations.
(ns kigen.diag)


(defn symmetric-gens
  "Generators of the symmetric group of degree n."
  [n]
  (let [transposition (concat [2 1] (range 3 (inc n)))
        cycle (concat (range 2 (inc n)) [1])]
    (map transformation->pbr [transposition cycle])))
