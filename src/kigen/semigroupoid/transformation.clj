(ns kigen.semigroupoid.transformation
  "Transformation semigroupoids.
   :s - source, domain
   :t - target, codomain
   :m - morphism, map"
  (:require [clojure.set :refer [union]]
            [orbit.core :refer [full-orbit]]))

; example transformation semigroupoid
(def ExA1 ; example from semigroupoid paper
  {:objects [2,3]
   :gens [{:s 0, :t 0, :m [1 0]}
          {:s 1, :t 1, :m [1 2 0] }
          {:s 0, :t 1, :m [0 0]}
          {:s 1, :t 0, :m [0 0 0]}]})

(defn compose
  "Right multiplication of typed transformations.
   Returns nil when the arrows a and b are not composable."
  [a b]
  (when (= (:t a) (:s b))
    {:s (:s a)
     :t (:t b)
     :m (mapv (:m b) (:m a))}))

(defn morphisms
  "Returns a hash-map with key [dom,codomain] pairs and values are the
   corresponding arrows."
  [S]
  (update-vals
   (group-by (fn [a] [(:s a) (:t a)]) S)
   (partial map :m)))

(defn sgpoid-by-gens
  [gens]
  (let [sources (group-by :s gens)
        targets (group-by :t gens)
        generator-fn
        (fn [a]
          (union
           (set (map (partial compose a)
                     (sources (:t a) [])))
           (set (map #(compose % a)
                     (targets (:s a) [])))))]
    (full-orbit gens generator-fn)))

(sgpoid-by-gens (:gens ExA1))
(def ExA1full (sgpoid-by-gens (:gens ExA1)))

(morphisms ExA1full)
(count ExA1full)