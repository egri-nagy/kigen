(ns kigen.sgp
  (:require [clojure.set :as set]))


;; semigroup by generators TODO maybe a general orbit algorithm will do
;; gens - generator elements
;; rightmul - right multiplication
(defn sgp-by-gens
  [gens rightmul]
  (let [funcs (for [x gens] #(rightmul % x))]
    (loop [orbit [(set gens)]]
      (if (empty? (last orbit))
        orbit
        (recur (conj orbit (set/difference
                            (set (for [x (last orbit) f funcs] (f x)))
                            (reduce set/union #{} orbit))))))))
