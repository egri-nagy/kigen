(ns kigen.sgp
  (:require [clojure.set :as set]))

;; semigroup by generators TODO maybe a general orbit algorithm will do
;; gens - generator elements
;; rightmul - right multiplication
(defn sgp-by-gens
  [gens rightmul]
  (let [funcs (for [x gens] #(rightmul % x))]
    (loop [orbit [(set gens)] total #{}]
      (let [newelts (set (for [x (last orbit) f funcs] (f x)))
            diff (set/difference newelts total)]
        (if (empty? diff)
          (reduce into #{} orbit)
          (recur (conj orbit diff) (into total diff)))))))
