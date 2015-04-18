(ns kigen.sgp
  (:require [clojure.set :as set]))

;; semigroup by generators TODO maybe a general orbit algorithm will do
;; gens - generator elements
;; rightmul - right multiplication
(defn sgp-by-gens
  [gens rightmul]
  (let [funcs (for [x gens] #(rightmul % x))]
    (loop [orbit [(set gens)]
           total #{}]
      (let [frontline (last orbit)
            newelts (set (for [x frontline f funcs] (f x)))]
        (if (empty? frontline)
          (reduce into #{} orbit)
          (recur (conj orbit (set/difference newelts total))
                 (into total newelts)))))))
