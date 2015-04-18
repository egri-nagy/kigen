(ns kigen.sgp
  (:require [clojure.set :as set]))

;; semigroup by generators TODO maybe a general orbit algorithm will do
;; gens - generator elements
;; mul - multiplication for generator elements
(defn sgp-by-gens
  [gens mul]
  (let [funcs (for [x gens] #(mul % x))] ;right multiplication functions
    ;; orbit - vector of sets containing semigroup elements in production order
    ;; total - cumulative union of orbit elements
    (loop [orbit [(set gens)] total (first orbit)]
      (let [newelts (set (for [x (last orbit) f funcs] (f x)))
            diff (set/difference newelts total)]
        (if (empty? diff)
          total
          (recur (conj orbit diff) (into total diff)))))))
