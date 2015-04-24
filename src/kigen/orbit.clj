(ns kigen.orbit
  (:require [clojure.set :as set]))


;; seed - elements to act on
;; funcs -
(defn orbit
  [seed funcs]
  ;; o - vector of sets containing semigroup elements in production order
  ;; total - cumulative union of orbit element
  (loop [o [(set gens)] total (first o)]
    (let [newelts (set (for [x (last o) f funcs] (f x)))
          diff (set/difference newelts total)]
      (if (empty? diff)
        total
        (recur (conj o diff) (into total diff))))))
