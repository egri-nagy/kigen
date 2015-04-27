(ns kigen.orbit
  (:use [clojure.set :only [difference]]))

;; seed - elements to act on
;; funcs - functions that produce a new element applied to an element
(defn orbit
  [seed funcs]
  ;; o - vector of sets containing orbit elements in production order
  ;; total - cumulative union of orbit element
  (loop [o [(set seed)] total (first o)]
    (let [newelts (set (for [x (last o) f funcs] (f x)))
          diff (difference newelts total)]
      (if (empty? diff)
        total
        (recur (conj o diff) (into total diff))))))
