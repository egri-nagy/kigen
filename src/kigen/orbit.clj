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

;; seed - elements to act on
;; func-seq - sequence of functions
(defn alternating-orbit
  [seed func-seq]
  ;; o - vector of sets containing orbit elements in production order
  ;; total - cumulative union of orbit element
  (loop [o [(set  seed)] total (first o) func-seq func-seq]
    (let [newelts (reduce into #{} (for [x (last o)] ((first func-seq) x)))
          diff (difference newelts total)]
      (if (empty? diff)
        total
        (recur (conj o diff) (into total diff) (rest func-seq))))))
