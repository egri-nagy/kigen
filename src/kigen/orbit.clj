(ns kigen.orbit
  (:use [clojure.set :only [difference]]))

(declare alternating-orbit)

;; seed - elements to act on
;; funcs - functions that produce a new element applied to an element
(defn orbit
  [seed funcs]
  (alternating-orbit seed (cycle [funcs])))

;; seed - elements to act on
;; funcs-seq - sequence of function colls
;; in each step we may apply different set of functions
(defn alternating-orbit
  [seed funcs-seq]
  ;; o - vector of sets containing orbit elements in production order
  ;; total - cumulative union of orbit element
  (loop [o [(set  seed)]
         total (first o)
         funcs-seq funcs-seq]
    (let [newelts  (for [x (last o) f (first funcs-seq)] (f x))
          newset (reduce into #{} newelts)
          diff (difference newset total)]
      (if (empty? diff)
        total
        (recur (conj o diff)
               (into total diff)
               (rest funcs-seq))))))
