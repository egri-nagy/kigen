(ns kigen.holonomy
  (:use [kigen.orbit :as o]))

;; elts - set of elements
;; rel - a partial order relation pred on elts
(defn hasse-diagram
  [elts rel]
  (letfn [(insert [hd key newval]
            (assoc hd key (conj (hd key) newval)))
          (g [hd e]
            (reduce #(insert % %2 e) hd  (filter #(rel e %) (keys hd))))]
    (reduce g (into {} (for [e elts] [e #{}])) elts)))

;; set
;; hd Hasse-diagram
(defn tile-chain
  [set hd]
  )
