(ns kigen.holonomy
  (:use [kigen.orbit :as o]))

;; elts - set of elements
;; rel - a partial order relation pred on elts
(defn hasse-diagram
  [elts rel]
  (letfn [(recalc-covers [covers newval]
            (if (some #(rel newval %) covers)
              covers
              (conj (set (filter #(not (rel % newval)) covers)) newval)))
          (insert [hd e]
            (let [candidates (filter #(and (not (= e %))
                                           (rel e %))
                                     (keys hd))]
              (reduce #(assoc % %2 (recalc-covers (%  %2) e)) hd candidates)))]
    (reduce insert (into {} (for [e elts] [e #{}])) elts)))

;; set
;; hd Hasse-diagram
(defn tile-chain
  [set hd]
  )
