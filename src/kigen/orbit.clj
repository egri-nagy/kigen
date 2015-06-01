(ns kigen.orbit
  (:use [clojure.set :only [difference]]))

(declare orbit
         alternating-orbit
         orbit-graph)

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

(defn orbit-graph
  [seed funcs]
  (let [og {:seed (set seed)
            :gens (vec funcs)
            :graph #{seed #{}}
            :orbit #{seed}}
        indxs (range 0 (count funcs))]
    (loop [frontier [seed]
           og og]
      (let [frontier (for [x frontier i indxs] [((nth funcs i) x) i])
            diff (filter (fn [[x]] (not (contains? (:orbit og) x))) frontier)
            nodes (map first diff)]
        (println og)
        (if (empty? nodes)
          og
          (recur nodes
                 {:seed seed
                  :gens (:gens og)
                  :orbit  (into (:orbit og) nodes)
                  :graph (reduce into (:graph og) diff)}))))))
