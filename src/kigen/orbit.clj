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
  [seed gens action]
  (let [og {:seed (set seed)
            :gens gens
            :graph {seed {}}
            :orbit #{seed}}
        funcs (for [g gens] #(action % g))
        indxs (range 0 (count funcs))]
    (loop [frontier [seed] og og]
      (let [frontier (for [x frontier i indxs] [((nth funcs i) x) {i x}])
            diff (filter (fn [[x]] (not (contains? (:orbit og) x))) frontier)
            nodes (map first diff)]
        (if (empty? nodes)
          og
          (recur nodes
                 {:seed seed
                  :gens (:gens og)
                  :orbit (into (:orbit og) nodes)
                  :graph (into (:graph og) diff)}))))))

(defn trace
  "Tracing a path to an element in the orbit graph"
  [e og]
  (drop 1 (rseq (loop [e e r []]
                  (if (nil? e)
                    r
                    (let [[[k v]] (seq (og e))]
                      (recur v (conj r k))))))))

(defn dfs
  [start ops]
  (loop [stack (into [] (for [op ops] [start op]))
         coll #{}]
    (if (empty? stack)
      coll
      (let [[e op] (peek stack)
            ne (op e)
            nstack (pop stack)]
        (if (contains? coll ne)
          (recur nstack coll)
          (recur (into nstack (for [op ops] [ne op])) (conj coll ne)))))))
