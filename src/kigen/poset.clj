(ns kigen.poset)

;; elts - set of elements
;; rel - a partial order relation predicate
(defn hasse-diagram
  "Creates the Hasse-diagram of a partial order as a graph
  (map: element -> set of covers), given by the elements and a predicate."
  [elts rel]
  (letfn [(recalc-covers [covers newval] ;recalculate the set of covers
            (if (some #(rel newval %) covers) ;nothing to do newval is below
              covers
              (conj (set (filter #(not (rel % newval)) covers)) newval)))
          (insert [hd e] ;insert an element into the graph by updating covers
            (let [candidates (filter #(and (not (= e %))
                                           (rel e %))
                                     (keys hd))]
              (reduce #(assoc % %2 (recalc-covers (%  %2) e))
                      hd
                      candidates)))]
    ;starting with the empty covering sets we insert all elements
    (reduce insert
            (into {} (for [e elts] [e #{}]))
            elts)))

(defn chain-extensions
  [chain hd]
  (map #(conj chain %) (hd (last chain))))

;; set
;; hd Hasse-diagram
(defn all-chains
  [e hd]
  (letfn [(f [x] (apply concat (map #(chain-extensions % hd) x)))]
    (last (take-while #(not (empty? %)) (iterate f [[e]])))))
