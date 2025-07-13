(ns kigen.digraph.transitivity
  "Function for checking and creating transitivity."
  (:require [clojure.set :refer [union]]
            [orbit.core :refer [full-orbit]]))

(defn transitive-closure
  "Computes the transitive closure of the given graph."
  [graph]
  (let [sources (group-by first graph)
        targets (group-by second graph)
        generator-fn (fn [[s t]]
                       (union (set (map
                                    (fn [[s' _]]
                                      [s' t])
                                    (targets s)))
                              (set (map
                                    (fn [[_ t']]
                                      [s t'])
                                    (sources t)))))]
    (full-orbit graph generator-fn)))