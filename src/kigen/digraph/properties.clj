(ns kigen.digraph.properties
 "Some properties of the directed graph, possibly used as isomoprhism
  invariants.")

(defn out-in-degrees
  "Returns the out and in-degree pairs in a vector indexed by the nodes.
   `G` is a directed graph, `m` is the number of nodes (graph may not be
   compact)"
  [G m]
  (let [outs (frequencies (map first G))
        ins (frequencies (map second G))]
    (mapv (fn [i] [(outs i 0) (ins i 0)])
          (range m))))