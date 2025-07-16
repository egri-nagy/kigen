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

(defn adjacency-matrix
  "returns the adjacency matrix of digraph G, G should be compact"
  [G m]
  (let [arrows (set G)
        nodes (range m)]
    (mapv
     (fn [src] (mapv (fn [trg] (if (contains? arrows [src trg])
                                 1
                                 0))
                     nodes))
     nodes)))

(defn compact
  "If the graph has unused vertices, then this compactifies to a zero-starting
   no gap isomorphic representation."
  [G]
  (let [vertices (set (apply concat G))]
    (if (= (count vertices) (dec (apply max vertices)))
      G ;already compact
      (let [zm (zipmap vertices (range))]
        (mapv (fn [arrow] (mapv zm arrow)) G)))))