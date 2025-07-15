(ns kigen.digraph.isomorphism
  "Deciding directed graph isomorphism. Directed graphs (digraphs) are sequences
   of source-target pairs. Vertices are non-negative integers."
  (:require [clojure.core.logic :as l]
            [kigen.logic :refer [lvar-vector]]))

(defn num-of-nodes
  "Returns the number of nodes - can be unsquashed."
  [G]
  (count (distinct (apply concat G))))

(defn out-in-degrees
  [G m]
  (let [outs (frequencies (map first G))
        ins (frequencies (map second G))]
    (mapv (fn [i] [(outs i 0) (ins i 0)]) (range m))))

(defn digraph-isomorphisms
  "Logic search for all isomorphisms of directed graph `G` to `H` given as
   a collection of arrows (ordered pair of integers). `m` is the number of
   vertices, when not given it is inferred from the graphs. "
  ([G H] (digraph-isomorphisms
          G
          H
          (inc (apply max (concat (apply concat G) (apply concat H))))))
  ([G H m]
   (let [phi (lvar-vector m) ;the morphism
         vertices (range m)
         Gdegrees (out-in-degrees G m)
         Hlookup (group-by (out-in-degrees H m) (range m))
         targets (mapv Hlookup Gdegrees)
         constraints
         (mapv (fn [[a b]] [(phi a) (phi b)]) ;substituting lvars into G arrows
               G)]
     (l/run*
      [q]
      (l/== q phi)
      ;(l/everyg (fn [v] (l/membero v vertices)) phi) ;phi maps to vertices
      (l/everyg (fn [v] (l/membero (phi v) (targets v))) vertices)
      (l/distincto phi) ;different vertices go to different vertices
      (l/everyg (fn [arrow] ;G-arrow mapped to H should be an arrow there
                  (l/membero arrow (vec H)))
                constraints)))))

(defn squash
  "If the graph has unused vertices, then this compactifies to a zero-starting
   no gap isomorphic representation."
  [G]
  (let [vertices (set (apply concat G))]
    (if (= (count vertices) (dec (apply max vertices)))
      G
      (let [zm (zipmap vertices (range))]
        (mapv (fn [arrow] (mapv zm arrow)) G)))))

(defn vec-mul
  [v1 v2]
  (reduce + (map * v1 v2)))

(defn nxn-mat-mul
  [A B]
  (let [coords (range (count A))]
    (mapv
     (fn [i]
       (mapv (fn [j] (vec-mul (nth A i)
                              (map (fn [v] (nth v j)) B)))
             coords))
     coords)))

(defn nxn-mat-square
  [A]
  (nxn-mat-mul A A))

(defn adjacency-matrix
  "returns the adjacency matrix of digraph G, G should be squashed"
  [G m]
  (let [arrows (set G)
        nodes (range m)]
    (mapv
     (fn [src] (mapv (fn [trg] (if ( contains? arrows [src trg])
                                 1
                                 0))
                     nodes))
     nodes)))

(defn signature
  "Just a quick graph isomorphism invariant: in-degrees sorted concatenated
   with out-degrees sorted."
  [G]
  (concat (sort (vals (frequencies (map first G))))
          (sort (vals (frequencies (map second G))))))


(defn signature2
  [G m]
  (sort (vals (frequencies (apply concat
                                  (nxn-mat-square (adjacency-matrix G m)))))))

(defn isomorphic?
  "Computes [[signature]] first for both graphs, if they match, it calls the
   heavier [[digraph-isomorphisms]]."
  [G H]
  (and (= (signature G) (signature H))
       (let [m (inc (apply max (concat (apply concat G) (apply concat H))))]
        (= (signature2 G m) (signature2 H m)))
       (first (digraph-isomorphisms G H))))

(defn iso-conj
  "Conjoining `G` in case it is not isomorphic to any of the digraphs in `reps`,
   otherwise returning `reps` unchanged."
  [reps G]
  (if (some (partial isomorphic? G) reps)
     reps ;if G isomorphic to something in reps already
     (conj reps G))) ;otherwise keep it

(defn digraphs-up-to-morphisms
  "Given a collection of directed graphs, it returns the isomorphism
   class representatives."
  ([digraphs] (digraphs-up-to-morphisms #{}))
  ([digraphs representatives]
   (reduce
    iso-conj
    representatives
    digraphs)))