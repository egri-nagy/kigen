(ns kigen.digraph.isomorphism
  "Deciding directed graph isomorphism."
  (:require [clojure.core.logic :as l]
            [kigen.logic :refer [lvar-vector]]))

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
         constraints
         (mapv (fn [[a b]] [(phi a) (phi b)]) ;substituting lvars into G arrows
               G)]
     (l/run*
      [q]
      (l/== q phi)
      (l/everyg (fn [v] (l/membero v vertices)) phi) ;phi maps to vertices
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

(defn signature
  "Just a quick graph isomorphism invariant: in-degrees sorted concatenated
   with out-degress sorted."
  [G]
  (concat (sort (vals (frequencies (map first G))))
          (sort (vals (frequencies (map second G))))))


(defn isomorphic?
  [G H]
  (and (= (signature G) (signature H))
       (first (digraph-isomorphisms G H))))

(defn iso-conj
  "Conjoining `G` in case it is not isomorphic to any of the graphs in `reps`,
   otherwise returning `reps`."
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
