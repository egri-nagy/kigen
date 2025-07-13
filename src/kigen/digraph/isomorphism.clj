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

(defn digraphs-up-to-morphisms
  "Given a collection of directed graphs, it returns the isomorphism
   class representatives."
  ([digraphs] (digraphs-up-to-morphisms #{}))
  ([digraphs representatives]
   (reduce
    (fn [reps G] ;representatives so far and the next semigroup
      (if (some (fn [H]
                  (first (digraph-isomorphisms G H)))
                reps)
        reps ;if G isomorphic to something in reps already
        (conj reps G))) ;otherwise keep it
    representatives
    digraphs)))
