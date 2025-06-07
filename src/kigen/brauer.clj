(ns kigen.brauer
  "Brauer-monoid direct implementation.
   A degree n diagram is represented by a single vector of length 2n.
   The value at position i is the image of point i." 
  (:require
    [kigen.sgp :as sgp]))

(defn alternate
  "Lazy infinite list repeating x y."
  [x y]
  (interleave (repeat x) (repeat y)))

(defn trace
  [start diagrams]
  (loop [point start
         processed #{start}
         diags diagrams]
    (let [npt ((first diags) point)]
      (if (or (nil? npt)
              (processed npt))
        [point processed]
        (recur npt (conj processed npt) (rest diags))))))

(defn mul
  [a b]
  (let [n (/ (count a) 2) ; get the degree
        nnils (repeat n nil) ; for padding 
        A (into a nnils) ; a with nils at the end
        B (into (vec nnils) ; b shifted by n, and n nils in front
                (mapv (partial + n) b))
        shift-back (fn [pt] (if (< pt n) pt (- pt n)))
        maps (loop [pairs []
                    processed #{}
                    pts (concat (range n) (range (* 2 n) (* 3 n)))]
               (if (empty? pts)
                 pairs
                 (let [pt (first pts)]
                   (if (processed pt)
                     (recur pairs
                            processed
                            (rest pts))
                     (let [diags (if (< pt n)
                                   (alternate A B)
                                   (alternate B A))
                           [img traced] (trace pt diags)]
                       (recur (conj pairs [(shift-back pt)
                                           (shift-back img)])
                              (into processed traced)
                              (rest pts)))))))] 
    (mapv second 
          (sort (into maps
                      (map (fn [[s t]] [t s]) maps))))))

(def i [3 4 5 0 1 2]) ;identity
(def t [4 3 5 1 0 2]) ; transposition
(def c [4 5 3 2 0 1]) ; cycle
(def l [1 0 5 4 3 2]) ;loop