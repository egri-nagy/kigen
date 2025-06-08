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
  "Trace a path starting from the given point. This is used when stacking two
   diagrams for composition. The second argument, diagrams, should contain a
   lazy infinite list, alternating the diagrams. When composing a and b, tracing
   a point from a should have the list a,b,a,b,a,b,...; while tracing from b
   should have b,a,b,a,..."
  [start diagrams]
  (loop [point start
         processed #{start}
         diags diagrams]
    (let [npt ((first diags) point)] ; we follow an edge here 
      (if (or (nil? npt)
              (processed npt))
        [point processed]
        (recur npt (conj processed npt) (rest diags))))))

(defn complete
  "Given half the maps individually (arrows in one direction), this completes
   the diagram."
  [maps]
  (mapv second ; the second coord becomes the image
        (sort ;by the first coordinate
         (into maps ;combine the existing maps with their reverses
               (map
                (fn [[s t]] [t s]) ;the opposite direction
                maps)))))

(defn mul
  "Composing two Brauer diagrams, assumed to be same degree."
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
                   (if (processed pt) ; it's already traced, go to next point
                     (recur pairs
                            processed
                            (rest pts))
                     (let [diags (if (< pt n) ; are we tracing from top?
                                   (alternate A B)
                                   (alternate B A))
                           [img traced] (trace pt diags)]
                       (recur (conj pairs [(shift-back pt)
                                           (shift-back img)])
                              (into processed traced)
                              (rest pts)))))))]
    (complete maps)))

(defn brauer-gens
  "The standard generators for the symmetric group and a cup-cap."
  [n]
  (let [identity (when (= n 1)
                   (complete [[0 1]]))
        transposition (when (> n 1)
                        (into [[0 (inc n)] [1 n]]
                              (map (fn [x] [x (+ x n)])
                                   (range 2 n))))
        cycle (when (> n 2)
                (into [[(dec n) n]]
                      (map (fn [x] [x (inc (+ x n))])
                           (range (dec n)))))
        cup-cap (when (> n 1)
                  (into [ [0 1] [n (inc n)]]
                        (map (fn [x] [x (+ x n)])
                             (range 2 n))))]
    (map complete
         (remove nil? [identity transposition cycle cup-cap]))))


(def i [3 4 5 0 1 2]) ;identity
(def t [4 3 5 1 0 2]) ;transposition
(def c [4 5 3 2 0 1]) ;cycle
(def l [1 0 5 4 3 2]) ;loop