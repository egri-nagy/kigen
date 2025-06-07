(ns kigen.brauer
  "Brauer-monoid direct implementation.
   A degree n diagram is represented by a single vector of length 2n.
   The value at position i is the image of point i." 
  (:require
    [kigen.multab :as multab]))

(defn mul
  [a b]
  (let [n (/ (count a) 2) ; get the degree
        nnils (repeat n nil) ; for padding 
        A (into a nnils) ; a with nils at the end
        B (into (vec nnils) ; b shifted by n, and n nils in front
                (mapv (partial + n) b))]
    [A B]))

(def a [3 4 5 0 1 2]) ;identity