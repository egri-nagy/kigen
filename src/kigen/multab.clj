(ns kigen.multab
  (:use [kigen.pos :as pos]))

(defn multab
  "Returns the multiplication table of the elements xs by the function mul."
  [xs mul]
  (let [indices (pos/index (vec xs))]
    (map
     (fn [x]
       (map
        #(pos/pos (partial = %) xs indices)
        (map #(mul x %) xs)))
     xs)))
