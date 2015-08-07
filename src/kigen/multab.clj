(ns kigen.multab
  (:use [kigen.pos :as pos]))

(defn multab
  "Returns the multiplication table of the elements xs by the function mul."
  [xs mul]
  (let [indices (pos/index (vec xs))]
    (vec (map
          (fn [x] (->> xs
                       (map #(mul % x))
                       (map #(pos/pos (partial = %) xs indices))
                       (vec)))
          xs))))
