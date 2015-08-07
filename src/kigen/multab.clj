(ns kigen.multab
  (:use [kigen.orbit :as o]
        [kigen.transf :as t]
        [kigen.poset :as p]
        [clojure.set :as set]))

(defn multab
  "Returns the multiplication table of the elements xs by the function mul."
  [xs mul]
  (vec (pmap #(vec (map (partial mul %) xs)) xs)))
