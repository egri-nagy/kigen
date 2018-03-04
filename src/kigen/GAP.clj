(ns kigen.GAP
  "Functions for transferring data between GAP and kigen."
  (:require [clojure.string :refer [replace]]))

(defn GAPtransformations->KIGENtransfs
  [s]
  (let [gens (-> s
                 (replace "Transformation" "")
                 (replace "(" "")
                 (replace ")" "")
                 (read-string))]
       (mapv (partial mapv dec) gens)))
