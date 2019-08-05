(ns kigen.GAP
  "Functions for transferring data between GAP and kigen."
  (:require [clojure.string :as string]))

(defn GAPtransformations->KIGENtransfs
  [s]
  (let [gens (-> s
                 (string/replace "Transformation" "")
                 (string/replace "(" "")
                 (string/replace ")" "")
                 (read-string))]
       (mapv (partial mapv dec) gens)))
