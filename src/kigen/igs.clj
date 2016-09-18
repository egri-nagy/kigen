(ns kigen.igs
  (:require [clojure.set :refer [difference union]]
            [kigen.multab :as multab]))

(defn is?
  "Decides whether the set A is an independent set in multiplication table mt."
  [mt A]
  (every?
   #(not (contains? (multab/closure mt (difference A #{%})) %))
   A))

(defn min-extensions
  "All independent sets that are extensions of the independent set I."
  [mt I]
  (let [complement (difference (multab/elts mt) I)]
    (filter #(is? mt %) (set (map #(union I #{%}) complement)))))
