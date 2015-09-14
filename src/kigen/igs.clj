(ns kigen.igs
  (:use [clojure.set :only [difference union]]
        [kigen.multab :as multab :only [closure]]))

(defn is?
  "Decides whether the set A is an independent set in multiplication table mt."
  [mt A]
  (every?
   #(not (contains? (multab/closure mt (difference A #{%})) %))
   A))

(defn min-extensions
  "All independent sets that are extensions of the independent set is."
  [mt is]
  (let [complement (difference (set (range (count mt))) is)]
    (filter #(is? mt %) (set (map #(union is #{%}) complement)))))
