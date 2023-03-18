(ns kigen.logic
  "Relations to extend core.logic."
  (:require [clojure.core.logic :as l]))

(l/defne reduceo
  "Relational reduce, succeds if the reduction produces the result."
  [relo initial coll result]
  ([_ _ () _] (l/== initial result))
  ([_ _ [next-item . remaining] _]
   (l/fresh [result-so-far]
            (relo initial next-item result-so-far)
            (reduceo relo result-so-far remaining result))))

(defn ntho
  ([coll n x] (ntho coll n x 0))
  ([coll n x current]
   (l/fresh [head tail]
            (l/firsto coll head)
            (l/resto coll tail)
            (l/conde [(l/== head x) (l/== n current)]
                     [(ntho tail n x (inc current))]))))