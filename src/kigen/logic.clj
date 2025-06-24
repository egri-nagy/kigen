(ns kigen.logic
  "Further relations (goals) to extend core.logic."
  (:require [clojure.core.logic :as l]))

(declare reduceo
         ntho)

(l/defne reduceo
  "Relational reduce, succeds if the reduction produces the result."
  [relo initial coll result]
  ([_ _ () _] (l/== initial result))
  ([_ _ [next-item . remaining] _]
   (l/fresh [result-so-far]
            (relo initial next-item result-so-far)
            (reduceo relo result-so-far remaining result))))

;todo: consider a version that cuts after reaching index
(defn ntho
  "Succeeds if the sequential collection `coll` has `content` at the position
   `index`. Classic recursive implementation."
  ([coll index content] (ntho coll index content 0)) ;initializing counter
  ([coll index content i]
   (l/fresh [head tail]
            (l/firsto coll head)
            (l/resto coll tail)
            (l/conde [(l/== head content) (l/== index i)] ;match at right place
                     [(ntho tail index content (inc i))])))) ;otherwise recur