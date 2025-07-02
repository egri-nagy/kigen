(ns kigen.semigroupoid.viz 
  ""
  (:require
   [tangle.core :as tangle]
   [clojure.java.io :refer [copy file]]))

;; todo this is repeated!
(def symbols (vec "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"))

(defn DotSemigroupoid
  "`S` symbolic composition tab
   `arrows` of the semigroupoid"
  [S arrows]
  (let [nodes (map
               (fn [type]
                 {:id (str "node" type)
                  :label (str "t" type)})
               (range (inc (apply max (apply concat arrows)))))
        edges (map
               (fn [i] 
                 [(str "node" (first (arrows i)))
                  (str "node" (second (arrows i))) {:label (symbols i)}])
               (range (count arrows)))]
    (tangle/graph->dot
     nodes
     edges
     {:directed? true ;:node {:shape :box}
      :node->id (fn [n] (if (keyword? n) (name n) (:id n)))
      :node->descriptor (fn [n] (when-not (keyword? n) n))})))

(defn Dot2PDF
  "Exporting a .dot file to its generated PDF image."
  [dotstring name]
  (copy (tangle/dot->image dotstring
                           "pdf")
        (file (str name ".pdf"))))