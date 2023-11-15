(ns kigen.transducer.viz 
  "FLEXIBLE INPUT OUTPUT TRANSDUCER CONSTRUCTION
 input words can be sequences of any type of distinct entities
 similarly outputs can be of any types
 internal states are nonnegtaive integers"
  (:require
   [kigen.transducer.common :refer [input-symbols-fn]]
   [tangle.core :as tangle]
   [clojure.java.io :refer [copy file]]))

(defn DotTransducer
  "Produces GraphViz source code for the state transition diagram of a transducer.
   io-pairs are needed to define the names of the inputs and outputs."
  [io-pairs {omega :omega delta :delta}]
  (let [nodes (map
               (fn [state]
                 {:id (str "node" state)
                  :label (str state "," (omega state))})
               (range (count omega)))
        edges (mapcat
               (fn [input-sym]
                 (map
                  (fn [a b]
                    (if b
                      [(str "node" a) (str "node" b) {:label input-sym}]
                      [(str "node" a)  "nil" {:label input-sym}]))
                  (range) (delta input-sym)))
               (input-symbols-fn io-pairs))]
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