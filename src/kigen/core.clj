(ns kigen.core
  (:require [kigen.pbr]
            [kigen.transf :as transf]
            [kigen.perm :as perm]
            [kigen.holonomy]
            [kigen.poset]
            [kigen.sgp]
            [kigen.multab]
            [kigen.igs]
            [kigen.orbit])
  (:gen-class))

(defn -main
  "just a main"
  [& args]
  (println "REPL me please!"))
