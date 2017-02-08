(ns kigen.core
  (:require [kigen.pbr :as pbr]
            [kigen.transf :as transf]
            [kigen.perm :as perm]
            [kigen.holonomy :as holonomy]
            [kigen.poset :as poset]
            [kigen.sgp :as sgp]
            [kigen.multab :as multab]
            [kigen.igs :as igs]
            [kigen.orbit :as orbit]
            [kigen.morphism :as morphism])
  (:gen-class))

(defn -main
  "just a main"
  [& args]
  (println "REPL me please!"))
