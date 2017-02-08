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
  "The first argument is a name of a file containing Clojure source code.
  This main method evaluates the forms contained."
  [& args]
  (load-file (first args))
  (shutdown-agents))
