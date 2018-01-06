(ns kigen.core
  (:require [kigen.pbr :as pbr]
            [kigen.transf :as t]
            [kigen.perm :as perm]
            [kigen.skeleton :as skeleton]
            [kigen.poset :as poset]
            [kigen.sgp :as sgp]
            [kigen.multab :as multab]
            [kigen.igs :as igs]
            [kigen.morphism :as morphism]
            [kigen.genmorph :as gmorph]
            [kigen.chain-sgp :as chain-sgp])
  (:gen-class))

;; to save compile time property into a runtime one
(defmacro get-version []
  (System/getProperty "kigen.version"))

(defn -main
  "The first argument is a name of a file containing Clojure source code.
  This main method evaluates the forms contained."
  [& args]
  (println "KIGEN Computational Semigroup Theory Software System Version"
           (get-version))
  (load-file (first args))
  (shutdown-agents))
