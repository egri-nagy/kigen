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

;just for experimenting with
(def T2 (multab/multab (sgp/sgp-by-gens (transf/full-ts-gens 2))))
(def T3 (multab/multab (sgp/sgp-by-gens (transf/full-ts-gens 3))))
(def S2 (multab/multab (sgp/sgp-by-gens (transf/symmetric-gens 2))))
(def S3 (multab/multab (sgp/sgp-by-gens (transf/symmetric-gens 3))))

(defn -main
  "just a main"
  [& args]
  (println "REPL me please!"))
