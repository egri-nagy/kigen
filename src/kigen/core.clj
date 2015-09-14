(ns kigen.core
  (:require [kigen.pbr :as pbr]) (:gen-class)
  (:require [kigen.orbit :as orbit]) (:gen-class)
  (:require [kigen.sgp :as sgp]) (:gen-class)
  (:require [kigen.perm :as perm]) (:gen-class)
  (:require [kigen.transf :as transf]) (:gen-class)
  (:require [kigen.poset :as poset]) (:gen-class)
  (:require [kigen.holonomy :as holonomy]) (:gen-class)
  (:require [kigen.multab :as multab]) (:gen-class)
  (:require [kigen.igs :as igs]) (:gen-class))

(defn -main
  "just a main"
  [& args]
  (println (pbr/rand-pbr 3 4)))
