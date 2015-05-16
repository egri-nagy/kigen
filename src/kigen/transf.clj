(ns kigen.transf
  (:require [kigen.pbr :as pbr]))

;;acting as pbr, then shift back the resulting set
(defn act
  [points t]
  (set (map #(- % (count (:dom t))) (pbr/act points t))))
