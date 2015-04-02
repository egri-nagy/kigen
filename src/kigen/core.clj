(ns kigen.core
  (:require [kigen.pbr :as pbr]) (:gen-class))

(defn -main
  "just a main"
  [& args]
  (println (pbr/rand-pbr 3 4)))

; another way, with keywords
(def kw  (keyword (str 12)))
(read-string (name kw))
