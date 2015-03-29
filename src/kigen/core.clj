(ns kigen.core
  (:require [kigen.pbr :as pbr]) (:gen-class))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!")
  (println {1 #{1 2}, 2 #{2}} )
  (println (pbr/cod-cod [] 0)))

; another way, with keywords
(def kw  (keyword (str 12)))
(read-string (name kw))
