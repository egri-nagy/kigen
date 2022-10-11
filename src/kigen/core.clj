(ns kigen.core
  (:require [taoensso.timbre :as timbre])
  (:gen-class))

;; to save compile time property into a runtime one
(defmacro get-version []
  (System/getProperty "kigen.version"))

;; setting default log level
(timbre/merge-config! {:min-level :warn}) ;

(defn -main
  "The first argument is a name of a file containing Clojure source code.
  This main method evaluates the forms contained."
  [& args]
  (println "KIGEN v" (get-version)
           " Computational Semigroup Theory Software System")
  (load-file (first args))
  (shutdown-agents))
