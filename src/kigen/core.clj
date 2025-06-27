(ns kigen.core
  (:require [taoensso.timbre :refer [set-min-level! merge-config!]])
  (:gen-class))

;; to save compile time property into a runtime one
(defmacro get-version []
  (System/getProperty "kigen.version"))

;; setting default log level
; levels: :warn, :info, :debug
(set-min-level! :warn)
(defn simple-logger
  [m]
  (str (:min-level (:config m)) " "
       (:vargs m)))
(merge-config! {:output-fn simple-logger})

(defn -main
  "The first argument is a name of a file containing Clojure source code.
  This main method evaluates the forms contained."
  [& args]
  (println "; KIGEN" (get-version)
           "Clojure" (clojure-version)
           "Java" (System/getProperty "java.vm.version")
           (System/getProperty "os.name")
           (System/getProperty "os.version")
           (System/getProperty "os.arch"))
  (load-file (first args))
  (shutdown-agents))
