(defproject kigen "0.1.0-SNAPSHOT"
  :description "Computing with diagram semigroups based on partitioned binary relations"
  :url "https://github.com/egri-nagy/kigen"
  :license {:name "GNU GENERAL PUBLIC LICENSE"
            :url "http://www.gnu.org/copyleft/gpl.html"}
  :dependencies [[org.clojure/clojure "1.8.0"]
                 [org.clojure/math.combinatorics "0.1.3"]]
  :plugins [[lein-cloverage "1.0.6"]]
  :main ^:skip-aot kigen.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})
