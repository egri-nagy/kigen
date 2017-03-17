(defproject kigen "17.03.17-DEV"
  :description "Computational semigroup theory software system shadowing several semigroup packages of the GAP computer algebra system."
  :url "https://github.com/egri-nagy/kigen"
  :license {:name "GNU GENERAL PUBLIC LICENSE"
            :url "http://www.gnu.org/copyleft/gpl.html"}
  :dependencies [[org.clojure/clojure "1.8.0"]
                 [org.clojure/math.combinatorics "0.1.4"]
                 [slamhound "1.5.5"]]
  :plugins [[lein-cloverage "1.0.6"]
            [lein-kibit "0.1.2"]
            [lein-ancient "0.6.10"]
            [lein-bikeshed "0.3.0"]
            [jonase/eastwood "0.2.3"]]
  :main ^:skip-aot kigen.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}}
  :aliases {"slamhound" ["run" "-m" "slam.hound"]})
