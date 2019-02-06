(defproject kigen "18.05.10"
  :description "Computational semigroup theory software system shadowing several semigroup packages of the GAP computer algebra system."
  :url "https://github.com/egri-nagy/kigen"
  :license {:name "GNU GENERAL PUBLIC LICENSE"
            :url "http://www.gnu.org/copyleft/gpl.html"}
  :dependencies [[org.clojure/clojure "1.10.0"]
                 [org.clojure/math.combinatorics "0.1.4"]
                 [org.clojure/data.int-map "0.2.4"]
                 [orbit "18.06.14"]
                 [rolling-stones "1.0.0-SNAPSHOT"]
                 [org.clojure/core.logic "0.8.11"]
                 [com.taoensso/timbre "4.10.0"]
                 [slamhound "1.5.5"]
                 [com.bhauman/rebel-readline "0.1.4"]]
  :plugins [[lein-cloverage "1.0.6"]
            [lein-kibit "0.1.2"]
            [lein-ancient "0.6.14"]
            [lein-bikeshed "0.3.0"]
            [jonase/eastwood "0.2.3"]]
  :main ^:skip-aot kigen.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}}
  :aliases {"slamhound" ["run" "-m" "slam.hound"]
            "rebl" ["trampoline" "run" "-m" "rebel-readline.main"]})
