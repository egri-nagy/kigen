(defproject kigen "19.08.05"
  :description "Computational semigroup theory software system shadowing several semigroup packages of the GAP computer algebra system."
  :url "https://github.com/egri-nagy/kigen"
  :license {:name "GNU GENERAL PUBLIC LICENSE"
            :url "http://www.gnu.org/copyleft/gpl.html"}
  :dependencies [[org.clojure/clojure "1.10.1"]
                 [org.clojure/math.combinatorics "0.1.6"]
                 [org.clojure/data.int-map "0.2.4"]
                 [orbit "19.08.05"]
                 [rolling-stones "1.0.1"]
                 [org.clojure/core.logic "0.8.11"]
                 [com.taoensso/timbre "4.10.0"]
                 [slamhound "1.5.5"]
                 [com.bhauman/rebel-readline "0.1.4"]]
  :plugins [[lein-cloverage "1.1.1"]
            [lein-kibit "0.1.6"]
            [lein-ancient "0.6.15"]
            [lein-bikeshed "0.5.2"]
            [jonase/eastwood "0.3.6"]]
  :main ^:skip-aot kigen.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}}
  :aliases {"slamhound" ["run" "-m" "slam.hound"]
            "rebl" ["trampoline" "run" "-m" "rebel-readline.main"]})
