(defproject kigen "19.08.05"
  :description "Computational semigroup theory software system shadowing several semigroup packages of the GAP computer algebra system."
  :url "https://github.com/egri-nagy/kigen"
  :license {:name "GNU GENERAL PUBLIC LICENSE"
            :url "http://www.gnu.org/copyleft/gpl.html"}
  :dependencies [[org.clojure/clojure "1.10.3"]
                 [org.clojure/math.combinatorics "0.1.6"]
                 [org.clojure/data.int-map "1.0.0"]
                 [orbit "19.08.05"]
                 [rolling-stones "1.0.2"]
                 [org.clojure/core.logic "1.0.1"]
                 [com.taoensso/timbre "5.1.2"]
                 [slamhound "1.5.5"]
                 [com.bhauman/rebel-readline "0.1.4"]]
  :plugins [[lein-cloverage "1.2.2"]
            [lein-kibit "0.1.8"]
            [lein-ancient "0.7.0"]
            [lein-bikeshed "0.5.2"]
            [jonase/eastwood "1.2.3"]]
  :main ^:skip-aot kigen.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}}
  :aliases {"slamhound" ["run" "-m" "slam.hound"]
            "rebl" ["trampoline" "run" "-m" "rebel-readline.main"]})
