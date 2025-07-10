(defproject org.clojars.egri-nagy/kigen "25.07.11"
  :description "Computational semigroup theory software system shadowing several semigroup packages of the GAP computer algebra system."
  :url "https://github.com/egri-nagy/kigen"
  :license {:name "GNU GENERAL PUBLIC LICENSE"
            :url "http://www.gnu.org/copyleft/gpl.html"}
  :dependencies [[org.clojure/clojure "1.12.1"]
                 [org.clojure/math.combinatorics "0.3.0"]
                 [org.clojure/data.int-map "1.3.0"]
                 [orbit "25.06.03"]
                 [rolling-stones "1.0.3"]
                 [org.clojure/core.logic "1.1.0"]
                 [com.taoensso/timbre "6.7.1"]
                 [com.clojure-goes-fast/clj-memory-meter "0.4.0"]
                 [progrock "1.0.0"]
                 [com.bhauman/rebel-readline "0.1.5"]
                 [macroz/tangle "0.2.2"]]
  :plugins [
	    [lein-hiera "2.0.0"]
            [lein-cloverage "1.2.4"]
            [lein-kibit "0.1.11"]
            [lein-ancient "0.7.0"]
            [lein-bikeshed "0.5.2"]
            [jonase/eastwood "1.4.3"]
            [cider/cider-nrepl "0.57.0"]]
  :jvm-opts ["-Djdk.attach.allowAttachSelf"]
  :main ^:skip-aot kigen.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}}
  :aliases {
            "rebl" ["trampoline" "run" "-m" "rebel-readline.main"]})
