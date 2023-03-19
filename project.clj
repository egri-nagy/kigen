(defproject org.clojars.egri-nagy/kigen "23.03.19"
  :description "Computational semigroup theory software system shadowing several semigroup packages of the GAP computer algebra system."
  :url "https://github.com/egri-nagy/kigen"
  :license {:name "GNU GENERAL PUBLIC LICENSE"
            :url "http://www.gnu.org/copyleft/gpl.html"}
  :dependencies [[org.clojure/clojure "1.11.1"]
                 [org.clojure/math.combinatorics "0.2.0"]
                 [org.clojure/data.int-map "1.2.0"]
                 [orbit "22.04.15"]
                 [rolling-stones "1.0.3"]
                 [org.clojure/core.logic "1.0.1"]
                 [com.taoensso/timbre "6.1.0"]
                 [com.clojure-goes-fast/clj-memory-meter "0.2.2"]
                 [progrock "0.1.2"]
                 [com.bhauman/rebel-readline "0.1.4"]]
  :plugins [[lein-cloverage "1.2.4"]
            [lein-kibit "0.1.8"]
            [lein-ancient "0.7.0"]
            [lein-bikeshed "0.5.2"]
            [jonase/eastwood "1.3.0"]]
  :jvm-opts ["-Djdk.attach.allowAttachSelf"]
  :main ^:skip-aot kigen.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}}
  :aliases {
            "rebl" ["trampoline" "run" "-m" "rebel-readline.main"]})
