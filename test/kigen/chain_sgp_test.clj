(ns kigen.chain-sgp-test
  (:require [clojure.test :refer [deftest testing is]]
            [kigen.diagram.transf :as t]
            [kigen.chain-sgp :refer [check-morphism]]))

;from resources/sgpbestiary.clj
(def bex [[0 1 0 0] [3 3 3 2] [2 2 3 3] [3 3 0 1] [1 0 3 3]])
;; overlapping covers for parallel components
(def overlapping-covers [[3 2 3 1] [0 1 1 0] [2 1 0 0]])
(def hey-bug [[1 1 3 3 5 5 7 7] [0 1 2 4 4 5 6 4] [0 1 2 3 2 3 2 3]])

(deftest chains-test
  (testing "Testing chains in cover relation."
    (is (check-morphism (t/full-ts-gens 3)))
    (is (check-morphism bex))
    (is (check-morphism hey-bug))
    (is (check-morphism overlapping-covers))))
