(ns kigen.chain-sgp-test
  (:require [clojure.test :refer :all]
            [kigen.chain-sgp :refer [check-morphism]]))

(load-file "resources/sgpbestiary.clj")

(deftest chains-test
  (testing "Testing chains in cover relation."
    (is (check-morphism alifex))
    (is (check-morphism overlapping-covers))))
