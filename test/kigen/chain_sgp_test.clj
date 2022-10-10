(ns kigen.chain-sgp-test
  (:require [clojure.test :refer [deftest testing is]]
            [kigen.transf :as t]
            [kigen.chain-sgp :refer [check-morphism]]))

(load-file "resources/sgpbestiary.clj")

(deftest chains-test
  (testing "Testing chains in cover relation."
    (is (check-morphism (t/full-ts-gens 3)))
    (is (check-morphism bex))
    (is (check-morphism hey-bug))
    (is (check-morphism overlapping-covers))))
