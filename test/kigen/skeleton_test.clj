(ns kigen.skeleton-test
  (:require [clojure.test :refer :all]
            [kigen.skeleton :as sk]
            [kigen.sgp :as sgp]
            [kigen.transf :as transf]
            [kigen.pbr :as pbr]))

(load-file "resources/sgpbestiary.clj")

(deftest test-skeleton
  (testing "Testing holonomy decomposition."
    (is (= 6 (sk/depth (sk/skeleton becks))))
    (is (= 7 (sk/depth (sk/skeleton (transf/full-ts-gens 7)))))))
