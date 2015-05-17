(ns kigen.pbr-test
  (:require [clojure.test :refer :all]
            [kigen.core :refer :all]))

(load-file "resources/arxiv1102.0862.clj")
(load-file "resources/roozbehtest.clj")
(deftest pbr-mul-test
  (testing "Testing pbr multiplication."
    (is (= alphabeta (kigen.pbr/mul alpha beta)))
    (is (= RHab (kigen.pbr/mul RHa RHb)))))
