(ns kigen.pbr-test
  (:require [clojure.test :refer :all]
            [kigen.pbr :as pbr]))

(load-file "resources/arxiv1102.0862.clj")
(load-file "resources/roozbehtest.clj")
(deftest pbr-mul-test
  (testing "Testing pbr multiplication."
    (is (= alphabeta (pbr/mul alpha beta)))
    (is (= RHab (pbr/mul RHa RHb)))))
