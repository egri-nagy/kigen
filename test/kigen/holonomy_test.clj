(ns kigen.holonomy-test
  (:require [clojure.test :refer :all]
            [kigen.holonomy :as h]
            [kigen.sgp :as sgp]
            [kigen.transf :as transf]
            [kigen.pbr :as pbr]))

(load-file "resources/sgpbestiary.clj")

(def BECKS (sgp/sgp-by-gens (map transf/transf->pbr becks) pbr/mul))

(deftest test-holonomy
  (testing "Testing holonomy decomposition."
    (is (= 6 (h/depth (h/skeleton BECKS))))
    (is (= 7 (h/depth (h/skeleton (transf/full-ts-gens 7)))))))
