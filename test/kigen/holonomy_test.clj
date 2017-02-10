(ns kigen.holonomy-test
  (:require [clojure.test :refer :all]
            [kigen.holonomy :as h]
            [kigen.sgp :as sgp]
            [kigen.transf :as transf]
            [kigen.pbr :as pbr]))

(load-file "resources/sgpbestiary.clj")

(deftest test-holonomy
  (testing "Testing holonomy decomposition."
    ;(is (= 6 (h/depth (h/skeleton becks))))
                                        ;(is (= 7 (h/depth (h/skeleton (transf/full-ts-gens 7)))))
    )
    )
