(ns kigen.holonomy.skeleton-test
  (:require [clojure.test :refer :all]
            [kigen.holonomy.skeleton :as sk]
            [kigen.diagram.transf :as transf]))

;from resources/sgpbestiary.clj
(def becks
  [[0,1,2,0,0,0], ;creates the image {0,1,2}
   [3,3,3,4,3,5], ;transposition in {3,4,5}
   [3,3,3,4,5,3], ;cycle of {3,4,5}
   [3,3,3,3,4,4], ;this and the nontrivial holonomy group of
                  ;{3,4,5} generate the images with cardinality 1
   [3,3,3,0,1,2], ;this maps {3,4,5} to {0,1,2}
   [1,2,0,3,3,3]]);makes H({0,1,2}) nontrivial

(deftest test-skeleton
  (testing "Testing holonomy decomposition."
    (is (= 6 (sk/depth (sk/skeleton becks))))
    (is (= 7 (sk/depth (sk/skeleton (transf/full-ts-gens 7)))))))
