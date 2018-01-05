(ns kigen.igs-test
  (:require [clojure.test :refer :all]
            [clojure.data.int-map :refer [int-set]]
            [orbit.core :refer [full-orbit]]
            [kigen.igs :as igs]
            [kigen.multab :as multab]
            [kigen.transf :as transf]))

(deftest num-of-igs-test
  (testing "Testing S3 for ."
    (let [mt3 (multab/multab (transf/sgp-by-gens (transf/symmetric-gens 3))
                             transf/mul)
          mt4 (multab/multab (transf/sgp-by-gens (transf/symmetric-gens 4))
                            transf/mul)]
      (is (= 16 (count (full-orbit
                        [(int-set)]
                        (partial igs/min-extensions
                                 mt3
                                 (multab/elts mt3))))))
      (is (= 413 (count (full-orbit
                         [(int-set)]
                         (partial igs/min-extensions
                                  mt4
                                  (multab/elts mt4)))))))))
