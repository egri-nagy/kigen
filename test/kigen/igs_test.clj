(ns kigen.igs-test
  (:require [clojure.test :refer :all]
            [clojure.data.int-map :refer [dense-int-set]]
            [orbit.core :refer [full-orbit]]
            [kigen.semigroup.sgp :refer [sgp-by-gens]]
            [kigen.igs :as igs]
            [kigen.multab :as multab]
            [kigen.diagram.transf :as transf]))

(deftest num-of-igs-test
  (testing "Testing S3 for ."
    (let [mt3 (multab/multab (sgp-by-gens (transf/symmetric-gens 3) transf/mul)
                             transf/mul)
          mt4 (multab/multab (sgp-by-gens (transf/symmetric-gens 4) transf/mul)
                            transf/mul)]
      (is (= 16 (count (full-orbit
                        [(dense-int-set)]
                        (partial igs/min-extensions
                                 mt3
                                 (multab/elts mt3))))))
      (is (= 413 (count (full-orbit
                         [(dense-int-set)]
                         (partial igs/min-extensions
                                  mt4
                                  (multab/elts mt4)))))))))
