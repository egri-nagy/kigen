(ns kigen.diagram.brauer-test
  (:require
   [clojure.test :refer [deftest is]]
   [kigen.diagram.brauer :as br]
   [kigen.sgp :as sgp]))

;;https://oeis.org/A001147
(deftest brauer-monoid-size
  (is (= [1 3 15 105 945 10395]
         (map (fn [n]
                (count (sgp/sgp-by-gens (br/brauer-gens n) br/mul)))
              [1 2 3 4 5 6]))))