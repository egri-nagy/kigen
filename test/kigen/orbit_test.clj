(ns kigen.orbit-test
  (:require [clojure.test :refer :all]
            [kigen.orbit :as orbit]
            [kigen.pbr :as pbr]))


;;a set-valued operator
(defn subset-covers [coll]
  (map #(set (remove (partial = %) coll)) coll))

(deftest test-sdfs
  (testing "Testing depth-first search with set-valued operator."
    (is (= 32 (count (orbit/sdfs [#{1 2 3 4 5}] subset-covers))))))
