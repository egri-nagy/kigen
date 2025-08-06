(ns kigen.canonical-labeling-test
  (:require [clojure.test :refer [deftest testing is]]
            [kigen.canonical-labeling :refer :all]))

(deftest can-seq-test
  (testing "Testing canonical labeling and from for sequences."
    ;can-lab- gives labeling info, but not the form
    (is (= (can-lab-seq [:a 6 7 :a "hello"])
           [{:a 0, 6 1, 7 2, "hello" 3} 4]))
    ;can- gives the form
    (is (= (can-seq [:a 6 7 :a "hello"])
           [0 1 2 0 3]))
    ;both can-lab- and can- can start from a partial labeling
    (is (= (can-lab-seq [{:a 10} 11] [:a 6 7 :a "hello"])
           [{:a 10, 6 11, 7 12, "hello" 13} 14]))
    (is (= (can-seq [{:a 3} 4] [:a 6 7 :a "hello"])
           [3 4 5 3 6]))))
