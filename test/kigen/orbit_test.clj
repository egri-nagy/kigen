(ns kigen.orbit-test
  (:require [clojure.test :refer :all]
            [kigen.orbit :as orbit]
            [kigen.pbr :as pbr]))

;;testing with operators that construct subsets of a set
;;a set-valued operator
(defn subset-covers [coll]
  (map #(set (remove (partial = %) coll)) coll))

(deftest test-*fs
  (let [bulkres (orbit/full-orbit-bulk [#{1 2 3 4 5 6 7 8}] subset-covers)
        singleres (orbit/full-orbit-single [#{1 2 3 4 5 6 7 8}] subset-covers)]
    (testing "Testing single and bulk extension searches."
      (is (= 256 (count bulkres)))
      (is (= bulkres singleres)))))

(deftest test-first-solution-*fs
  (let [f (fn [x] (hash-set (conj x (inc (count x)))))
        cand? (fn [x] (<= (count x) 11))
        sol? (fn [x] (= (count x) 11))
        bulksol (orbit/first-solution-bulk #{0} f cand? sol?)
        singlesol (orbit/first-solution-single #{0} f cand? sol?)
        ]
    (testing "Testing bulk and single extension first solutions."
      (is (not (some nil? [bulksol singlesol]))))))
