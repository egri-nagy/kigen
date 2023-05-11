(ns kigen.transducer-from-trajectories-test
  (:require [clojure.test :refer :all]
            [clojure.core.logic :as l]
            [kigen.transducer.common :refer [trajectories check]]
            [kigen.transducer.from-trajectories :as ft]))

(deftest small-fixed-transducer-test
  (testing "Testing an example with non-partial output."
    (let [io-pairs
          [["aaba" :foo]
           ["bb" :bar]
           ["bababc" :foobar]
           ["bba" :foo]
           ["c" :bar]
           ["cac" :foo]
           ["ccaabb" :foobar]]
          solution (first (ft/transducer io-pairs 3))]
      (is
       (check io-pairs solution))
      (is (= '("0 ·a 1 ·a 2 ·b 0 ·a 1 = :foo ✔"
               "0 ·b 2 ·b 0 = :bar ✔"
               "0 ·b 2 ·a 2 ·b 0 ·a 1 ·b 2 ·c 2 = :foobar ✔"
               "0 ·b 2 ·b 0 ·a 1 = :foo ✔"
               "0 ·c 0 = :bar ✔"
               "0 ·c 0 ·a 1 ·c 1 = :foo ✔"
               "0 ·c 0 ·c 0 ·a 1 ·a 2 ·b 0 ·b 2 = :foobar ✔")
             (trajectories io-pairs solution))))))

(deftest compatiblo-test
  (testing "Relation compatiblo."
    (is (= (l/run*
            [q]
            (ft/compatiblo [0 0] [1 0])
            (ft/compatiblo [0 0] [1 1]))
           '(_0)))
    (is (=
         (l/run* [q] (ft/compatiblo [0 0] [0 1]))
         (l/run* [q] (ft/compatiblo [2 0] [2 1]))
         ()))
    (is (= (l/run* [q]
                   (l/membero q [0 1 2])
                   (ft/compatiblo [1 2] [q 2]))
           '(0 1 2)))
    (is (= (l/run* [q]
                   (l/membero q [0 1 2])
                   (ft/compatiblo [1 2] [1 q]))
           '(2)))))