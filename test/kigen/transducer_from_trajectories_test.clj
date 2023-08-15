(ns kigen.transducer-from-trajectories-test
  (:require [clojure.test :refer :all]
            [clojure.core.logic :as l]
            [clojure.core.logic.fd :as fd]
            [kigen.transducer.common :refer [trajectories check]]
            [kigen.transducer.from-trajectories :as ft]))

(deftest small-non-partial-transducer-test
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

(deftest small-partial-transducer-test
  (testing "Small examples with partial results."
    (let [sl-3-3 [["|__" :first]
                  ["_|_" :second]
                  ["__|" :third]]
          sl-3-3b
          [["|__" :first]
           ["_|_" :second]
           ["__|" :third]
           ["___" :none]]]
      (is (check sl-3-3 (first (ft/transducer sl-3-3 3))))
      (is (check sl-3-3b (first (ft/transducer sl-3-3b 4)))))))

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
           '(2)))
    (is (= (let [X (range 3)]
             (l/run* [q] (l/fresh [a b]
                                  (l/== q [a b])
                                  (l/membero a X)
                                  (l/membero b X)
                                  (ft/compatiblo [:x 0] q))))
           '([0 0] [0 1] [1 0] [0 2] [1 1] [2 0] [1 2] [2 1] [2 2])))
    (is (= (let [X (range 3)]
             (l/run* [q] (l/fresh [a b]
                                  (l/== q [a b])
                                  (l/membero a X)
                                  (l/membero b X)
                                  (ft/compatiblo [1 0] q))))
           '([0 0] [0 1] [1 0] [0 2] [2 0] [2 1] [2 2])))))

(deftest compatible-with-collo-test
  (testing "Compatible-with-collo"
    (is (=
         (l/run* [q] (ft/compatible-with-collo [0 0] [[0 0] [1 0] [1 1]]))
         (l/run* [q] (ft/compatible-with-collo [0 0] [[1 0] [1 1]]))
         '(_0)))
    (is (=
         (l/run* [q] (ft/compatible-with-collo [0 0] [[2 1] [1 0] [0 1]]))
         '()))))

(deftest compatible-collo-test
  (testing "Compatible-collo"
    (is (= (l/run* [q]
                   (l/== q [[(l/lvar) (l/lvar)] [(l/lvar) (l/lvar)]])
                   (ft/compatible-collo q))
           '([[_0 _1] [_0 _1]] ([[_0 _1] [_2 _3]] :- (!= (_0 _2))))))
    ;;todo, interpret this
    (is (= (l/run* [q]
                   (l/fresh [a b c d e f]
                            (l/== q [[a b] [c d] [e f]])
                            (ft/compatible-collo q)))
           '([[_0 _1] [_0 _1] [_0 _1]]
             ([[_0 _1] [_2 _3] [_0 _1]] :- (!= (_2 _0)) (!= (_0 _2)))
             ([[_0 _1] [_0 _1] [_2 _3]] :- (!= (_0 _2)))
             ([[_0 _1] [_2 _3] [_2 _3]] :- (!= (_0 _2)))
             ([[_0 _1] [_2 _3] [_4 _5]] :- (!= (_2 _4)) (!= (_0 _4)) (!= (_0 _2))))))
    (is (= 114
           (count (l/run* [q]
                          (l/fresh [a b c d e f]
                                   (l/everyg (fn [x] (fd/in x (fd/domain 1 2 3)))
                                             [a b c d e f])
                                   (l/== q [[a b] [c d] [e f]])
                                   (l/distincto q)
                                   (ft/compatible-collo q))))))))
