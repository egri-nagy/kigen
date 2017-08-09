(ns kigen.transf-test
  (:require [clojure.test :refer :all]
            [kigen.transf :as t]
            [kigen.sgp :as sgp]
            [kigen.pbr :as pbr]
            [kigen.conjugacy :as conjugacy]))

(deftest transf-singleton-test
  (testing "Testing the singleton predicate."
    (is (= true (t/singleton? [:x])))
    (is (= false (t/singleton? [:x :y])))
    (is (= false (t/singleton? [])))
    (is (= false (t/singleton? nil)))))


(deftest transf-full-ts-test
  (testing "Testing full transformation semigroups."
    (let [pow #(reduce * 1 (repeat %2 %1))]
      (doseq [n [1 2 3 4]]
        (is (= (pow n n)
               (count (t/sgp-by-gens (t/full-ts-gens n)))))))))

(deftest transf-pts-test
  (testing "Testing partial transformation monoids."
    (let [pow #(reduce * 1 (repeat %2 %1))]
      (doseq [n [1 2 3 4]]
        (is (= (pow (inc n) n)
               (count (t/sgp-by-gens (t/pts-gens n)))))))))


(deftest transf-symmetric-group-test
  (testing "Testing symmetric groups."
    (let [factorial #(reduce * 1 (range 1 (inc %)))]
      (doseq [n [1 2 3 4 5 6]]
        (is (= (factorial n)
               (count (t/sgp-by-gens (t/symmetric-gens n)))))))))

(deftest transf-symmetric-inverse-monoid-test
  (testing "Testing symmetric inverse monoids."
    (is (= [2 7 34 209 1546]
           (map #(count (t/sgp-by-gens (t/sym-inv-gens %)))
                [1 2 3 4 5])))))

(deftest transf-inverse-test
  (testing "Inverses of bijective transformations in S7."
    (let [S7 (t/sgp-by-gens (t/symmetric-gens 7))
          id7 (t/idmap 7)]
      (is (every? #(and
                    (= id7 (t/mul % (t/inverse %)))
                    (= id7 (t/mul (t/inverse %) %)))
                  S7)))))

;;CONJUGATION
(deftest relaize-a-mapping-test
  (testing "Realizing an individual map."
    (is (= {1 3, 2 4} (t/realize-a-mapping [1 2] [3 4] {})))
    (is (= nil (t/realize-a-mapping [1 2] [3 4] {1 2})))
    (is (= {1 2, 2 1} (t/realize-a-mapping [1 2] [2 1] {})))
    (is (= {1 1, 2 4} (t/realize-a-mapping [1 2] [1 4] {})))
    (is (= {1 3, 2 2} (t/realize-a-mapping [1 2] [3 2] {})))
    (is (= nil (t/realize-a-mapping [1 2] [3 2] {2 3})))
    ))



(deftest transf-conjugate-test
  (testing "Conjugates."
    (is (= [4 3 0 3 1] (t/conjugate [1 4 3 3 2] [2 0 1 3 4])))
    (is (= [4 3 0 3 1] (t/conjugate-by-definition [1 4 3 3 2] [2 0 1 3 4])))))

(deftest transf-conjrep-test
  (testing "Conjugacy class representatives, direct search vs. naive method."
    (let [S5 (t/sgp-by-gens (t/symmetric-gens 5))
          T5 (t/sgp-by-gens (t/full-ts-gens 5))]
      (is (every? #(and
                    (t/conjrep %)
                    (conjugacy/conjrep t/conjugate % S5))
                  T5)))))

(deftest transf-test-conjugators
  (testing "Minimal conjugators."
    (let [T4 (t/sgp-by-gens (t/full-ts-gens 4))
          S4 (t/sgp-by-gens (t/symmetric-gens 4))]
      (is (empty?
           (filter
            (fn [x] (not= (set (t/conjugators x (t/conjrep x)))
                          (set (second
                                (conjugacy/minconjugators
                                 t/conjugate
                                 x
                                 S4)))))
            T4))))))
