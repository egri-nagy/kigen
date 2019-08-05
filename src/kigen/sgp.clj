(ns kigen.sgp
  "General functions for semigroups. Black box style, the element(s)
  and the operation need to be supplied."
  (:require [clojure.math.combinatorics :refer [selections]]
            [orbit.core :refer [full-orbit]]
            [kigen.action :refer [right-action right-actions set-action]]
            [kigen.memory-info :refer [mem-info]]
            [taoensso.timbre :refer [info]]))

(declare sgp-by-gens
         commutative?
         k-nilpotent?
         index-period)

(defrecord Sgp [gens mul]) ;;another field is elts

(defn sgp-by-gens
  "Computes all elements of a semigroup generated by gens combined by the
  given binary operation mul."
  [gens mul]
  (let [sgp (full-orbit gens
                        (set-action (right-actions mul gens)))]
    (info "Semigroup of size " (count sgp) " " (mem-info))
    sgp))

(defn commutative?
  "Brute-force (but lazy) checking of commutativity of a semigroup."
  [sgp mul]
  (every? (fn [[x y]] (= (mul x y) (mul y x)))
          (selections sgp 2)))

(defn k-nilpotent?
  "Checking for k-nilpotency."
  [k sgp mul]
  (let [zero (reduce mul (repeat k (first sgp)))]
    (every? (fn [l]
              (= zero (reduce mul l)))
            (selections sgp k))))

(defn index-period
  "The index-period pair of integers in a vector for a given semigroup
  element x with multiplication mul."
  [x mul]
  (let [xmul (right-action mul x)]
    (loop [elt->idx {x 1} ;map semigroup elements to their indices
           k 2            ;the index of the next element
           nx (xmul x)]   ;the next element in orbit
      (if (contains? elt->idx nx)
        (let [index (elt->idx nx)
              period (- k index)]
          [index period])
        (recur (assoc elt->idx nx k)
               (inc k)
               (xmul nx))))))
