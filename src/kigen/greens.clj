(ns kigen.greens
  "Black box algorithms to compute Green's relations."
  (:require [kigen.action :refer [right-action]]))

(defn principal-right-ideal
  "Computes the principal right ideal aS1 of a in semigroup S with
   multiplication mul."
  [a S mul]
  (let [aS (set  (map (partial mul a) S))]
    (conj aS a)))

(defn principal-left-ideal
  "Computes the principal left ideal S1a of a in semigroup S with
   multiplication mul."
  [a S mul]
  (let [Sa (set  (map (right-action mul a) S))]
    (conj Sa a)))

(defn principal-ideal
  "Computes the principal ideal of S1aS1 of a in S with multiplication mul."
  [a S mul]
  (let [aS1 (principal-right-ideal a S mul)]
    (into aS1 (for [x  S
                    y aS1]
                (mul x y)))))


(defn D-classes
  "Computes the D-classes of a finite semigroup."
  [S mul]
  (vals
   (group-by (fn [a]
               (principal-ideal a S mul))
             S)))

(defn R-classes
  "Computes the R-classes of a finite semigroup."
  [S mul]
  (vals (group-by (fn [a]
                    (principal-right-ideal a S mul))
                  S)))

(defn L-classes
  "Computes the D-classes of a finite semigroup."
  [S mul]
  (vals (group-by (fn [a]
                    (principal-left-ideal a S mul))
                  S)))