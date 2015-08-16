(ns kigen.multab
  (:use [kigen.pos :as pos]))

(defn multab
  "Returns the multiplication table of the elements xs by the function mul."
  [xs mul]
  (let [indices (pos/index (vec xs))]
    (vec (pmap
          (fn [x] (->> xs
                       (map #(mul % x))
                       (map #(pos/pos (partial = %) indices))
                       (vec)))
          xs))))

(defn content
  "content of subarray spanned by the elements"
  [mt elts]
  (set (for [i elts j elts] (nth (nth mt i) j))))

(defn closure
  "Returns the smallest closed subarray that contains the elements."
  [mt elts]
  )
