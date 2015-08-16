(ns kigen.multab
  (:use [clojure.set :only [difference union]]
        [kigen.pos :as pos]))

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

(defn extend-by
  [mt base exts]
  (let [u (union base exts)]
    (union (set (for [i exts j u] (nth (nth mt i) j)))
           (set (for [i base j exts] (nth (nth mt i) j))))))

(defn closure
  "Returns the smallest closed subarray that contains the elements."
  [mt elts] (loop [base (union (set elts) (content mt elts))
                   exts (difference base elts)]
              (if (empty? exts)
                base
                (let [nbase (union base exts (extend-by mt base exts))
                      nexts (difference nbase (union base exts) )]
                  (recur nbase nexts)))))