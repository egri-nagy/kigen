(defn x-chain [S distances]
  (let [mt (multab/multab S)
        subs (multab/subsgps mt)
        covrel (poset/cover-rel subs clojure.set/superset?)
        dists (distances #{} covrel)]
    (dists (set (range 0 (count S))))))

(defn shortest-chain [S]
  (x-chain S poset/min-distances))

(defn longest-chain [S]
  (x-chain S poset/max-distances))
