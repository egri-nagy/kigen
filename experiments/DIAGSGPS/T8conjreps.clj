;; A001372

(defn nxt [v m]
  (let [[ms others] (split-with #(= % m) v)]
    (vec (concat
          (repeat (count ms) 0)
          (if (empty? others)
            ()
            (concat
             [(inc (first others))]
             (rest others)))))))


(defn zerovec [m] (vec (repeat (inc m) 0)))
(defn maxcnj [m] (vec (concat (range 1 (inc m)) [0])))

(defn g [m] (conj (distinct
                   (map
                    (comp t/conjrep reverse)
                    (take-while #(not (= (vec  (reverse %)) (maxcnj m)))
                                (iterate #(nxt % m) (zerovec m)))))
                  (zerovec m)))

(spit "x" (vec (g 3)))
