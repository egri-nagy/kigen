(def mtT3 (multab/multab T3 pbr/mul))

(def T3subs (multab/min-extensions mtT3 #{}))

(def T3subs (orbit/dfs [#{}] (partial multab/min-extensions mtT3)))

(def T3sr (poset/cover-rel T3subs clojure.set/superset?))

(def result (poset/max-distances #{} T3sr))

(result (set (range 0 27)))
