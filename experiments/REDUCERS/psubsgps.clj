(use '[kigen.multab :as multab])
(use '[kigen.transf :as t])
(use '[taoensso.timbre :as timbre])



(def mtS
  (multab/multab (t/sgp-by-gens (t/pts-gens 3))
                 t/mul))

(timbre/merge-config! {:level :info})

(defn psubsgps-timing [mt task-size]
  (println "Task size: " task-size)
  (binding [orbit.extension/*task-size* task-size]
    (time (count (multab/psubsgps mt)))))

(defn subsgps-timing [mt]
  (println "Sequential")
  (time (count (multab/subsgps mt)))
  (println)))



(doseq [x [32 256 512 1024]]
  (psubsgps-timing mtS x))
(subsgps-timing mtS)
