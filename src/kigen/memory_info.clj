(ns kigen.memory-info
  "Information about memory usage.")

(defn mem-info
  "Info string containing free and total memory (after gc)"
  []
  (let [rt (java.lang.Runtime/getRuntime)
        GB (* 1024 1024 1024)]
    (System/gc)
    (let [totalmem (.totalMemory rt)
          freemem (.freeMemory rt)
          [unit divisor] (cond
                           (< totalmem (* 1024 1024)) ["KB" 1024]
                           (< totalmem (* 1024 1024 1024)) ["MB" (* 1024 1024)]
                           :else ["GB" (* 1024 1024 1024)])]
      (str
       "used: "
       (format "%.1f" (float (/ (- totalmem freemem) divisor))) unit
       " total: "
       (format "%.2f" (float (/ totalmem divisor))) unit
       " free: " (format "%.2f" (* 100 (float (/ freemem totalmem)))) "%"))))
