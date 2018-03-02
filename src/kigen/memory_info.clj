(ns kigen.memory-info
  "Information about memory usage.")

(defn mem-info
  "Info string containing free and total memory (after gc)"
  []
  (let [rt (java.lang.Runtime/getRuntime)
        GB (* 1024 1024 1024)]
    (System/gc)
    (str
     (format "%.2f" (float (/ (- (.totalMemory rt) (.freeMemory rt)) GB)))
     "/"
     (format "%.2f" (float (/ (.totalMemory rt) GB)))
     "GB")))
