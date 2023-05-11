; trajectory based transducer construction
; this way we can naturally do partial automata, but it does not
; scale well due to the large number of lvars (total sum of length of
; all trajectories)

(require '[kigen.logic :as kl])
(require '[clojure.core.logic :as l])
(require '[clojure.core.logic.fd :as fd])
(require '[taoensso.timbre :refer [info debug set-min-level! *config* merge-config!]])
(require '[kigen.position :refer [index]])
(require '[clojure.pprint :refer [pprint]])
(require '[clojure.math.combinatorics :as combo])
(require '[kigen.transducer.common :refer :all])
(require '[kigen.transducer.from-trajectories :refer :all])

; levels: :warn, :info, :debug
(set-min-level! :info)
(defn simple-logger
  [m]
  (str (:min-level (:config m)) " "
       (:vargs m)))
(merge-config! {:output-fn simple-logger})









(def sl-3-3
  [["|__" :first]
   ["_|_" :second]
   ["__|" :third]])

(trajectories sl-3-3 (first (transducer sl-3-3 3)))

(def sl-3-3b
  [["|__" :first]
   ["_|_" :second]
   ["__|" :third]
   ["___" :none]])

(check sl-3-3b (first (transducer sl-3-3b 4)))