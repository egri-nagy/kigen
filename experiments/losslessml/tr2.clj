(require '[clojure.math.combinatorics :as combo])
(require '[kigen.transducer :refer :all])
(require '[taoensso.timbre :as timbre])
(require '[tangle.core :as tangle])
(require '[clojure.java.io :refer [copy file]])

(require '[clojure.core.logic :as l])
(require '[clojure.core.logic.fd :as fd])

(l/defne geto
  "Succeeds if the map m has key k with value v."
  [m k v]
  ([() _ v] (l/== v nil))
  ([[[K . V] . tail] k v]
   (l/conde [(l/== v V) (l/== k K)]
            [(l/!= k K) (geto tail k v)])))

;; this does not work with good-keysvalso TODO: Why?
;; (defn geto
;;   [m k v] 
;;   (l/fresh [K V]
;;            (l/== k K)
;;            (l/== v V)
;;            (l/membero [K V] m)))

(l/defne good-keysvalso
  [m kdom vdom]
  ([() _ _] l/succeed)
  ([[[K . V] . tail] kdom vdom]
   (fd/in K kdom)
   (fd/in V vdom)
   (good-keysvalso tail kdom vdom)))

(defn nonmembero2
  [x l]
  (l/fresh [h t]
           (l/conde
            [(l/== l ())]
            [(l/conso h t l)
             (l/!= x h)
             (nonmembero2 x t)])))

(l/defne good-mapo
  [m banned]
  ([() _] l/succeed)
  ([[[k . _] . tail] banned]
   (l/nafc l/membero k banned)
   ;(nonmembero2 k banned)
   (l/fresh [newbanned]
            (l/appendo k banned newbanned)
            (good-mapo tail newbanned))))

(let [kdom (fd/domain 1 2)
      vdom (fd/domain 3 4)]
  (l/run 3 [q]
         (geto q 1 4)
         (good-keysvalso q kdom vdom)
         (good-mapo q [])
         ))

(def interval (vec (range 1 4)))
(l/run* [a b c]
      (l/membero a interval)
      (l/membero b interval)
      (l/membero c interval)
      (l/project [a b c]
               (l/== 2 (count (set (list a b c))))))