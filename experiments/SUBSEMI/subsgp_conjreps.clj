;; Rough version of subsemigroup enumeration up to conjugation.
;; Set-wise conjugacy representative is decided natively for transformations,
;; converted back and forth to int-maps.

(require '[kigen.transf :as t])
(require '[kigen.multab :as mt])
(require '[kigen.conjugacy :as conjugacy])

(require '[orbit.core :as orbit])

(require '[clojure.data.int-map :as i])

(defn min-extensions-up-to-conjugacy
  [mt elts closedsub conj_rep_func]
  (let [raw (mt/min-extensions mt elts closedsub)]
    (distinct (map conj_rep_func raw))))

(defn subsgps-up-to-conjugacy [S]
  (let [vS (vec (sort S))
        mtS (mt/multab vS t/mul)
        n (count vS)
        trans2indices (clojure.set/map-invert (zipmap (range n) vS))
        crf (fn [sub]
              (into (i/int-set)
                    (map trans2indices
                         (t/setconjrep (map vS sub)))))
        elts (mt/elts mtS)]
    (orbit/full-orbit-parallel [(i/int-set)]
                               (fn [sub] (min-extensions-up-to-conjugacy mtS
                                                                         elts
                                                                         sub
                                                                         crf)))))

(defn subsgps-up-to-conjugacy2 [S G]
  (let [vS (vec (sort S))
        mtS (mt/multab vS t/mul)
        n (count vS)
        t2i (clojure.set/map-invert (zipmap (range n) vS))
        Ghom (fn [p] (mapv t2i (map #(t/conjugate % p) vS)))
        syms (map Ghom G)
        cf (fn [x p] (p x))
        minconjs (zipmap (mt/elts mtS)
                         (map (fn [x] (conjugacy/minconjugators cf x G))
                              (mt/elts mtS)))
]
    minconjs))
