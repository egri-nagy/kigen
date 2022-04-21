;; Subsemigroup enumeration up to conjugation.

(require '[kigen.transf :as t])
(require '[kigen.transf-conj :as t-c])
(require '[kigen.multab :as mt])
(require '[kigen.conjugacy :as conjugacy])

(require '[orbit.core :as orb])

(require '[clojure.data.int-map :as i])
(require '[clojure.set :refer [map-invert]])

(defn min-extensions-up-to-conjugacy
  "Simply doing the enumeration, then collect representatives only."
  [mt elts closedsub conj_rep_func]
  (let [raw (mt/min-extensions mt elts closedsub)]
    (distinct (map conj_rep_func raw))))

(defn subsgps-up-to-conjugacy
  "Conjugacy is done natively for sets of transformation, converting back and forth
  to int-maps."
  [S]
  (let [vS (vec (sort S))
        mtS (mt/multab vS t/mul)
        t2i (map-invert (zipmap (range (count vS)) vS))
        crf (fn [sub]
              (into (i/int-set)
                    (map t2i
                         (t-c/setconjrep (map vS sub)))))
        elts (mt/elts mtS)]
    (orb/full-orbit [(i/int-set)]
                    (fn [sub]
                      (min-extensions-up-to-conjugacy mtS
                                                      elts
                                                      sub
                                                      crf)))))

(defn subsgps-up-to-conjugacy2 [S G]
  (let
      [vS (vec (sort S))
       n (count vS)
       indices (vec (range n))
       t2i (map-invert (zipmap (range n) vS))
       ;; turning conjugation into simple action
       Ghom (fn [p] (mapv t2i (map #(t/conjugate % p) vS)))
       H (map Ghom G)
       cf (fn [x p] (p x))
       ;; mapping indices to the index of the conjugacy rep
       conjreps (zipmap indices
                        (map (fn [x] (t2i (t-c/conjrep (vS x))))
                             indices))
       ;; index to its minimal conjugators
       minconjs (zipmap indices
                        (map (fn [x] (set
                                      (second
                                       (conjugacy/minconjugators cf x H))))
                             indices))
       ;; set-wise conjugacy class rep function for subsets of indices
       crf (fn [sub]
             (let [conjugators (reduce
                                (fn [[m mcjs :as r] x]
                                  (let [xrep (conjreps x)
                                        flag (compare xrep m)]
                                    (cond (neg? flag) [xrep (minconjs x)]
                                          (zero? flag) [m (into mcjs (minconjs x))]
                                          :else r)))
                                [(inc n) #{}]
                                sub)]
               (i/int-set (conjugacy/setconjrep cf (seq sub) (second conjugators)))))
       mtS (mt/multab vS t/mul)]
    (orb/full-orbit [(i/int-set)]
                    (fn [sub]
                      (min-extensions-up-to-conjugacy mtS
                                                      indices
                                                      sub
                                                      crf)))))

(def S3 (t/sgp-by-gens (t/symmetric-gens 3)))
(def T3 (t/sgp-by-gens (t/full-ts-gens 3)))

(time (def T3subs (subsgps-up-to-conjugacy T3)))
(time (def T3subs2 (subsgps-up-to-conjugacy2 T3 S3)))
(println (count T3subs) " vs " (count T3subs2))


