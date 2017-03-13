(ns kigen.genmorph
  "Constructing morphisms by generators."
  (:require [clojure.math.combinatorics :refer [subsets
                                                partitions
                                                combinations
                                                cartesian-product]]
            [kigen.sgp :as sgp]
            [kigen.transf :as transf]
            [kigen.gentab :refer [gentab]]
            [kigen.conjugacy :refer [conjrep-general]]))

(declare extend-morph add-edge extend-node add-gen-and-close embeddings-conj)

(defn minconjugators
  "Finds the minimal conjugate transformation of t under permutations G.
  Also returns the subset of G that takes t to the rep."
  [t G]
  (let [conjugations (map
                      (fn [p] [(transf/conjugate t p) p])
                      G)
        mint (first (sort (map first conjugations)))]
    [mint, (map second (filter #(= mint (first %)) conjugations))]))

(defn conj-conj
  "A conjugated list is a sequence of transformation and a set of permutations.
  The list is extended by a new transformation, conjugated to make it minimal
  in the conjugacy class. Set of possible conjugators reduced accordingly."
  [[L G] t]
  (let [[mint nG] (minconjugators t G)]
    [(conj L mint) nG]))

(def transf-conjrep (partial conjrep-general transf/conjugate))

(def transf-set-conjrep
  (partial conjrep-general (fn [X p]
                             (vec
                              (sort
                               (map (fn [x] (transf/conjugate x p)) X))))))

(defn transf-seq-conjrep
  "The conjugacy class representative of a sequence of transformations L under
  permutations G."
  [L G]
  (first (reduce conj-conj [[] G] L)))


(defn targets
  [srcgens srcmul trgens trgmul]
  (let [T (sgp/sgp-by-gens trgens trgmul)
        genips (map #(sgp/index-period % srcmul) srcgens)
        genipset (set genips)
        m (zipmap genipset (repeat []))
        nm (reduce (fn [m t]
                     (let [ip (sgp/index-period t trgmul)]
                       (if (contains? genipset ip)
                         (update m ip (fn [v x] (conj v x)) t)
                         m)))
                   m
                   T)]
    (map nm genips)))

(defn embeddings
  "All morphisms from embedding seeds, but lossy ones filtered out."
  [Sgens Smul Tgens Tmul]
  (let [[mSgens mSmul] (let [src (gentab Sgens Smul)] [(:gens src) (:mul src)])
        tgs (targets Sgens Smul Tgens Tmul)]
    (loop [n 0, tgs tgs, morphs [{}]]
      (println (count morphs))
      (if (empty? tgs)
        morphs
        (let [nmorphs (mapcat #(pmap (fn [m]
                                      (add-gen-and-close m
                                                         n
                                                         %
                                                         (take (inc n) mSgens)
                                                         mSmul
                                                         Tmul))
                                    morphs)
                              (first tgs))]
          (recur (inc n) (rest tgs) (filter #(apply distinct? (vals %))
                                            (remove number? nmorphs))))))))
(defn sgp-embeddings-by-gens
  "Prepares the semigroups for calling embeddings-conj. Gentabbing and finding
  conj candidates."
  [Sgens Smul Tgens Tmul G f]
  (let [[mSgens mSmul] (let [src (gentab Sgens Smul)] [(:gens src) (:mul src)])
        ts (targets Sgens Smul Tgens Tmul)
        tgs (cons (pmap #(transf-conjrep % G) (first ts)) (rest ts))]
    (map (fn [m] (zipmap Sgens (map m mSgens)))
     (f mSgens mSmul Tgens Tmul tgs G))))

(defn embeddings-conj
  "All morphisms from embedding seeds, but lossy ones filtered out."
  [Sgens Smul Tgens Tmul tgs G]
  (println (count (first tgs)) " candidate(s) for 1st generator")
  (loop [n 0, morphconjpairs [ [ {}, [[] G] ] ]]
    (if (= n (count Sgens))
      (map first morphconjpairs)
      (let [ngens (nth tgs n)
            maps (pmap
                  (fn [[phi cL]]
                    (let [ncongs (map #(conj-conj cL %) ngens)
                          f (fn [[umcprs imgs] cng]
                              (let [nmorph (add-gen-and-close
                                            phi
                                            (nth Sgens n)
                                            (last (first cng))
                                            (take (inc n) Sgens)
                                            Smul
                                            Tmul)]
                                (if (and (coll? nmorph)
                                         (apply distinct? (vals nmorph)))
                                  (let [img (transf-set-conjrep
                                             (vec (sort (vals nmorph)))
                                        ;(map nmorph (take (inc n) mSgens)) ;faster but leaves duplicates 
                                             G)]
                                    (if (contains? imgs img)
                                      [umcprs imgs]
                                      [(conj umcprs [img  [nmorph cng]])
                                       (conj imgs img)]))
                                  [umcprs imgs])))]
                      (first (reduce f [{} #{}] ncongs))))
                  morphconjpairs)
            nmcprs (vals (apply merge maps))]
        (println (inc n) (count nmcprs) "morph(s)")
        (recur (inc n) nmcprs)))))

(defn embeddings-conj2
  "All morphisms from embedding seeds, but lossy ones filtered out."
  [Sgens Smul Tgens Tmul tgs G]
  (println (count (first tgs)) " candidate(s) for 1st generator")
  (loop [n 0, morphs [{}] ]
    (if (= n (count Sgens))
      morphs
      (letfn [(f [phi]
                (first
                 (let [currentgens(if (empty? phi)
                                    []
                                    (mapv phi
                                          (take n Sgens)))
                       partconj (if (empty? currentgens) [[] G]
                                    (reduce conj-conj [[] G] currentgens))
                       ngens (distinct
                              (map (comp last first)
                                   (map #(conj-conj partconj %) (nth tgs n))))
                       f (fn [[nmorphs imgs] ngen]
                           (let [nmorph (add-gen-and-close
                                         phi
                                         (nth Sgens n)
                                         ngen
                                         (take (inc n) Sgens)
                                         Smul
                                         Tmul)]
                             (if (and (coll? nmorph)
                                      (apply distinct? (vals nmorph)))
                               (let [img (transf-set-conjrep
                                          (vec (sort (vals nmorph)))
                                          G)]
                                 (if (contains? imgs img)
                                   [nmorphs imgs]
                                   [(conj nmorphs nmorph)
                                    (conj imgs img)]))
                               [nmorphs imgs])))]
                   (reduce f [[] #{}] ngens))))]
        (let [nmorphs (apply concat (pmap f morphs))]
          (println "gens:" (inc n) "morphs:" (count nmorphs))
          (recur (inc n) nmorphs))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Cayley graph morph matching - next 3 functions are nested, top to bottom ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn extend-morph
  "Extends partial morphism systematically by the generators starting at the
  frontline. If morphism is not possible, returns the number of matchings."
  [phi front Sgens Smul Tmul]
  (loop [phi phi, stack (vec front)]
    (if (empty? stack)
      phi
      (let [result (extend-node phi (peek stack) Sgens Smul Tmul)]
        (if (number? result)
          result
          (recur (:phi result) (into (pop stack) (:new result))))))))

;; all nodes 1 new generator
(defn add-gen
  " "
  [phi gen phiofgen Smul Tmul]
  (loop [phi (conj phi [gen phiofgen]), incoming [gen], front (conj  (keys phi) gen)]
    (if (empty? front)
      {:phi phi :new incoming}
      (let [p (add-edge phi (first front) gen Smul Tmul)]
        (cond (number? p) p
              (empty? p) (recur phi
                                incoming
                                (rest front))
              :else (recur (conj phi p)
                           (conj incoming (first p))
                           (rest front)))))))

(defn add-gen-and-close
  " "
  [phi gen phiofgen Sgens Smul Tmul]
  (let [res (add-gen phi gen phiofgen Smul Tmul)]
    (if (number? res)
      res
      (extend-morph (:phi res) (:new res) Sgens Smul Tmul))))

;; 1-node all generators
(defn extend-node
  "Extending a single element by all generators one-by-one, so breach of
  morphism gets detected immediately.
  Returns the updated morphism phi and the newly added nodes."
  [phi a gens Smul Tmul]
  (loop [phi phi, incoming [], gens gens]
    (if (empty? gens)
      {:phi phi :new incoming}
      (let [p (add-edge phi a (first gens) Smul Tmul)]
        (cond (number? p) p
              (empty? p) (recur phi incoming (rest gens))
              :else (recur (conj phi p)
                           (conj incoming (first p))
                           (rest gens)))))))
;; 1-node 1-generator
(defn add-edge
  "Extends the morphism phi by applying a generator b to a single element a.
  This is where the homomorphism condition is checked.
  phi - morphism represented as a map
  a - an elementof S to be extended, already in phi
  b - a generator of S, already in phi
  It returns the number of matched elements so far if it is not homomorphic,
  [] when homomorphic but no new element generated,
  [ab AB] where AB is the newly assigned image of the product ab."
  [phi a b mulS mulT]
  (let [ab (mulS a b)
        AB (mulT (phi a) (phi b))]
    (if (contains? phi ab)
      (if (= AB (phi ab))
        []
        (count phi))
      [ab AB])))

;; shadow
;; sorting with duplicates removed, like UNIX's sort -u
(def sort-u (comp vec sort set))

(defn conjclass
  [L G]
  (sort-u (map #(mapv (fn [x] (transf/conjugate x %)) L) G)))

(defn conjrep-shadow [l G] (first (conjclass l G)))
