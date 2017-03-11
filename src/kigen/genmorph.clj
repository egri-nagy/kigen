(ns kigen.genmorph
  "Constructing morphisms by generators."
  (:require [clojure.math.combinatorics :refer [subsets
                                                partitions
                                                combinations
                                                cartesian-product]]
            [kigen.sgp :as sgp]
            [kigen.transf :as transf]))

(declare extend-morph add-edge extend-node add-gen-and-close)

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

(defn conjrep-general
  [thing symmetries conjugation-function]
  (letfn [(f [minimal-thing sym]
            (let [new-thing (conjugation-function thing sym)]
              (if (< (compare new-thing minimal-thing) 0)
                new-thing
                minimal-thing)))]
    (reduce f thing symmetries)))

(defn transf-conjrep [t G]
  (conjrep-general t G transf/conjugate))

(defn transf-set-conjrep
  [coll G]
  (conjrep-general (vec (sort coll))
                   G
                   (fn [X p]
                     (vec
                      (sort
                       (map (fn [x] (transf/conjugate x p)) X))))))

(defn transf-seq-conjrep
  "The conjugacy class representative of a sequence of transformations L under
  permutations G."
  [L G]
  (conjrep-general L
                   G
                   (fn [L] (first
                            (reduce conj-conj [[] G] L)))))

(defn source
  "data items of source semigroup"
  [gens mul]
  (let [S (sgp/sgp-by-gens gens mul)
        elts (vec (concat gens (remove (set gens) S)))
        indices (zipmap elts (range (count elts)))
        gentab (vec (pmap
                     (fn [x] (->> gens
                                  (map #(mul x %))
                                  (map #(indices %))
                                  (vec)))
                     elts))]
    {:gens (range (count gens))
     :gentab gentab
     :elts elts
     :indices indices
     :mul (fn [x y] ((gentab x) y))
     :genips (map #(sgp/index-period % mul) gens)}))

(defn target
  "data items of target semigroup"
  [gens mul]
  (let [S (sgp/sgp-by-gens gens mul)
        classes (group-by #(sgp/index-period % mul) S)
        elts (vec (concat gens (remove (set gens) S)))
        indices (zipmap elts (range (count elts)))]
    {:gens gens
     :mul mul
     :elts elts
     :indices indices
     :classes classes}))

(defn targets [src trgt] (map #((:classes trgt) %)
                              (:genips src)))

(defn embedding-seeds
  "Given a generator set this returns the sequence of all possible seed maps
  for embeddings (meaning that they are index-period checked)."
  [src trgt]
  (map (fn [l] (zipmap (:gens src) l))
       (apply cartesian-product (targets src trgt))))

(defn embeddings
  "All morphisms from embedding seeds, but lossy ones filtered out."
  [Sgens Smul Tgens Tmul]
  (let [src (source Sgens Smul)
        trgt (target Tgens Tmul)]
    (filter #(apply distinct? (vals %))
            (remove number?
                    (map #(extend-morph % (:gens src)  (:gens src) (:mul src) (:mul trgt))
                         (embedding-seeds src trgt))))))

(defn embeddings-conj
  "All morphisms from embedding seeds, but lossy ones filtered out."
  [Sgens Smul Tgens Tmul G]
  (let [[ts mSgens mSmul] (let [src (source Sgens Smul) ;not to keep src, trgt
                                 trgt (target Tgens Tmul)]
                             [(targets src trgt), (:gens src) (:mul src)])
        tgs (cons (pmap #(transf-conjrep % G) (first ts)) (rest ts))]
    (println (count (first tgs)) " candidate(s) for 1st generator")
    (loop [n 0, morphconjpairs [ [ {}, [[] G] ] ]]
      (if (= n (count Sgens))
        (map (fn [m] (zipmap Sgens (map m mSgens)))
             (map first morphconjpairs))
        (let [ngens (nth tgs n)
              maps (pmap
                      (fn [[phi cL]]
                        (let [ncongs (map #(conj-conj cL %) ngens)
                              f (fn [[umcprs imgs] cng]
                                  (let [nmorph (add-gen-and-close
                                                phi
                                                n
                                                (last (first cng))
                                                (take (inc n) mSgens)
                                                mSmul
                                                Tmul)]
                                    (if (and (coll? nmorph)
                                             (apply distinct? (vals nmorph)))
                                      (let [img (transf-set-conjrep
                                                 (vals nmorph)
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
          (recur (inc n) nmcprs))))))

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
