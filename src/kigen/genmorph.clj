(ns kigen.genmorph
  "Constructing morphisms by generators, i.e. searching for an isoomorphisms of
  Cayley-graphs."
  (:require [kigen.sgp :as sgp]
            [kigen.gentab :refer [gentab]]
            [kigen.conjugacy :as conjugacy]))

(declare extend-morph ;; low-level morphism checking/extending functions
         add-edge
         extend-node
         add-gen-and-close
         embeddings ;; high-level function for finding embeddings
         embeddings-conj
         sgp-embeddings-by-gens ;;main entry point
         index-period-matched) ;;preparation

(defn index-period-matched
  "Returns for each generator in S, the elements of T with matching index-period
  values. WARNING: It fully enumerates T."
  [Sgens Smul Tgens Tmul]
  (let [T (sgp/sgp-by-gens Tgens Tmul)
        genips (map #(sgp/index-period % Smul) Sgens)
        genipset (set genips)
        Sgenips->Tsets (reduce (fn [m t]
                                 (let [ip (sgp/index-period t Tmul)]
                                   (if (contains? genipset ip)
                                     (update m ip conj t)
                                     m)))
                               (zipmap genipset (repeat []))
                               T)]
    (map Sgenips->Tsets genips)))

(defn sgp-embeddings-by-gens
  "Computes all embeddings from source semigroup to target semigroup.
  Semigroups are given by generators and their multiplication functions. Source
  semigroup is replaced by its generation table. It returns a list of maps
  containing the images of the source generators, or an empty list.
  Results are up to conjugation if conjugation action and symmetries are given."
  ([Sgens Smul Tgens Tmul] ; ALL EMBEDDINGS
   (let [[mSgens mSmul] (let [src (gentab Sgens Smul)] [(:gens src) (:mul src)])
         tgs (index-period-matched mSgens mSmul Tgens Tmul)]
     (map (fn [m] (zipmap Sgens (map m mSgens)))
          (embeddings mSgens mSmul tgs Tmul))))
  ([Sgens Smul Tgens Tmul Tconj G] ; ALL DISTINCT EMBEDDINGS UP TO CONJUGATION
   (let [[mSgens mSmul] (let [src (gentab Sgens Smul)] [(:gens src) (:mul src)])
         ts (index-period-matched mSgens mSmul Tgens Tmul)
         conjrep (partial conjugacy/conjrep Tconj)
         tgs (cons (distinct (pmap #(conjrep % G) (first ts))) (rest ts))]
     (map (fn [m] (zipmap Sgens (map m mSgens)))
          (embeddings-conj mSgens mSmul tgs Tmul Tconj G)))))

(defn embeddings
  "All embeddings of source semigroup into target induced by the possible
  images of the generators."
  [Sgens Smul tgs Tmul]
  (loop [n 0, morphs [{}]]
    (println (count morphs))
    (if (= n (count Sgens))
      morphs
      (let [nmorphs (mapcat #(pmap
                              (fn [m]
                                (add-gen-and-close m
                                                   (nth Sgens n)
                                                   %
                                                   (take (inc n) Sgens)
                                                   Smul
                                                   Tmul))
                              morphs)
                            (nth tgs n))]
        (recur (inc n) (filter #(apply distinct? (vals %))
                               (remove number? nmorphs)))))))

(defn embeddings-conj
  "All morphisms from embedding seeds, but lossy ones filtered out."
  [Sgens Smul tgs Tmul Tconj G]
  (println (count (first tgs)) " candidate(s) for 1st generator")
  (let [conj-conj (partial conjugacy/conj-conj Tconj)
        setconjrep (partial conjugacy/setconjrep Tconj)]
    (loop [n 0, morphs [{}] ]
      (if (= n (count Sgens))
        (map first (vals (group-by #(setconjrep (vec (vals %)) G) morphs)))
        (letfn [(extend-phi [phi]
                  (let [currentgens(if (empty? phi)
                                     []
                                     (mapv phi
                                           (take n Sgens)))
                        partconj (if (empty? currentgens) [[] G]
                                     (reduce conj-conj [[] G] currentgens))
                        ngens (distinct
                               (map (comp last first)
                                    (map #(conj-conj partconj %) (nth tgs n))))
                        check-gen (fn [imgs2morphs ngen]
                                    (let [gens (take (inc n) Sgens)
                                          nmorph (add-gen-and-close
                                                  phi
                                                  (nth Sgens n)
                                                  ngen
                                                  gens
                                                  Smul
                                                  Tmul)]
                                      (if (and (coll? nmorph)
                                               (apply distinct? (vals nmorph)))
                                        (let [img (setconjrep
                                        ; (vec (sort (vals nmorph)))
                                                   (vec (sort (map nmorph gens)))
                                                   G)]
                                          (if (contains? imgs2morphs img)
                                            imgs2morphs
                                            (assoc imgs2morphs img nmorph)))
                                        imgs2morphs)))]
                    (reduce check-gen {} ngens)))]
          (let [nmorphs (vals (apply merge (pmap extend-phi morphs)))]
            (println "gens:" (inc n) "morphs:" (count nmorphs))
            (recur (inc n) nmorphs)))))))


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
