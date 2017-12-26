(ns kigen.genmorph
  "Constructing morphisms by generators, i.e. searching for an isomorphisms of
  Cayley-graphs."
  (:require [kigen.sgp :as sgp]
            [kigen.conjugacy :as conjugacy]
            [kigen.sgp :refer [sgp-by-gens]]
            [orbit.core :refer [acyclic-search-single]]
            [clojure.core.reducers :as r]))

(declare sys-mul;; low-level morphism checking/extending functions
         new-mapping
         add-gen-and-close
         embeddings ;; high-level function for finding embeddings
         embeddings-conj
         sgp-embeddings-by-gens ;;main entry point
         index-period-matched) ;;preparation

(defn gentab
  "Right generation table for semigroup given by generator elements and
  multiplication."
  [gens mul]
  (let [S (sgp-by-gens gens mul)
        elts (vec (concat gens (remove (set gens) S))) ; why putting the generators in the front? confirmed: someone does assume order
        indices (zipmap elts (range (count elts)))
        gt (vec (pmap
                 (fn [x] (->> gens
                              (map #(mul x %))
                              (map indices)
                              (vec)))
                 elts))]
    [(range (count gens)) ;generators
     (fn [x y] ((gt x) y))])) ;multiplication


(defn index-period-matched
  "Returns for each generator in S, the elements of T with matching index-period
  values. WARNING: It fully enumerates T."
  [Sgens Smul Tgens Tmul]
  (let [ipfunc (fn [mul] (fn [x] (sgp/index-period x mul)))
        S-index-period (ipfunc Smul)
        T-index-period (ipfunc Tmul)
        T (sgp/sgp-by-gens Tgens Tmul)
        Tip->Tset (group-by T-index-period T)]
    (map Tip->Tset (map  S-index-period Sgens))))

(defn sgp-embeddings-by-gens
  "Computes all embeddings from source semigroup to target semigroup.
  Semigroups are given by generators and their multiplication functions. Source
  semigroup is replaced by its generation table. It returns a list of maps
  containing the images of the source generators, or an empty list.
  Results are up to conjugation if conjugation action and symmetries are given."
  ([Sgens Smul Tgens Tmul] ; ALL EMBEDDINGS
   (let [[mSgens mSmul] (gentab Sgens Smul)
         tgs (index-period-matched mSgens mSmul Tgens Tmul)]
     (map (fn [m] (zipmap Sgens (map m mSgens)))
          (embeddings mSgens mSmul tgs Tmul))))
  ([Sgens Smul Tgens Tmul Tconj G] ; ALL DISTINCT EMBEDDINGS UP TO CONJUGATION
   (let [[mSgens mSmul] (gentab Sgens Smul)
         tgs (index-period-matched mSgens mSmul Tgens Tmul)
         conjrep #(conjugacy/conjrep Tconj % G)
         setconjrep #(conjugacy/setconjrep Tconj % G)
         conj-conj (conjugacy/conj-conj-fn Tconj G)]
     (map (fn [m] (zipmap Sgens (map m mSgens)))
          (embeddings-conj mSgens mSmul tgs Tmul
                           conjrep conj-conj setconjrep)))))

(defn embeddings
  "All embeddings of source semigroup into target induced by the possible
  images of the generators."
  [Sgens Smul tgs Tmul]
  (let [solution? (fn [[n m]] (= n (count Sgens))) ;n - #generators, m - morphs
        generator (fn [[n m :as v]]
                    (if (solution? v)
                      []
                      (let [nmorphs (map
                                     (fn [g]
                                       (add-gen-and-close
                                        m
                                        (nth Sgens n)
                                        g                                                            
                                        (take (inc n) Sgens)
                                        Smul
                                        Tmul))
                                     (nth tgs n))
                            filtered (filter #(apply distinct? (vals %))
                                             (remove nil? nmorphs))]
                        (map (fn [x] [(inc n) x]) filtered))))]
    (map second (acyclic-search-single [[0 {}]]
                           generator
                           solution?))))

(defn morph-distinguisher
  "Returns the distinct morphs up to conjugation in an efficient way by
  calculating the frequencies of conjugacy class representatives. S4 -> S7
  shows that this is just a heuristics."
  [morphs repconj setconjrep]
  (let [selected (map first
                      (vals (group-by #(set (vals %)) morphs)))
        conjrepfreqs (fn [coll] (frequencies (map repconj coll)))
        classes (group-by #(conjrepfreqs (vals %)) selected)
        easykeys (filter #(= 1 (count (classes %)))  (keys classes))
        hardkeys (filter #(< 1 (count (classes %)))  (keys classes))
        hard (mapcat classes hardkeys)]
    ;(println (count morphs) "->" (count selected))
    ;(println (map count (vals classes)))
    (concat (mapcat classes easykeys)
            (map first (vals (group-by #(setconjrep (vec (vals %))) hard))))))

(defn new-generator-conjreps
  [phi n Sgens tgs repconj conj-conj]
  (if (zero? n)
    (set (map repconj (first tgs)))
    (let [gens (mapv phi (take n Sgens))
          partconj (reduce conj-conj
                           (conj-conj (first gens))
                           (rest gens))]
      (into #{}
            (r/map (comp last first)
                   (r/map #(conj-conj partconj %)
                          (nth tgs n)))))))

(defn embeddings-conj
  "All morphisms from embedding seeds, but lossy ones filtered out."
  [Sgens Smul tgs Tmul repconj conj-conj setconjrep]
  (loop [n 0, morphs [{}] ]
    (if (= n (count Sgens))
      (morph-distinguisher morphs repconj setconjrep)
      (letfn [(extend-phi [phi]
                (let [ngens (new-generator-conjreps phi n Sgens tgs repconj conj-conj)
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
                                        ;we need to keep distinct generator sets
                                                 (vec
                                                  (sort
                                                   (map nmorph gens))))]
                                        (if (contains? imgs2morphs img)
                                          imgs2morphs
                                          (assoc imgs2morphs img nmorph)))
                                      imgs2morphs)))]
                  (reduce check-gen {} ngens)))]
        (let [nmorphs (vals (apply merge (pmap extend-phi morphs)))]
          ;(println "gens:" (inc n) "morphs:" (count nmorphs))
          (recur (inc n) nmorphs))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Cayley graph morph matching                                                ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn add-gen-and-close
  "Add a new generator and close the Cayley-graph.
  There are two phases for adding a new generator
  1. we multiply everything (including itself by the new generator)
  2. for any new elements generated we go through with all generators so far"
  [phi gen phiofgen gens Smul Tmul]
  (let [ephi (conj phi [gen phiofgen])
        res (sys-mul ephi (keys ephi) [gen] Smul Tmul)] ;phase 1
    (when-not (nil? res)
      (loop [phi (:phi res) newelts (conj (:new res) gen)]
        (if (empty? newelts)
          phi
          (let [res (sys-mul phi newelts gens Smul Tmul)]
            (when-not (nil? res)
              (recur (:phi res) (:new res)))))))))

(defn sys-mul
  "Systematic right multiplication of elts by gens and collecting new elements.
  Elts and gens are all in phi already. Phi is being built along the way
  but new elements need proper extension later, that's why they are collected."
  [phi elts gens Smul Tmul]
  (loop [phi phi
         newelts [] ;newly generated elements
         pairs (for [a elts b gens] [a b])] ;possible a,b pairs for products
    (if (empty? pairs)
      {:phi phi :new newelts}
      (let [v (first pairs)
            p (new-mapping phi (first v) (second v) Smul Tmul)]
        (cond (nil? p) p
              (empty? p) (recur phi
                                newelts
                                (rest pairs))
              :else (recur (conj phi p)
                           (conj elts (first p))
                           (rest pairs)))))))

(defn new-mapping
  "Extends the morphism phi by multiplying a by b and finding phi(a,b) if
  the map is homomorphic, if not it returns nil.
  If phi(a,b) is already known, then it returns an empty vector. If it is newly
  found, it gives a vector [ab phi(a,b)], that can be conjoined to phi.
  phi - morphism represented as a map
  a,b - elements of S  already in phi
  mulS, mulT - multiplication in S and T"
  [phi a b mulS mulT]
  (let [ab (mulS a b)
        AB (mulT (phi a) (phi b))]
    (if (contains? phi ab)
      (when (= AB (phi ab)) [])
      [ab AB])))
