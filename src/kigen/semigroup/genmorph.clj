(ns kigen.semigroup.genmorph
  "Constructing isomorphisms/embeddings between semigroups given  by generators.
  In other words, searching for an isomorphisms of Cayley-graphs.
  The elements of both semigroups are fully enumerated.
  The source semigroup is converted to generator table (partial multiplication
  table containing all the images of right multiplication by generators).
  The elements of target semigroups are classified by their index-periods in
  order to find possible targets for generators."
  (:require [kigen.semigroup.sgp :refer [sgp-by-gens index-period]]
            [kigen.table.gentab :refer [gentab]]
            [orbit.core :refer [ptree-search-depth-first]] ;tree-search for single-threaded execution
            [clojure.core.reducers :as r]
            [kigen.memory-info :refer [mem-info]]
            [taoensso.timbre :refer [trace]]))

(declare sys-mul;; low-level morphism checking/extending functions
         new-mapping
         add-gen-and-close
         embeddings ;; high-level function for finding embeddings
         embeddings-distinct
         sgp-embeddings-by-gens ;;main entry point
         index-period-matched) ;;preparation

(defn index-period-matched
  "Returns for each generator in S, the elements of T with matching index-period
  values. WARNING: It fully enumerates T."
  [Sgens Smul Tgens Tmul]
  (let [S-index-period (partial index-period Smul)
        T-index-period (partial index-period Tmul)
        T (sgp-by-gens Tgens Tmul)
        Tip->Tset (group-by T-index-period T)] ;todo: wasteful collection!
    (mapv Tip->Tset (map  S-index-period Sgens))))

(defn sgp-embeddings-by-gens
  "Computes all embeddings from source semigroup to target semigroup.
  Semigroups are given by generators and their multiplication functions. Source
  semigroup is replaced by its generation table. It returns a list of maps
  containing the images of the source generators, or an empty list.
  Results are up to conjugation if conjugation action and symmetries are given."
  ([Sgens Smul Tgens Tmul] ; ALL EMBEDDINGS
   (let [{mSgens :gens mSmul :mul} (gentab Sgens Smul)]
     (map (fn [m] (zipmap Sgens (map m mSgens))) ; mappings of the generators
          (embeddings mSgens
                      mSmul
                      (index-period-matched mSgens mSmul Tgens Tmul)
                      Tmul))))
  ([Sgens Smul Tgens Tmul conj-fn-bundle] ; ALL EMBEDDINGS UP TO CONJUGATION
   (let [{mSgens :gens mSmul :mul} (gentab Sgens Smul)]
     (map (fn [m] (zipmap Sgens (map m mSgens)))
          (embeddings-distinct mSgens
                               mSmul
                               (index-period-matched mSgens mSmul Tgens Tmul)
                               Tmul
                               conj-fn-bundle)))))

(defn bijective?
  "Returns true if the given hash-map is bijective, i.e., the values are all
   distinct."
  [m]
  (apply distinct? (vals m)))

(defn embeddings
  "All embeddings of source semigroup into target induced by the possible
  images of the generators."
  [Sgens Smul targets Tmul]
  (trace (str "Number of targets:" (vec (map count targets))))
  (let [solution? (fn [[n _]] (= n (count Sgens))) ;n - #generators, phi - morph
        generator (fn [[n phi :as v]]
                    (if (solution? v)
                      []
                      (let [g (nth Sgens n)
                            ngens (take (inc n) Sgens)
                            addgen (fn [G] ; G is the candidate generator in G
                                     (when-not (some #{G} (vals phi)) ;no dups!
                                       (add-gen-and-close phi g G
                                                          ngens
                                                          Smul Tmul)))
                            result (r/reduce
                                    (fn [coll phi] (conj coll [(inc n) phi]))
                                    []
                                    (->>
                                     (r/map addgen (nth targets n))
                                     (r/remove nil?)
                                     (r/filter bijective?)))]
                        (trace (str "#gens:" n
                                    " #phi:" (count phi)
                                    " #targets:" (count (nth targets n))
                                    " #extensions:" (count result)))
                        result)))]
    (map second ; get the phi's
         (ptree-search-depth-first [[0 {}]] generator solution?))))


(defn embedding
  "Finding an embedding of source semigroup into target induced by the possible
  images of the generators by a backtrack algorithm."
  [Sgens Smul tgs Tmul]
  (trace (str "Number of targets:" (vec (map count tgs))))
  (let [N (count Sgens)] ;; the max number of levels
    (loop [n 0 ;the number of generators mapped so far
           morphs [{}] ;the partial morphisms up to this, so (morphs n) is the map after n generators mapped
           coords [-1]]
      (println coords)
      (when-not (< n 0) ;when bactracked too far, we just return nil
        ;; we try the next coordinate value if available, if not back one level
        (if-not (< (coords n) (dec (count (tgs n))))
          (recur (dec n) ;backtrack
                 (pop morphs)
                 (pop coords))
          (let [g (nth Sgens n)
                ncoord (inc (coords n))
                G ((tgs n) ncoord)
                ngens (take (inc n) Sgens)
                phi (peek morphs)
                nphi (add-gen-and-close phi g G ngens Smul Tmul)]
              ;(println nphi)
            (cond
                ;when we don't have a a good new phi
              (or (nil? nphi)
                  (not (bijective? nphi))) (recur n
                                                  morphs
                                                  (conj (pop coords) ncoord))
                ;if it is a solution, just return it
              (= (count coords) N) nphi
                ;not a solution, but good, so add a new generator
              :else (recur (inc n) 
                           (conj morphs nphi)
                           (conj (conj (pop coords) ncoord) -1)))))))))

(defn call-embedding
  [Sgens Smul Tgens Tmul] ; ALL EMBEDDINGS
  (let [{mSgens :gens mSmul :mul} (gentab Sgens Smul)]
    (map (fn [m] (zipmap Sgens (map m mSgens))) ; mappings of the generators
         (embedding mSgens
                    mSmul
                    (index-period-matched mSgens mSmul Tgens Tmul)
                    Tmul))))

(defn class-reps
  "Classifies the elements of coll by function f and keeps only
  a representative element from each class."
  [f coll]
  (map first (vals (group-by f coll))))

(defn morphisms-up-to-conjugation
  "Returns the distinct morphs up to conjugation. First checking by the equality
  of the image set, then by its conjugacy class representative."
  [morphs setconjrep]
  (->> morphs
       (class-reps (comp set vals))
       (class-reps (comp setconjrep vals))))

(defn new-generator-conjreps
  "Finds the possible target generators up to conjugation. For the very first
  generator it chooses conjugacy class representatives.
   Strategy: compute conjugacy class reps for sequences,
   but filter them with setwise conjugacy."
  [phi n Sgens tgs
   {repconj :conjrep setconjrep :setconjrep conj-conj :conjconj}]
  (if (zero? n)
    (set (map repconj (first tgs)))
    (let [gens (mapv phi (take n Sgens))
          partconj (reduce conj-conj ; for not computing minconjugators always
                           (conj-conj (first gens))
                           (rest gens))
          conjed_seqs (r/map first
                             (r/map (partial conj-conj partconj)
                                    (nth tgs n)))
          ;we need to keep distinct generator sets
          selected_seqs (class-reps setconjrep conjed_seqs)]
      (into #{} (r/map last selected_seqs))))) ;set does not work here

(defn embeddings-distinct
  "All morphisms from embedding seeds, but lossy ones filtered out."
  [Sgens Smul tgs Tmul conj-fn-bundle]
  (let [solution? (fn [[n _]] (= n (count Sgens))) ;n #generators, phi morphs
        generator (fn [[n phi :as v]]
                    (if (solution? v)
                      []
                      (let [ngens (new-generator-conjreps phi n Sgens tgs
                                                          conj-fn-bundle)
                            check-gen (fn [newmorphs ngen]
                                        (let [gens (take (inc n) Sgens)
                                              nmorph (add-gen-and-close
                                                      phi
                                                      (nth Sgens n)
                                                      ngen
                                                      gens
                                                      Smul
                                                      Tmul)]
                                          (if (and (coll? nmorph)
                                                   (bijective? nmorph)) ;iso?
                                            (conj newmorphs [(inc n) nmorph])
                                            newmorphs)))
                            result (reduce check-gen [] ngens)]
                        (trace (count phi) "elts in phi,"
                               (count ngens) "targets for gen" n ","
                               (count result) "realized" (mem-info))
                        result)))
        morphs (map second (ptree-search-depth-first [[0 {}]]
                                                     generator
                                                     solution?))]
    (trace (count morphs) "morphisms found." (mem-info))
    (morphisms-up-to-conjugation morphs (:setconjrep conj-fn-bundle))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Cayley graph morph matching                                                ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn add-gen-and-close
  "Add a new generator and close the Cayley-graph.
  There are two phases for adding a new generator
  1. we multiply everything (including itself) by the new generator only
  2. for any new elements generated we go through with all generators so far"
  [phi g G ngens Smul Tmul]
  (let [ephi (conj phi [g G])
        res (sys-mul ephi (keys ephi) [g] Smul Tmul)] ;phase 1
    (when-not (nil? res)
      (loop [ephi (:phi res) newelts (conj (:new res) g)]
        (if (empty? newelts)
          ephi
          (let [res (sys-mul ephi newelts ngens Smul Tmul)]
            (when-not (nil? res)
              (recur (:phi res) (:new res)))))))))

(defn sys-mul
  "Systematic right multiplication of elts by gens and collecting new elements.
  Elts and gens are all in phi already. Phi is being built along the way
  but new elements need proper extension later, that's why they are collected."
  [phi elts gens Smul Tmul]
  (loop [phi phi
         newelts [] ;newly generated elements
         pairs (for [a elts g gens] [a g])] ;possible a,g pairs for products
    (if (empty? pairs)
      {:phi phi :new newelts}
      (let [p (new-mapping phi (first pairs) Smul Tmul)]
        (cond (nil? p) nil ;not morphic
              (empty? p) (recur phi ;we know the product, it is morphic
                                newelts
                                (rest pairs))
              :else (recur (conj phi p) ;we extended phi
                           (conj newelts (first p))
                           (rest pairs)))))))

(defn new-mapping
  "Attempt to extends the morphism phi for the product of element a by generator
  g and finding phi(ag). The product ag may be known or a new one.
  If phi(ag) is already known and it is morphic, then an empty vector is
  returned. 
  If it is newly found, it gives a vector [ag phi(ag)],
  that can be conjoined to phi.
  If it is not homomorphic, nil is returned.
  phi - morphism represented as a map
  a,g - elements of S  already in phi, g is a generator
  mulS, mulT - multiplication in S and T"
  [phi [a g] mulS mulT]
  (let [ag (mulS a g)
        AG (mulT (phi a) (phi g))]
    (if (contains? phi ag)
      (when (= AG (phi ag)) [])
      [ag AG])))