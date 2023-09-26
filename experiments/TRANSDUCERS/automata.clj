(require '[kigen.transducer.trie :refer :all])
(require '[kigen.transducer.common :refer :all])


;;automata related functions

;;TODO write a function that checks the io-pairs for contradicting pairs
;; like the same word requiring two different outputs

;a symbol that indicates the end of a word in the tree
;it is assumed that this character is not used anywhere
(def stopper \⏹)

(defn add-stoppers
  "Adding stopper to a list of words.
   The words are converted to vectors."
  [words]
  (map (fn [w] (conj (vec w) stopper))
       words))

(defn outputs-as-stoppers
  [io-pairs]
  (map
   (fn [[w s]] (conj (vec w) s))
   io-pairs))

(def i-o [["aa" :as]
          ["bb" :bs]
          ["ab" :mixed]
          ["ba" :mixed]])

(defn recognizer-inputs
  "Takes transducer i-o pairs and separates them into recognizer
   inputs."
  [io-pairs]
  (update-vals (group-by second io-pairs)
               (partial map first)))

(defn firsts-in-trie
  "Returns a set of the elements that appear the first positions in the
   branches."
  [trie]
  (cond
    (empty? trie) #{}
    (vector? (first trie)) (into #{} (map first (first trie)))
    :else #{(first trie)}))

;traversing the trie, collecting stored things and their coordinates
;; recursive 
(defn rec-traverse
  ([trie] (rec-traverse trie [0] 0))
  ([trie coords state]
   (let [parent (get-in trie (butlast coords))
         pos (last coords)
         thing (get-in trie coords)]
     (if (vector? thing)
       (doseq [i (range (count thing))]
         (rec-traverse trie (into coords [i 0]) state))
       (when (< pos (count parent))
         (do
           (println coords thing state "->" (inc state))
           (rec-traverse trie (update coords (dec (count coords)) inc) (inc state))))))))

(defn rec-count
  ([trie] (rec-count trie [0]))
  ([trie coords]
   (let [parent (get-in trie (butlast coords))
         pos (last coords)
         thing (get-in trie coords)]
     (if (vector? thing)
       (reduce
        (fn [sum i]
          (+ sum (rec-count trie (into coords [i 0]))))
        0 (range (count thing)))
       (if (< pos (count parent))
         (do
           (println coords thing)
           (inc (rec-count trie (update coords (dec (count coords)) inc))))
         0)))))

(defn rec-maps
  "Information traveling in recursion:
   going-in only: the trie itself (unchanged), coords to pick entries,
   current state
   going in coming back: the maps, the next available state"
  ;setting up the recursion with the initial input arguments
  ([trie stoppers] (rec-maps trie stoppers
                             [0] ;pointing to the root of the trie
                             0 ;the defualt initial state
                             {:delta {} ;empty state transition table,
                              :acceptors #{}
                              :next 1})) ;the next assignable state 
  ([trie stoppers coords state maps]
   (let [parent (get-in trie (butlast coords))
         pos (last coords)
         thing (get-in trie coords)]
     (if (vector? thing)
       (reduce ;just making sure that the result form a branch is passed on
        (fn [m i]
          (rec-maps trie stoppers (into coords [i 0]) state m))
        maps
        (range (count thing))) ;thing is a vector, so these are the branch indices
       (if (= pos (count parent)) ;we reached the end
         maps ;this is where recursion stops, we return the collected maps
         (let [nstate (:next maps) ;we use the next available state
               nmaps (if (stoppers thing)
                       (update maps :acceptors (fn [m] (conj m state)))
                       (-> maps
                         ;add the mapping state -> new state
                           (update-in [:delta thing state] (constantly nstate))
                           (update :next inc)))]
           ;(println coords thing state "->" nstate)
           (rec-maps trie stoppers
                     (update coords (dec (count coords)) inc)
                     nstate
                     nmaps)))))))




(defn traverse
  [trie]
  (let [stopper [(count trie)]]
    (loop [coords [0] bag [] counter 0]
      (let [location (vec (butlast coords))
            parent (get-in trie location)
            pos (last coords)
            thing (get-in trie coords)
            ncoords (cond
                      (vector? thing)  (conj coords 0)
                      (< pos (count parent)) (update coords
                                                     (dec (count coords))
                                                     inc)
                      (nil? thing) (update location
                                           (dec (count location))
                                           inc))
            not-real? (or (nil? thing) (vector? thing))
            nbag (if not-real?
                   bag
                   (conj bag ["coords:" coords "thing:" thing counter]))
            ncounter (if not-real? counter (inc counter))]
        (if (= stopper ncoords)
          nbag
          (recur ncoords nbag ncounter))))))