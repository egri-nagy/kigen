(require '[kigen.transducer.trie :refer :all])
(require '[kigen.transducer.common :refer :all])
(require '[clojure.set :refer [difference intersection union]])

;;automata related functions

(defn outputs-as-stoppers
  [io-pairs]
  (map
   (fn [[w s]] (conj (vec w) s))
   io-pairs))

(def i-o [["" :mixed]
          ["aa" :as]
          ["bb" :bs]
          ["ab" :mixed]
          ["ba" :mixed]])

(def suffs [["kisnyuszi" :ok]
            ["nagynyuszi" :ok]
            ["nyuszimuszi" :ok]
            ["farkas" :nem]
            ["roka" :nem]])

(def ex3 [["aaa" :1]
          ["baa" :1]
          ["abb" :2]])

(def ex4 [["aaa" :1]
          ["baa" :1]])

(def A {:delta {\a [1 0 4 4 4 5] \b [2 3 5 5 5 5]}
        :omega [:reject :reject :accept :accept :accept :reject]
        :n 6})

(def HU {:delta {\a [1 6 0 2 7 2 6 6] \b [5 2 2 6 5 6 4 2]}
         :omega [:reject :reject :accept :reject :reject :reject :reject :reject]
         :n 8})

(defn rec-maps
  "Recursively constructs state transition mappings from a trie.

   Information traveling in recursion:
   going-in only: the trie itself (unchanged), coords to pick entries,
   current state
   going in coming back: the maps, the next available state"
  ;setting up the recursion with the initial input arguments
  ([trie stoppers] (rec-maps trie stoppers
                             [0] ;pointing to the root of the trie
                             0 ;the default initial state
                             {:delta {} ;empty state transition table,
                              :omega {}
                              :n 1})) ;the next assignable state (also #states)
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
         (let [nstate (:n maps) ;we use the next available state
               nmaps (if (stoppers thing)
                       (update-in maps [:omega state] (constantly thing))
                       (-> maps
                         ;add the mapping state -> new state
                           (update-in [:delta thing state] (constantly nstate))
                           (update :n inc)))]
           ;(println coords thing state "->" nstate)
           (rec-maps trie stoppers
                     (update coords (dec (count coords)) inc)
                     nstate
                     nmaps)))))))

(defn transducer
  [io-pairs]
  (rec-maps (build-trie (outputs-as-stoppers io-pairs))
            (set (output-symbols-fn io-pairs))))

(defn initial-partition
  "The states are grouped by their known behaviour, i.e., byt their output
   value."
  [{omega :omega n :n :as T}] 
  (map (comp set second) (group-by omega (range n))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;the Hopcroft-Ullman 1979 algorithm
(defn ordered-pairs
  "Returns all strictly ordered pairs in A, or from A and B.
   A and B are sets of numbers (or anything that can be compared).
   Not a direct product! Only returns different entries in pairs."
  ([A] (ordered-pairs A A))
  ([A B]
   (let [pairs (for [a A, b B] (sort [a b]))
         ordered (filter (fn [[a b]] (< a b))
                         pairs)]
     (map vec (distinct ordered)))))

;; TABLE OF MARKS FOR NON_EQUIVALENCE ;;;;;;;;;;;;;;;;;
(defn initial-table
  "Intitialzing the table of non-equivalence."
  [P]
  (let [stateset (apply union P)
        non-eqs (mapcat (fn [S] ;a set S against the rest
                          (ordered-pairs
                           S
                           (difference stateset S)))
                        P)]
    (into {}
          (concat
           (map (fn [pair] [pair :x]) non-eqs) ;known-noneq
           (map (fn [pair] [pair #{}]) ;collecting possible noneqs here
                (difference (set (ordered-pairs stateset))
                            (set non-eqs)))))))

(defn joined-states
  "Not reporting the singletons."
  [table]
  (let [;getting the pairs that are in non-trivial equivalence classes
        equiv-pairs (map first (filter (fn [[_ v]] (not= v :x)) table))
        ;are the two pairs related?
        rel? (fn [p1 p2] (seq (intersection (set p1) (set p2)))) ;idiom for "not empty?"
        ;extracting the equivalent elements
        extract (fn [p1 pairs]
                  (reduce ;collecting everything equivalent to p1
                   (fn [result pair]
                     (if (rel? result pair)
                       (into result pair)
                       result))
                   (set p1)
                   pairs))]
    ;in each run of the loop we sweep up an equivalence class
    (loop [pairs equiv-pairs result #{}]
      (if (empty? pairs)
        result
        (let [S (extract (first pairs) pairs)
              remainder (filter (comp not (partial rel? S)) pairs)]
          (recur remainder (conj result S)))))))

(defn rec-mark
  "Marking a pair in the table as non-equivalent and recursively the pairs that
   can be distinguished by this pair."
  [table pair]
  (if (= :x (table pair))
    table ;recursion stops when it is already marked
    (let [to-be-marked (table pair)]
      (reduce
       rec-mark
       (update table pair (constantly :x)) ;mark the pair first
       to-be-marked))))

(defn hopcroft-ullman
  "The Hopcroft-Ullman 1979 textbook minimization algorithm."
  [{delta :delta :as T}]
  (let [P (initial-partition T)
        table (initial-table P)
        inputs (keys delta)
        pairs (mapcat ordered-pairs P)
        ;function computing the resulting pair (sorted) when applying input
        resultpair (fn [pair input] (vec (sort (map (delta input) pair))))
        resultpairs (fn [pair]
                      (remove
                       (fn [[f s]] (= f s))
                       ;(fn [[f s]] (and (= f s) (not (nil? f)))) ;TODO why this breaks
                       (distinct (map
                                  (partial resultpair  pair)
                                  inputs))))]
    (reduce
     (fn [tab pair]
       (let [rps (resultpairs pair)]
         (if (or
              (some nil? (apply concat rps))
              (some #{:x} (map tab rps)))
           (rec-mark tab pair)
           (reduce
            (fn [tab pr]
              (if (= pair pr)
                tab
                (update tab pr (fn [callbacks] (conj callbacks pair)))))
            tab
            rps))))
     table
     pairs)))

(defn recode-transducer
 [{delta :delta omega :omega n :n :as T}
  joined]
 (let [stateset (set (range n))
       nn (- (count stateset) (apply + (map (comp dec count) joined))) ;new #states
       joined-states (reduce union joined)
       singletons (apply sorted-set (difference stateset joined-states))
       ;the map from new states to original ones
       new-to-orig (zipmap
                    (range)
                    (into (vec singletons) joined))
       phi (reduce
            (fn [m [k v]]
              (if (number? v)
                (conj m [v k])
                (into m (map (fn [x] [x k]) v))))
            {}
            new-to-orig)
       phi-inv (into {} (map
                         (fn [[k v]]
                           (if (number? v)
                             [k v]
                             [k (apply min v)]))
                         new-to-orig))]
   {:delta (into {}
                 (map (fn [input]
                        [input (mapv
                                (fn [state]
                                  (phi ((delta input) (phi-inv state))))
                                (range nn))])
                      (keys delta)))
    :omega (mapv
            (fn [state]
              (omega (phi-inv state)))
            (range nn))
    :n nn}))

(defn minimize-transducer
  [T]
  (recode-transducer T (joined-states (hopcroft-ullman T))))

(defn experiment2
  [io-pairs]
  (let [T (transducer io-pairs)
        minT (minimize-transducer T)]
    (println (:n T) "->"  (:n minT))
    (println "works?" (check io-pairs minT) )))

(experiment2 suffs)