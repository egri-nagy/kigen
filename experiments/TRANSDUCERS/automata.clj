(require '[kigen.transducer.trie :refer :all])
(require '[kigen.transducer.common :refer :all])
(require '[clojure.set :refer [difference]])


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

(defn recognizer-inputs
  "Takes transducer i-o pairs and separates them into recognizer
   inputs."
  [io-pairs]
  (update-vals (group-by second io-pairs)
               (partial map first)))

(defn rec-maps
  "Recursively constructs state transition mappings from a trie.
   If the stopper symbols is used for denoting word ends, then the set of
   acceptor states is also returned.
   Information traveling in recursion:
   going-in only: the trie itself (unchanged), coords to pick entries,
   current state
   going in coming back: the maps, the next available state"
  ;setting up the recursion with the initial input arguments
  ([trie stoppers] (rec-maps trie stoppers
                             [0] ;pointing to the root of the trie
                             0 ;the defualt initial state
                             {:delta {} ;empty state transition table,
                              :omega {}
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
                       (update-in maps [:omega state] (constantly thing))
                       (-> maps
                         ;add the mapping state -> new state
                           (update-in [:delta thing state] (constantly nstate))
                           (update :next inc)))]
           ;(println coords thing state "->" nstate)
           (rec-maps trie stoppers
                     (update coords (dec (count coords)) inc)
                     nstate
                     nmaps)))))))

(defn transducer
  [io-pairs]
  (dissoc (rec-maps (build-trie (outputs-as-stoppers io-pairs))
                    (set (output-symbols-fn io-pairs))) :next))

(defn initial-partition
  "0 as the initial state is added to the non-acceptors in case it is not
   in acceptors."
  [{delta :delta omega :omega}]
  (let [stateset (conj (set (mapcat (comp vals second) delta)) 0)]
    (map (comp set second) (group-by omega stateset))))

(defn split
  "What a split version of this set?
   Set S from partition P, state transition table delta"
  [S P delta]
  (if (= 1 (count S))
    nil
    (let [inputs (keys delta)
          parts (loop [symbols inputs]
                  (if (empty? symbols)
                    nil
                    (let [a (first symbols)
                          classes (group-by (fn [i] (some #(% ((delta a) i)) P)) S)]
                      (if (> (count classes) 1)
                        (map set (vals classes))
                        (recur (rest symbols))))))]
      parts)))

(defn refined-partition
  [{delta :delta :as FA}]
  (let [refine-one (fn [S P]
                     (let [parts (split S P delta)]
                       (when parts
                         (into (difference (set P) #{S}) parts))))
        refine-any (fn [P]
                     (some #(refine-one % P) P))
        ip (initial-partition FA)]
    (last (take-while (comp not nil?) (iterate refine-any ip)))))