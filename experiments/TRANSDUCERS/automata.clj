(require '[kigen.transducer.trie :refer :all])
(require '[kigen.transducer.common :refer :all])
(require '[clojure.set :refer [difference]])


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

(defn rec-maps
  "Recursively constructs state transition mappings from a trie.
   If the stopper symbols is used for denoting word ends, then the set of
   acceptor states is also returned.
   Information traveling in recursion:
   going-in only: the trie itself (unchanged), coords to pick entries,
   current state
   going in coming back: the maps, the next available state"
  ;setting up the recursion with the initial input arguments
  ([trie] (rec-maps trie [0] ;pointing to the root of the trie
                    0 ;the defualt initial state
                    {:delta {} ;empty state transition table,
                     :acceptors #{}
                     :next 1})) ;the next assignable state 
  ([trie coords state maps]
   (let [parent (get-in trie (butlast coords))
         pos (last coords)
         thing (get-in trie coords)]
     (if (vector? thing)
       (reduce ;just making sure that the result form a branch is passed on
        (fn [m i]
          (rec-maps trie (into coords [i 0]) state m))
        maps
        (range (count thing))) ;thing is a vector, so these are the branch indices
       (if (= pos (count parent)) ;we reached the end
         maps ;this is where recursion stops, we return the collected maps
         (let [nstate (:next maps) ;we use the next available state
               nmaps (if (= thing stopper)
                       (update maps :acceptors (fn [m] (conj m state)))
                       (-> maps
                         ;add the mapping state -> new state
                           (update-in [:delta thing state] (constantly nstate))
                           (update :next inc)))]
           ;(println coords thing state "->" nstate)
           (rec-maps trie
                     (update coords (dec (count coords)) inc)
                     nstate
                     nmaps)))))))

(defn recognizer
  [words]
  (dissoc (rec-maps (build-trie (add-stoppers words))) :next))

(defn initial-partition
  "0 as the initial state is added to the non-acceptors in case it is not
   in acceptors."
  [{delta :delta acceptors :acceptors}]
  (let [stateset (set (mapcat (comp vals second) delta))
        non-acceptors (into (difference stateset acceptors)
                            (if (acceptors 0) [] [0]))]
    [non-acceptors acceptors #{nil}]))

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