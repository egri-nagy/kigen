(ns kigen.transducer.trie
  "A custom implementation of a search trie with the added feature that
   if there is no branching, then a (sub)word is stored as a sequential data structure.
   WARNING! The outputs are assumed to be different from the input symbols, as they
   are used as leaf nodes in the trie when constructing a transducer.
   The code is separated into two parts: 1. the trie data structure,
   2. transducer generation."
  (:require [kigen.transducer.common :refer (output-symbols-fn)]))

;;; trie functions ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn search
  "Searching a word in a trie, reporting the location of mismatch if the word
   is not in the trie.
   It returns the the coords (the nested keys) in the trie and the position in
   the word where the search fails. If the word is fully matched, it returns
   :matched."
  [trie word]
  (loop [coords [0], pos 0] ;start with the first entry and first symbol 
    (let [entry (get-in trie coords) ;try to get them
          letter (get word pos)]
      (cond
        (nil? letter) :matched ;no more letters in word
        (nil? entry) [coords, pos] ;no more entries in trie, the word is longer
        :else (cond
                (vector? entry) ;vector means branching
                  ;;which one is the right branch?
                (let [branch (filter
                              (fn [i] (= letter (first (get entry i))))
                              (range (count entry)))] ;branch indices
                  (if (empty? branch)
                    [coords pos] ;mismatch if no good branch found
                      ;;otherwise continue matching in the right branch
                    (recur (conj coords (first branch) 0) pos)))
                ;letter matches, keep going, next in entry, and next symbol
                (= entry letter) (recur
                                  (update coords (dec (count coords)) inc)
                                  (inc pos))
                ;different symbol so return the mismatch positions
                :else [coords, pos])))))

(defn insert-at
  "Insert a word into the trie at the location given by search."
  [trie word [coords pos]]
  (let [suffix (vec (drop pos word)) ;we only insert this suffix
        entry (get-in trie coords)]
    (if (vector? entry)
      ;adding a new branch by adding a new vector
      (update-in trie coords (fn [v] (conj v suffix)))
      ;we don't have a vector so we may need to create one
      (let [location (butlast coords) ;where we make the change
            parent (get-in trie location)
            [pref suff] (split-at (last coords) parent)
            newnode (if (empty? suff)
                      (into (vec pref) suffix) ;when no need to branch
                      (conj (vec pref) [(vec suff) suffix]))]
        (if (empty? location)
          newnode
          (update-in trie location (constantly newnode)))))))

(defn insert
  "Inserts a word into the trie. It takes care of finding the location of
   mismatch if any."
  [trie word]
  (let [match-result (search trie word)]
    (if (= match-result :matched)
      trie
      (insert-at trie word match-result))))

(defn build-trie
  "Builds a trie form a collection of words"
  [words]
  (reduce insert words))

(defn retrieve
  "Returns all the words stored in the trie."
  ([trie] (retrieve trie [] []))
  ([trie so-far result]
   (let [elts (take-while (fn [x] (not (vector? x))) trie)
         new-so-far (into so-far elts)]
     (if (= (count trie) (count elts))
       (conj result new-so-far)
       (reduce
        (fn [r v]  (retrieve v new-so-far r))
        result
        (last trie))))))

;; creating a transducer from a trie ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn outputs-as-stoppers
  "Puts the outputs into the input words so in a trie for the inputs
   it is still possible to see the outputs.
   WARNING! It is assumed that that the inputs and outputs are of different kind!"
  [io-pairs]
  (map
   (fn [[w s]] (conj (vec w) s))
   io-pairs))

(defn mappings-from-trie
  "Recursively constructs state transition mappings from a trie and the output values.
   The stoppers (the output values) are expected to be in the trie. A counter is
   maintained through recursion the have a new state if needed.
   Information traveling in recursion:
   going-in only: the trie itself (unchanged), coords to pick entries, current state
   going in and coming back: the maps, the next available state"
  ;setting up the recursion with the initial input arguments
  ([trie stoppers] (mappings-from-trie trie stoppers
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
          (mappings-from-trie trie stoppers (into coords [i 0]) state m))
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
           (mappings-from-trie trie stoppers
                               (update coords (dec (count coords)) inc)
                               nstate
                               nmaps)))))))

(defn transducer
  "Creates a transducer for the given input-output pairs by building a trie.
   Not minimized. Inputs and outputs should have empty intersection!"
  [io-pairs]
  (mappings-from-trie (build-trie (outputs-as-stoppers io-pairs))
                      (set (output-symbols-fn io-pairs))))