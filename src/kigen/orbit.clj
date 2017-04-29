(ns kigen.orbit
  "Calculating orbits by graph search algorithms.")

(declare single-step
         bulk-step
         full-orbit
         full-orbit-single
         full-orbit-bulk
         first-solution
         first-solution-single
         first-solution-bulk
         acyclic-search
         acyclic-search-bulk)

;; EXTENSION STRATEGIES
(defn bulk-step
  "Applies action to all elements in one go. Returns the empty set as
  unprocessed elements."
  [elts action]
  [(distinct (mapcat action elts)) #{}])

(defn single-step
  "Produces elements by applying the set-valued action to a single element
  from the given collection of elements. Also returns unprocessed elements."
  [elts action]
  [(action (first elts)) (rest elts)])

; FULL ORBIT ALGORITHMS
(defn full-orbit
  "Generic graph-search for producing the full orbit from seeds
  by applying set valued action sa. The order of the enumeration
  is determined by the step function."
  [seeds sa stepf]
  (loop [waiting (seq seeds), orbit (set seeds)]
    (if (empty? waiting)
      orbit
      (let [[extensions unprocessed] (stepf waiting sa)
            newelts (remove orbit extensions)]
        (recur (into unprocessed newelts) (into orbit newelts))))))

;; seeds - elements to act on
;; sa - set action function
(defn full-orbit-bulk
  "Bulk-extension search starting from the elements in seeds using a single
  set-valued action function producing new elements."
  [seeds sa]
  (full-orbit seeds sa bulk-step))

(defn full-orbit-single
  "Single extension search starting from the elements in seeds using a single
  set-valued action function."
  [seeds sa]
  (full-orbit seeds sa single-step))

; PARTIAL ORBITS, STOPPING AT FIRST SOLUTIONS
(defn first-solution
  "Generic search with the ability to bail out early when
  a solution is found. It returns a solution or nil."
  [seed sa candidate? solution? stepf]
  (loop [waiting (set [seed]), orbit #{}]
    (let [candidates (filter candidate? waiting)
          solutions (filter solution? candidates)]
      (if (or (not-empty solutions) (empty? candidates))
        (first solutions)
        (let [[newelts unprocessed] (stepf candidates sa)
              norbit (into orbit candidates)
              diff (remove norbit newelts)]
          (recur (into unprocessed diff) norbit))))))

(defn first-solution-single
  "Returns a first solution when searching by breadth-first."
  [seed sa candidate? solution?]
  (first-solution seed sa candidate? solution? single-step))

(defn first-solution-bulk
  "Returns a first solution when searching by depth-first."
  [seed sa candidate? solution?]
  (first-solution seed sa candidate? solution? bulk-step))

(defn acyclic-search
  "Searching for solutions by predicate solution?, where the search graph is
  guaranteed to be acyclic, thus no need for keeping the orbit."
  [seeds sa solution? stepf]
  (loop [waiting (seq seeds), solutions (set (filter solution? seeds))]
    (if (empty? waiting)
      solutions
      (let [[newelts unprocessed] (stepf waiting sa)]
        (recur (into unprocessed newelts)
               (into solutions (filter solution? newelts)))))))

(defn acyclic-search-bulk
  [seeds sa solution?]
  (acyclic-search seeds sa solution? bulk-step))
