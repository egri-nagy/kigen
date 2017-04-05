(ns kigen.orbit
  "Calculating orbits by graph search algorithms.")

(declare right-action
         right-actions ;operators as functions
         set-action ;combining actions into a single function
         single-step
         bulk-step
         full-orbit
         full-orbit-single
         full-orbit-bulk
         first-solution
         first-solution-single
         first-solution-bulk)

(defn right-action
  "Returns a right action from a binary function and an argument.
  For left actions, the function partial can be used."
  [binaryfunc arg]
  (fn [x] (binaryfunc x arg)))

(defn right-actions
  "Produces a right-action function for each arguments."
  [binaryfunc args]
  (map (partial right-action binaryfunc) args))

(defn set-action
  "Combines several action functions into a single set-valued function."
  [afs]
  (fn [x]
    (set (for [f afs] (f x)))))

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
  "Generic graph-search for poducing the full orbit from seeds
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
