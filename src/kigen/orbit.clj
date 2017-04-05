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

;; elts - a set of elements
;; afs - operations that act on elts, i.e. functions: elt -> elt
(defn cayley-graph
  "Unlabelled Cayley graph of elements elts by action functions afs.
  Returns a map: element -> image set (successors) of element"
  [elts afs]
  (into {} (for [x elts]
             [x (set (for [o afs] (o x)))])))

;;clj-me.cgrand.net/2013/03/18/tarjans-strongly-connected-components-algorithm/
;;https://gist.github.com/cgrand/5188919
;;en.wikipedia.org/wiki/Tarjan's_strongly_connected_components_algorithm

;;env is a map containing:
;; 1. a stack of nodes in the order of search
;; 2. the current list of SCCs
;; 3. key-value pairs: from nodes to time of discovery (stack length)
;;                                   or oldest ancestor's value,
;;                                   or nil,
;;EXAMPLE
;;kigen.core=> (kigen.orbit/scc #{:1 :2 :3} {:1 #{:2 } :2 #{:3} :3 #{:2}})
;;#{#{:2 :3} #{:1}}
(defn scc
  "Returns the strongly connected components of a graph specified by its nodes
  and a successor function succ-func from node to nodes."
  [nodes succ-func]
  (letfn [(strongconn [env node]
;            (println "strogconn" env node)
            (if (contains? env node)
              env ;nothing to do, the node is already visited
              (let [stack (:stack env)
                    n (count stack) ;n is the time of discovery for node
                    env (assoc env node n :stack (conj stack node))
                    f (fn [env succ]
 ;                       (println "f" env succ "n=" n)
                       (let [env (strongconn env succ)]
                         (assoc env
                                node
                                (min (or (env succ) n) (env node)))))
                    env (reduce f env (succ-func node))]
                (if (= n (env node)) ; no link below, an SCC found
                  (let [nodes (:stack env)
                        scc (set (take (- (count nodes) n) nodes))
                        env (reduce #(assoc %1 %2 nil) env scc)] ;clear lengths
;                    (println "scc found" scc)
                    (assoc env :stack stack :sccs (conj (:sccs env) scc)))
                  env))))]
    (:sccs ;getting the result from the environment
     (reduce strongconn
             {:stack () :sccs #{}} ;initial environment
             nodes))))
