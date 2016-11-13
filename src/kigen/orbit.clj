(ns kigen.orbit
  "Calculating orbits by graph search algorithms.")

(declare right-action
         right-actions ;operators as functions
         set-action ;combining actions into a single function
         single-step
         dfs ;depth-first search
         bulk-step
         bfs ;breadth-first search
         controlled-bfs
         full-orbit)

(defn right-action
  "Returns a right action from a binary function and an argument."
  [f a]
  #(f % a))

(defn right-actions
  "Applying right-action to a collection of arguments."
  [f as]
  (map (partial right-action f) as))

(defn set-action
  "Combining several action functions into a single set-valued function."
  [afs]
  #(set (for [f afs] (f %))))

; GENERIC ORBIT ALGORITHMS
(defn full-orbit
  "Generic graph-search for poducing the full orbit from seeds
  by applying set valued action sa. The order of the enumeration
  is determined by the step function."
  [seeds sa stepf]
  (let [initial (set seeds)]
    (loop [waiting initial, orbit initial]
      (if (empty? waiting)
        orbit
        (let [[newelts unprocessed] (stepf waiting sa)
              newelts (remove orbit newelts)]
          (recur (into unprocessed newelts) (into orbit newelts)))))))

(defn first-solution
  "Generic search with the ability to bail out early when
  a solution is found. It returns a solution or nil."
  [seed sa candidate? solution? stepf]
  (loop [waiting (set [seed]), orbit #{}]
    (let [candidates (filter candidate? waiting)
          solutions (filter solution? candidates)
          norbit (into orbit candidates)]
      (if (or (not-empty solutions) (empty? candidates))
        (first solutions)
        (let [[newelts unprocessed] (stepf candidates sa)
              newelts (remove norbit newelts)]
          (recur (into unprocessed newelts) norbit))))))

;; BREADTH-FIRST SEARCH
(defn bulk-step
  "Extends all elements in one go."
  [waiting sa]
  [(mapcat sa waiting) #{}])

(defn single-step
  "Extends only one element."
  [waiting sa]
  [(sa (first waiting)) (rest waiting)])

;; seeds - elements to act on
;; sa - set action function
(defn bfs
  "Breadth-first search starting from the elements in seeds using a single
  set-valued action function producing new elements."
  [seeds sa]
  (full-orbit seeds sa bulk-step))

(defn first-solution-bfs
  "Returns a first solution when searching by breadth-first."
  [seed sa candidate? solution?]
  (first-solution seed sa candidate? solution? single-step))

;;DEPTH-FIRST SEARCH (same arguments as bfs)

(defn dfs
  "Depth-first search starting from the elements in seeds using a single
  set-valued action function."
  [seeds sa]
  (full-orbit seeds sa single-step))

(defn first-solution-dfs
  "Returns a first solution when searching by depth-first."
  [seed sa candidate? solution?]
  (first-solution seed sa candidate? solution? single-step))

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
