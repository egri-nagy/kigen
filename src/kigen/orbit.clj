;; calculating orbits by graph search algorithms
(ns kigen.orbit
  (:use [clojure.set :only [difference]]))

(declare right-action
         right-actions ;operators as functions
         set-action ;combining actions into a single function
         dfs ;depth-first search
         bfs ;breadth-first search
         orbit-graph)

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

;; BREADTH-FIRST SEARCH
;; seeds - elements to act on
;; sa - set action function
(defn bfs
  "Breadth-first search starting from the elements in seeds using a single
  set-valued action function producing new elements."  
  [seeds sa]
  ;; o - vector of sets containing orbit elements in production order
  ;; total - cumulative union of orbit elements
  (loop [o [(set  seeds)], total (first o)]
    ; simple logging (print (count total)) (print "\n") (flush)
    (let [newset (set (mapcat sa (last o)))
          diff (difference newset total)]
      (if (empty? diff)
        total
        (recur (conj o diff) (into total diff))))))

;;DEPTH-FIRST SEARCH (same arguments as bfs)
(defn dfs
  "Depth-first search starting from the elements in seeds using a single
  set-valued action function."
  [seeds af]
  (loop [stack (vec seeds)  orbit (set seeds)]
    (if (empty? stack)
      orbit
      (let [frontier (set (af (peek stack)))
            newelts (filter #(not (contains? orbit %)) frontier)]
        (recur (into (pop stack) newelts)
               (into orbit newelts))))))

;;ORBIT-GRAPH
;;stops if some solutions found
;;we start search from one specific seed element
(defn orbit-graph
  ([seed afs]
   (letfn [(T [] true) ;default candidate predicate
           (F [] false)] ;default solution predicate
     (orbit-graph seed afs T F)))
  ([seed afs candidate? solution?]
   (let [fs (vec afs) ;the order of the action functions does matter
         indxs (range 0 (count fs))
         seed-graph {:graph {seed {}} :orbit #{seed}}
         null-graph {:graph {} :orbit #{} :solutions #{}}]
     (cond (solution? seed) (assoc seed-graph :solutions #{seed})
           (not (candidate? seed)) null-graph
           :else (loop [frontier [seed] og seed-graph]
                   (let [elts (for [x frontier i indxs] [((nth fs i) x) {i x}])
                         diff (filter
                               (fn [[x]] (and (not (contains? (:orbit og) x))
                                              (candidate? x)))
                               elts)
                         newfront (map first diff)
                         solutions (set (filter solution? newfront))]
                     (if (or (not-empty solutions) (empty? newfront))
                       (assoc  og :solutions solutions)
                       (recur newfront {:orbit (into (:orbit og) newfront)
                                        :graph (into (:graph og) diff)}))))))))

(defn trace
  "Tracing a path to an element in the orbit graph"
  [e og]
  (drop 1 (rseq (loop [e e r []]
                  (if (nil? e)
                    r
                    (let [[[k v]] (seq (og e))]
                      (recur v (conj r k))))))))

;;immensely wasteful, but will do the job in the beginning
(defn geodesic
  [source target gens action]
  (trace target (:graph (orbit-graph source (right-actions gens action)))))

;; the operations should be supplied (can be left or right action)
;; elts - a set of elements
;; afs - operations that act on elts, i.e. functions: elts -> elts
(defn cayley-graph
  ;;  TODO the graph is not labelled
  [elts afs]
  (into {} (for [x elts]
             [x (set (for [o afs] (o x)))])))

;;clj-me.cgrand.net/2013/03/18/tarjans-strongly-connected-components-algorithm/
;;en.wikipedia.org/wiki/Tarjan's_strongly_connected_components_algorithm

;;env is a map from nodes to stack length or nil,
;;nil means the node is known to belong to another SCC
;; ::stack  the current stack
;; ::sccs   the current set of SCCs
(defn scc
  "Returns the strongly connected components of a graph specified by its nodes
  and a successor function succs from node to nodes."
  [nodes succs]
  (letfn [(strongconn [env node]
            (if (contains? env node)
              env ;nothing to do, the node is already visited
              (let [stack (::stack env)
                    n (count stack)
                    env (assoc env node n ::stack (conj stack node))
                    env (reduce (fn [env succ]
                                  (let [env (strongconn env succ)]
                                    (assoc env
                                           node
                                           (min (or (env succ) n) (env node)))))
                                env (succs node))]
                (if (= n (env node)) ; no link below, an SCC found
                  (let [nodes (::stack env)
                        scc (set (take (- (count nodes) n) nodes))
                        env (reduce #(assoc %1 %2 nil) env scc)] ;clear lengths
                    (assoc env ::stack stack ::sccs (conj (::sccs env) scc)))
                  env))))]
    (::sccs ;getting the result from the environment
     (reduce strongconn
             {::stack () ::sccs #{}} ;initial environment
             nodes))))
