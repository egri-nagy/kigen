(ns kigen.orbit
  "Calculating orbits by graph search algorithms."
  (:require [clojure.set :refer [difference]]))

(declare right-action
         right-actions ;operators as functions
         set-action ;combining actions into a single function
         dfs-step
         dfs ;depth-first search
         bfs-step
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
  (let [initial (stepf seeds sa)] ;initializing the search alg. data structure
    (loop [waiting (first initial) orbit (second initial)]
      (if (empty? waiting)
        orbit
        (let [[waiting orbit] (stepf waiting orbit sa)]
          (recur waiting orbit))))))

;; BREADTH-FIRST SEARCH
(defn bfs-step
  "The next step of a depth-first search including the initial one."
  ([seeds sa] [ [(set seeds)] (set seeds) ])
  ([stack orbit sa]
   (let [newelts (remove orbit (mapcat sa (last orbit)))]
     [(conj orbit newelts)
      (into orbit newelts)])))

;; seeds - elements to act on
;; sa - set action function
(defn bfs
  "Breadth-first search starting from the elements in seeds using a single
  set-valued action function producing new elements."
  [seeds sa]
  (full-orbit seeds sa dfs-step))

;;DEPTH-FIRST SEARCH (same arguments as bfs)
(defn dfs-step
  "The next step of a depth-first search including the initial one."
  ;initialization, creating a stack
  ([seeds sa] [(vec seeds) (set seeds)])
  ;extending the element at the top of the stack
  ([stack orbit sa]
   (let [newelts (remove orbit (set (sa (peek stack))))]
     [(into (pop stack) newelts)
      (into orbit newelts)])))

(defn dfs
  "Depth-first search starting from the elements in seeds using a single
  set-valued action function."
  [seeds sa]
  (full-orbit seeds sa dfs-step))

(defn controlled-bfs
  "Breadth-first search with the ability to bail out early when
  a solution is found. The partial orbit and the solutions found
  in the last frontline is returned in a map with keywords :orbit :solutions."
  [seed sa candidate? solution?]
  (cond (solution? seed) {:orbit #{seed} :solutions #{seed}}
        (not (candidate? seed)) {:orbit #{} :solutions #{}}
        :else (loop [frontier [seed] orbit  #{seed}]
                (let [elts (mapcat sa frontier)
                      diff (filter
                            (fn [x] (and (not (contains? orbit x))
                                         (candidate? x)))
                            elts)
                      solutions (filter solution? diff)
                      norbit (into orbit diff)]
                  (if (or (not-empty solutions) (empty? diff))
                    {:orbit norbit :solutions (set solutions)}
                    (recur diff norbit))))))

;; (defn trace
;;   "Tracing a path to an element in the orbit graph"
;;   [e og]
;;   (drop 1 (rseq (loop [e e r []]
;;                   (if (nil? e)
;;                     r
;;                     (let [[[k v]] (seq (og e))]
;;                       (recur v (conj r k))))))))

;; ;;immensely wasteful, but will do the job in the beginning
;; (defn geodesic
;;   [source target gens action]
;;   (trace target (:graph (orbit-graph source (right-actions gens action)))))

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
