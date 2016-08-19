;; calculating orbits by graph search algorithms
(ns kigen.orbit
  (:use [clojure.set :only [difference]]))

(declare right-action
         right-actions ;operators as functions
         set-action ;combining actions into a single function
         dfs ;depth-first search
         bfs ;breadth-first search
         controlled-dfs)

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
;; seed - we start search from this single specific  element
(defn controlled-dfs
  [seed afs candidate? solution?]
  (cond (solution? seed) {:orbit #{seed} :solutions #{seed}}
        (not (candidate? seed)) {:orbit #{} :solutions #{}}
        :else (loop [frontier [seed] o {:orbit #{seed}}]
                (let [elts (for [x frontier a afs] (a x))
                      diff (filter
                            (fn [x] (and (not (contains? (:orbit o) x))
                                         (candidate? x)))
                            elts)
                      solutions (set (filter solution? diff))]
                  (if (or (not-empty solutions) (empty? diff))
                    (assoc  o :solutions solutions)
                    (recur diff {:orbit (into (:orbit o) diff)}))))))

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
