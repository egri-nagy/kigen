(ns kigen.orbit
  (:use [clojure.set :only [difference]]))

(declare orbit
         bfs
         orbit-graph
         actions)

;; seed - elements to act on
;; funcs - functions that produce a new element applied to an element
;; TODO this is just temporary dispatching
(defn orbit
  [seeds afs] (bfs seeds afs))

(defn actions
  "Creating actions (as functions) from operators and a function for
  describing how an operator acts"
  [operators action]
  (for [o operators] #(action % o)))

;; BREADTH-FIRST SEARCH
;; seeds - elements to act on
;; afs - action functions
(defn bfs
  [seeds afs]
  ;; o - vector of sets containing orbit elements in production order
  ;; total - cumulative union of orbit element
  (loop [o [(set  seeds)], total (first o)]
    (let [newset (set (for [x (last o) f afs] (f x)))
          diff (difference newset total)]
      (if (empty? diff)
        total
        (recur (conj o diff) (into total diff))))))

;; DEPTH-FIRST SEARCH
;; stack: node and action function pairs, the work that still needs to be done
(defn dfs
  [seeds afs]
  (let [todof #(for [af afs] [% af])] ; elt -> pairs [elt af], todo list func
    (loop [stack (into [] (mapcat todof seeds))  orbit (set seeds)]
      (if (empty? stack)
        orbit
        (let [[e af] (peek stack)
              ne (af e)
              nstack (pop stack)]
          (if (contains? orbit ne)
            (recur nstack orbit)
            (recur (into nstack (todof ne)) (conj orbit ne))))))))

;;another variant - environment style
(defn dfs2
  [seeds afs]
  (letfn [(todof [t] (for [af afs] [t af])) ; elt -> pairs [elt af]
          (f [env]
            (let [stack (::stack env)
                  orbit (::orbit env)]
              (if (empty? stack)
                orbit
                (let [[e af] (peek stack)
                      ne (af e)
                      nstack (pop stack)]
                  (if (contains? orbit ne)
                    (recur (assoc env ::stack nstack))
                    (recur (assoc env
                                  ::stack (into nstack (todof ne))
                                  ::orbit (conj orbit ne))))))))]
    (f {::stack (into [] (mapcat todof seeds))
        ::orbit (set  seeds)})))

;;dfs with a single set-valued operator enabling very simple code
(defn sdfs
  "Depth-first search starting from the elements in seeds using a single
  set-valued action function."
  [seeds af]
  (loop [stack (vec seeds)  orbit (set seeds)]
    (if (empty? stack)
      orbit
      (let [frontier (af (peek stack))
            newelts (filter #(not (contains? orbit %)) frontier)]
        (recur (into (pop stack) newelts)
               (into orbit newelts))))))


;;ORBIT-GRAPH
;;we start search from one specific seed element
(defn orbit-graph
  [seed afs]
  (let [fs (vec afs) ;the order of the action functions does matter
        indxs (range 0 (count fs))]
    (loop [frontier [seed] og {:graph {seed {}} :orbit #{seed}}]
      (let [elts (for [x frontier i indxs] [((nth fs i) x) {i x}])
            diff (filter (fn [[x]] (not (contains? (:orbit og) x))) elts)
            newfront (map first diff)]
        (if (empty? newfront)
          og
          (recur newfront {:orbit (into (:orbit og) newfront)
                           :graph (into (:graph og) diff)}))))))

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
  (trace target (:graph (orbit-graph source (actions gens action)))))

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
