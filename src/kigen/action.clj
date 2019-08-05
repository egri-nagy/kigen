(ns kigen.action
  "Producing actions from binary functions.")

(declare right-action  ; binary function to single argument operator
         right-actions ; bulk creation of single argument operators
         set-action) ; combining actions into a single set-valued function

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
