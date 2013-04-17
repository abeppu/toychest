(ns toychest.amb
  (:require [clojure.set :as set]
            [clojure.pprint :as pprint]))

(defn amb-let-helper [bindings body]
  (if (< 0 (count bindings))    
    (let [[form expression] (take 2 bindings)
          more-bindings (drop 2 bindings)
          filtered-recurse (if (= :where (first more-bindings))
                             `(when ~(second more-bindings)
                                ~(amb-let-helper (drop 2 more-bindings) body))
                             (amb-let-helper more-bindings body))
          res (if  (and (seq? expression) 
                        (= 'amb (first expression)))
                `(apply concat (for [~form ~(second expression)]
                                 ~filtered-recurse))
                `(let [~form ~expression]
                   ~filtered-recurse))]
      res)      
    [body]))

(defmacro amb-let [bindings body]
    "vaguely like let, except 
   -- if the expression bound to a variable is of the form
      (amb col), this has the semantics that the value of the variable
      is one of the members of the collection
   -- following the binding form, we accept a vector of requirements
      each of which a vector whose first is a set of variables to which
      it applies, and whose second is an expression depending on those vars
   -- we return a lazy seq of the values produced by the let for variable
      assignments which satisfy the requirements"
  (amb-let-helper bindings body))

(defn permutations [set]
  "given a set, give back a vector of vectors, which are all the perms
   of the elements of that set"
  (if (empty? set)
    [[]] 
    (for [item set
          others (permutations (disj set item))]
      (conj others item))))