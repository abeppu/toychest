(ns toychest.amb
  (:require [clojure.set :as set]
            [clojure.pprint :as pprint]))

(defn unravel 
  "given a form which may have vector-style destructuring,
   (e.g. '[a b c :as alphas]), picks out the variables in it"
  [form]
  (cond
   (symbol? form) #{form}
   (= :as form) #{}
   :else (reduce clojure.set/union (map unravel form))))

(defn amb-let-helper
  "helper for amb-let;
   so-far keeps track of which variables we've already defined
   for the purposes of recognizing when a requirement can be used"
  [bindings requirements body so-far]
  (if (< 0 (count bindings))
    (let [[form expression] (take 2 bindings)         
          more-bindings (drop 2 bindings)]
      (if (= (first expression) 'amb)   
        (let [defined (clojure.set/union so-far (unravel form))
              grouped-reqs (group-by #(clojure.set/subset? (first %) defined) requirements)
              valid-so-far (cons 'and (seq (map second (get grouped-reqs true))))
              remaining-reqs (get grouped-reqs false)
              deeper (amb-let-helper more-bindings
                                     remaining-reqs
                                     body
                                     defined)]
          `(apply concat (remove nil? (for [~form ~(second expression)]
                                        (when ~valid-so-far
                                          ~deeper
                                          )))))
        (let [deeper (amb-let-helper more-bindings
                                     requirements
                                     body
                                     so-far)]
          `(apply concat (remove nil? (let [~form ~expression]
                                        ~deeper))))))
    body))


(defmacro amb-let
  "vaguely like let, except 
   -- if the expression bound to a variable is of the form
      (amb col), this has the semantics that the value of the variable
      is one of the members of the collection
   -- following the binding form, we accept a vector of requirements
      each of which a vector whose first is a set of variables to which
      it applies, and whose second is an expression depending on those vars
   -- we return a lazy seq of the values produced by the let for variable
      assignments which satisfy the requirements"
  [bindings requirements body]
  (let [a (amb-let-helper bindings requirements (vector body) #{})]
    a))

(defn permutations [set]
  "given a set, give back a vector of vectors, which are all the perms
   of the elements of that set"
  (if (empty? set)
    [[]] 
    (for [item set
          others (permutations (disj set item))]
      (conj others item))))