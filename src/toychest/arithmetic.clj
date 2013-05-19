(ns toychest.arithmetic
  (:refer-clojure :exclude [+ * - /])
  (:require [clojure.core :as core]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; operations
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn first-type [l & others] (type l))
(defmulti + first-type)
(defmulti * first-type)
(defmulti - first-type)
(defmulti / first-type)
(defmulti sin type)
(defmulti cos type)
(comment
  "
TODO : pow
       exp log
       sin/cos/tan ...
       asin/acos/atan ...

")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; built-in : covers longs, doubles
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defmethod + java.lang.Number
  ([a] a)
  ([a b] (core/+ a b))
  ([a b & others] (reduce core/+ (core/+ a b) others)))
(defmethod * java.lang.Number
  ([a] a)
  ([a b] (core/* a b))
  ([a b & others] (reduce core/* (core/* a b) others)))
(defmethod - java.lang.Number
  ([a] (core/- 0 a))
  ([a b] (core/- a b))
  ([a b & others] (reduce core/- (core/- a b) others)))

;; for some reason, the reader barfs on 'core//' but is fine on 'clojure.core//'
(defmethod / java.lang.Number
  ([a] (/ 1 3))
  ([a b] (clojure.core// a b))
  ([a b & others] (reduce / (/ a b) others)))

(defmethod sin java.lang.Number [x] (Math/sin x))
(defmethod cos java.lang.Number [x] (Math/cos x))

(defprotocol PDecomposable
  (decompose [this]))

(defn piecewise-apply
  "helper for composite numbers;
   for point-wise operations"
  [op a  b ]
  (map op (decompose a) (decompose b)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Complex numbers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defrecord Complex
    [^double real ^double imag]
  PDecomposable
  (decompose [this] [real imag]))

(defn ensure-complex [b]
  (if (= Complex (type b)) b (Complex. b 0.0)))

(defn complex-destructor [c]
  [(.real c) (.imag c)])

(defmethod + Complex [^Complex a b]
  (let [b (ensure-complex b)]
    (apply ->Complex (piecewise-apply + a b))))

(defmethod - Complex [^Complex a b]
  (let [b (ensure-complex b)]
    (apply ->Complex (piecewise-apply - a b))))

(defmethod * Complex [^Complex a b]
  (let [b (ensure-complex b)]
    (Complex. (- (* (.real a) (.real b))
                 (* (.imag a) (.imag b)))
              (+ (* (.real a) (.imag b))
                 (* (.real b) (.imag a))))))

(defmethod / Complex [^Complex l r]
  (let [r (ensure-complex r)
        [a b] (decompose l)
        [c d] (decompose r)
        denom (+ (* c c) (* d d))]
    (Complex. (/ (+ (* a c) (* b d)) denom)
              (/ (- (* b c) (* a d)) denom))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Dual numbers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defrecord DualNumber [^double real ^double nilpotent]
  PDecomposable
  (decompose [this] [real nilpotent]))

(defn ensure-dual [b]
  (if (= DualNumber (type b)) b (DualNumber. b 0.0)))

(defmethod + DualNumber [^DualNumber a b]
  (let [b (ensure-dual b)]
    (apply ->DualNumber (piecewise-apply + a b))))

(defmethod - DualNumber [^DualNumber a b]
  (let [b (ensure-dual b)]
    (apply ->DualNumber (piecewise-apply - a b))))

(defmethod * DualNumber [^DualNumber a b]
  (let [b (ensure-dual b)]
    (DualNumber. (* (.real a) (.real b))
                 (+ (* (.real a) (.nilpotent b))
                    (* (.real b) (.nilpotent a))))))

(defmethod / DualNumber [^DualNumber l r]
  (let [r (ensure-dual r)
        [a b] (decompose l)
        [c d] (decompose r)]
    (DualNumber. (/ a c)
                 (/ (- (* b c) (* a d)) (* d d)))))

(defmethod sin DualNumber [^DualNumber x]
  (let [[real nilpotent] (decompose x)]
    (DualNumber. (sin real) (* nilpotent (cos real)))))

(defmethod cos DualNumber [^DualNumber x]
  (let [[real nilpotent] (decompose x)]
    (DualNumber. (cos real) (* (- 0 nilpotent) (sin real)))))
