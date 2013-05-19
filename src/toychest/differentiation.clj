(ns toychest.differentiation
  (:refer-clojure :exclude [+ * - /])
  (:use toychest.matrix-utils toychest.arithmetic)
  (:require [clojure.core :as core])
  (:import [org.ejml.simple SimpleMatrix]
           [org.ejml.data DenseMatrix64F]
           [org.ejml.factory SingularValueDecomposition]
           [org.ejml.factory DecompositionFactory]
           [org.ejml.alg.dense.decomposition.chol CholeskyDecompositionInner]))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Auto/numeric differentiation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn univariate-autodiff
  "given a f : R -> R, and an x, produce the derivative of f at x"
  [f]
  (fn [^double x]
    (let [f-real (f x)
          f-dual (f (->DualNumber x 1.0))]
      (.nilpotent f-dual))))

(defn univariate-numdiff
  [f]
  (fn [^double x]
    (let [epsilon 10e-10
          fx0 (f x)
          fx1 (f (+ x epsilon))]
      (/ (- fx1 fx0) epsilon))))

(defn vector-numdiff
  ([f] (vector-numdiff f 10e-9))
  ([f epsilon]
     (fn [^SimpleMatrix x]
       (let [fx0 (f x)
             n (.numRows x)  ;; assume is column matrix

             g (fn [^SimpleMatrix x]
                 (column (for [i (range n)]
                           (let [xi (+ x (singleton-column n i epsilon true))]
                             (/ (- (f xi) fx0) epsilon)))
                         true))]
         (g x)))))

(defn vector-autodiff
  [f]
  (fn [^SimpleMatrix x] ;; column vector
    (let [n (rows x)
          dual-x (->> (for [i (range n)]
                        (get x i 0))
                      (map #(->DualNumber % 1.0))
                      vector
                      vector
                      (->Matrix n 0))]
      (.nilpotent (f dual-x)))))