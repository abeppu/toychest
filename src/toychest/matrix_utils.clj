(ns toychest.matrix-utils
  (:refer-clojure :exclude [+ * - /])
  (:use toychest.arithmetic)
  (:import [org.ejml.simple SimpleMatrix]
           [org.ejml.data DenseMatrix64F]
           [org.ejml.factory SingularValueDecomposition]
           [org.ejml.factory DecompositionFactory]
           [org.ejml.alg.dense.decomposition.chol CholeskyDecompositionInner]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; utilities
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defrecord Matrix [m n seq-of-seqs])

(defn row [coll & [simple-matrix]]
  (if simple-matrix
    (->> [coll]
         (map double-array)
         into-array
         SimpleMatrix.)
    (->Matrix 1 (count coll) [coll])))

(defn column [coll & [simple-matrix]]
  (if simple-matrix
    (-> coll
        (row simple-matrix)
        .transpose)
    (->> coll
         (map vector)
         vec
         (->Matrix (count coll) 1)
         )))

(defn unit-norm [col-vector]
  (let [norm (.normF col-vector)]
    (.divide col-vector norm)))

(defn matrix [seq-of-seqs & [simple-matrix]]
  (if simple-matrix
    (->> seq-of-seqs
         (map double-array)
         into-array
         SimpleMatrix.)
    (let [n (count seq-of-seqs)
          m (count (first seq-of-seqs))]
      (->Matrix n m seq-of-seqs))))

(defn unwrapped-singleton [n i elem]
  (let [before (repeat i 0)
        after (repeat (- n i 1) 0)]
    (concat before [elem] after)))

(defn singleton-row [n i elem & [kind]]
  (row (unwrapped-singleton n i elem) kind))

(defn singleton-column [n i elem & [simple-matrix]]
  (let [before (repeat i 0)
        after (repeat (- n i 1) 0)]
    (column (concat before [elem] after) simple-matrix)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; matrix wrapper
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; operations that we used on matrices
;; addition, subtraction, multiplication, scaling, getting, setting, transpose
;;
;; want to be able to populate matrix with other kinds of 'number'
;; namely dual numbers, if necessary
(defmulti transpose type)
(defmulti get-entry first-type)
;;(defmulti set first-type)
(defmulti get-diag type)
(defmulti rows type)
(defmulti cols type)
(defmulti dot first-type)
(defmulti light-diag-inverse type)
(defmulti scale first-type)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; SimpleMatrix

(defmethod + SimpleMatrix [a b]
  (.plus a b))
(defmethod - SimpleMatrix [a b]
  (.minus a b))
(defmethod * SimpleMatrix
  ([a b] (.mult a b))
  ([a b & [others]]
     (reduce * (.mult a b) others)))
(defmethod dot SimpleMatrix [a b] (.dot a b))
(defmethod transpose SimpleMatrix [a] (.transpose a))
(defmethod get-entry SimpleMatrix [m i j] (.get m i j))
;;(defmethod set SimpleMatrix [m i j e] (.set m i j e))
(defmethod rows SimpleMatrix [m] (.numRows m))
(defmethod cols SimpleMatrix [m] (.numCols m))
(defmethod get-diag SimpleMatrix [m]
  (let [n (min (rows m) (cols m))]
    (for [i (range n)]
      (get-entry m i i))))
(defmethod light-diag-inverse SimpleMatrix [matrix]
  (let [n (rows matrix)
        m (cols matrix)
        diag (get-diag matrix)]
    (->> (for [i (range (rows matrix))]
           (let [elem (if (< i (min n m))
                        (/ 1.0 (nth diag i)) 0.0)]
             (unwrapped-singleton m (min i (dec m))  elem)))
         (map double-array)
         into-array
         SimpleMatrix.)))
(defmethod scale SimpleMatrix [m c]
  (.scale m c))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; home-brew matrix (allows plugging in arbitrary other kinds of numbers)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defn ensure-matrix [m]
  (if (= Matrix (type m)) m
      (->> m
           .seq-of-seqs
           (map double-array)
           into-array
           SimpleMatrix.)))
(defmethod transpose Matrix
  [matrix]
  (let [n (.n Matrix)
        m (.m Matrix)
        seq-of-seqs (.seq-of-seqs Matrix)]
    (Matrix. n m (partition m (apply interleave seq-of-seqs)))))
(defmethod get-entry Matrix
  [m i j]
  (let [row (get (.seq-of-seqs m) i)]
    (get row j)))
(defn pointwise [op a b]
  (let [n (rows a)
        m (cols a)]
    (assert (= n (rows b)))
    (assert (= m (cols b)))
    (partition m (map op [] (apply interleave a) (apply interleave b)))))
(defmethod + Matrix
  ([a b] (pointwise + a b))
  ([a b & others] (reduce + (pointwise + a b) others)))
(defmethod - Matrix [a b] (pointwise - a b))
(defmethod * Matrix [a b]
  (let [m (rows a)
        n (cols a)
        p (cols b)]
    (assert (= n (rows b)))
    (->> (for [i (range m)
               j (range p)]
           (apply + (for [k (range n)]
                      (* (get a i k)
                         (get b k j)))))
         (partition p))))
(defmethod dot Matrix [a b]
  (let [b (ensure-matrix b)]
    (reduce + (map *
                   (apply interleave (.seq-of-seqs a))
                   (apply interleave (.seq-of-seqs b))))))
(defmethod rows Matrix [m] (count m))
(defmethod cols Matrix [m] (count (first m)))
(defmethod get-diag Matrix [m]
  (let [n (min (rows m) (cols m))]
    (for [i (range n)]
      (get m i i))))
(defmethod light-diag-inverse Matrix [m]
  (->> (for [i (rows m)
             m (cols m)
             diag (get-diag m)]
         (singleton-row m i (get diag i)))
       (->Matrix m m)))
(defmethod scale Matrix [^Matrix m c]
  (->Matrix (.m m) (.n m) (map (partial map (partial * c)) (.seq-of-seqs m))))