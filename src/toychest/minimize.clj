(ns toychest.minimize
  "this is intended to build up towards disciplined convex optimization"
  (:refer-clojure :exclude [+ * - /])
  (:use toychest.arithmetic)
  (:require [toychest.matrix-utils :as matrix] )
  (:import [org.ejml.simple SimpleMatrix]
           [org.ejml.data DenseMatrix64F]
           [org.ejml.factory SingularValueDecomposition]
           [org.ejml.factory DecompositionFactory]
           [org.ejml.alg.dense.decomposition.chol CholeskyDecompositionInner]))

;; basis : a fn which produces an infinite seq?

(defn powers [x]
  (cons 1 (lazy-seq (map #(* x %) (powers x)))))

(defn sines
  ([x] (sines x 1))
  ([x c] (cons (Math/sin (* c x))
               (cons (Math/cos (* c x))
                     (lazy-seq (sines x (inc c)))))))

(defn random-fn [dim basis & [weights]]
  (let [weights (or weights (repeatedly rand))]
    (fn [x]
      (->> (take dim (basis x))
           (map * (take dim weights))
           (reduce +)))))


(defn lsq [X y]
  "given NxD matrix X and Nx1 col vector y
   return Dx1 vector b such that (Xb - y)^2 is minimized"
  ;; Xb = y cannot be solved
  ;; instead solve
  ;; Xb = p, where p is projection of y into col space of X
  ;; officially p = X (X^T X)^-1 X^-1 y
  ;; but we want to avoid inverting
  ;;
  ;; this takes the expensive route of SVD
  ;; investigate other ways later
  (assert (= org.ejml.simple.SimpleMatrix (type X)))
  (assert (= org.ejml.simple.SimpleMatrix (type y)))
  (assert (= (.numRows X) (.numRows y)))
  (assert (= 1 (.numCols y)))
  (let [svd (DecompositionFactory/svd (.numRows X)
                                      (.numCols X)
                                      true
                                      true
                                      false)
        _ (.decompose svd (.getMatrix X))
        [u w v]  [(SimpleMatrix. (.getU svd nil true))
                  (SimpleMatrix. (.getW svd nil))
                  (SimpleMatrix. (.getV svd nil false))]
        w+ (matrix/light-diag-inverse w)
        ;; u and v are orth matrices; w is diag
        b-hat (.mult v (.mult (.transpose w+) (.mult u y)))]
    b-hat
    ))

(comment
  "goals :

  -- be able to handle least squares, linear programming, general
     convex programming

  -- have general methods which can handle even the special cases
     (i.e. don't worry about performance yet)

  what are pieces I should have in place?

  -- autodiff; e.g. state a function, and be able to produce its
     derivative.

  what steps comprise a reasonable path forward?

  -- start by just minizing a single multivariate function subject to
     no constraints.

  what are potential issues?

  -- clojure doesn't have operator overloading; can do (:refer-clojure
     :exclude [+]) in namespace declaration, but nothing more
     fine-grained than that. This makes things like dual numbers, sums
     of functions, etc, awkward. Maybe I can use macros which find and
     replace math symbols with methods?

  -- s-expressions actually seem pretty awkward for math; I'd really
     like to be able to say (x*x + x + 3) sometimes

  -- not having generics (this is a function from R^n -> R) could
     potentially make this confusing.

  what are potential solutions?

  -- use Scala

  -- clojure's big weapon is that macros are supposedly easy.
"


  )

(defn l2 [l r]
  (let [diff (.minus l r)]
    (.dot diff diff)))


(comment
  "Autodiff using dual numbers?

  -- dual numbers introduce a new arithmetic

  -- in scala I would do dual number arithmetic with operator
     overloading, and then have functions accept a type parameter, so
     you could apply to either normal numbers (or vectors) or to dual
     numbers

  -- in clojure, operators can't dispatch on type of args, so while
     you can replace '+' (by explicitly not loading the core version,
     you can't def one function and vary what kind of number you give
     it (and what kind of arithmetic gets used)

  -- you could *always* used dual numbers -- but if you will sometimes
     be evaluating the fn without also evaluating the
     derivative/gradient (as during the line-search portion of QN),
     you'd be wasting effort [is this a premature optimization
     concern?]

  -- you could explicitly pass around an arithmetic?
")

(defn gradient-minimize
  "given a function, and its gradient function, and a start point, for
  a max of max-iters steps, walk downhill"
  [f0 f1 x0 & [max-iters]]
  (loop [x x0 iters 0]
    (let [f0x (f0 x)
          f1x (f1 x)
          scaled  (matrix/scale f1x (/ -1 (inc iters)))
          xnext (+ x scaled)]
      (if (and (< iters (or max-iters 100))
               (> (l2 x xnext) 10e-9))
        (recur xnext (inc iters))
        x))))

(defn line-search
  "given a function f : R^n => R and a point x in R^n, and a vector,
  find a reasonable step along p away from x which appropriately
  reduces f(x + c * p) relative to f(x)"
  [f x p]
  nil)

(defn quasi-netwon-minimize
  "probably bfgs or similar; attempt to approximate hessian to take
  steps in a better direction than steepest decent"
  [f0 f1 x0 & [max-iters]]
  nil)

(defn dual-minimize
  ""
  [])
