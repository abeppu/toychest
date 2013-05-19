(ns toychest.minimize-test
  (:refer-clojure :exclude [+ * - /])
  (:use clojure.test toychest.minimize toychest.matrix-utils toychest.arithmetic)
  (:require [toychest.differentiation :as diff])
  (:import [org.ejml.simple SimpleMatrix]))

(deftest column-test
  (testing "can make column vector"
    (let [simple  (column [1 2 3] true)]
      (is simple) ;; makes something
      (is (= org.ejml.simple.SimpleMatrix
             (type simple)))
      (is (= 3 (rows simple)))
      (is (= 1 (cols simple)))
      (is (= 1.0 (get-entry simple 0 0)))
      (is (= 2.0 (get-entry simple 1 0)))
      (is (= 3.0 (get-entry simple 2 0))))))

(deftest row-test
  (testing "can make row vector"
    (let [simple  (row [1 2 3] true)]
      (is simple) ;; makes something
      (is (= org.ejml.simple.SimpleMatrix
             (type simple)))
      (is (= 1 (rows simple)))
      (is (= 3 (cols simple)))
      (is (= 1.0 (get-entry simple 0 0)))
      (is (= 2.0 (get-entry simple 0 1)))
      (is (= 3.0 (get-entry simple 0 2))))))

(deftest matrix-test
  (testing "can make matrices"
    (let [seq-of-seqs [[1 2 3] [4 5 6] [7 8 9] [10 11 12]]
          matrix (matrix seq-of-seqs true)]
      (is (= org.ejml.simple.SimpleMatrix (type matrix)))
      (is (= 4 (rows matrix)))
      (is (= 3 (cols matrix)))
      (is (= 2.0 (get-entry matrix 0 1)))
      (is (= 6.0 (get-entry matrix 1 2))))))

(deftest dot-test
  (testing "dot-product sanity"
    (let [vec1 (column [1.0 2.0 0.0] true)
          vec2 (column [1.0 0.0 2.0] true)
          vec3 (column [0.0 3.0 0.0] true)]
      (is (= 5.0 (dot vec1 vec1)))
      (is (= 1.0 (dot vec1 vec2))))))

(deftest powers-test
  (testing "powers work as expected"
    (is (= [1 2 4 8]
           (take 4 (powers 2))))
    (is (= [1 1 1 1]
           (take 4 (powers 1))))
    (is (= [1 0 0 0]
           (take 4 (powers 0))))))

(deftest unit-norm-test
  (testing "norm"
    (is (> 10e-9 (.normF (- (column [0.5 0.5 0.5 0.5] true)
                            (unit-norm (column [1 1 1 1] true))))))))

(deftest sines-test
  (testing "sines work as expected"
    (let [inv-root2 (/ 1.0 (Math/sqrt 2.0))
          expect (unit-norm (column [inv-root2 inv-root2  ;; pi/4
                                     1.0 0.0 ;; pi/2
                                     inv-root2
                                     (- inv-root2)  ;; 3pi/4
                                     0.0 -1.0] true)) ;; pi
          got (->> (/ Math/PI 4)
                   sines
                   (take 8)
                   (#(column % true))
                   unit-norm)

          sq (fn [x] (* x x))
          almost-1 (.dot expect got)]
      (is (> 10e-9
             (sq (- 1 almost-1)))))))

(deftest random-fn-test
  (testing "testing generator fns"
    (let [myfn (random-fn 3 powers [1 1 1])]
      (is (= 1 (myfn 0)))
      (is (= 3 (myfn 1)))
      (is (= 7  (myfn 2))))))

(deftest lsq-test
  (testing "basic least squares"
    (let [[dim num] [5 20]
          rand-gen (java.util.Random.)
          X (matrix (->> (take num (map #(* % 100) (repeatedly #(.nextGaussian rand-gen))))
                         (map (fn [x0] (take dim (sines x0))))) true)
          _ (do (is (= org.ejml.simple.SimpleMatrix (type X)))
                (is (= num (rows X)))
                (is (= dim (cols X))))
          b (column (take dim (repeatedly rand)) true)
          _ (do (is (= org.ejml.simple.SimpleMatrix (type b)))
                (is (= dim (rows b)))
                (is (= 1 (cols b))))
          noise (column (map #(* % 10e-6) (take num (repeatedly #(.nextGaussian rand-gen)))) true)
          y (.plus (.mult X b) noise)
          b-hat (lsq X y)]
      ;; b, b-hat must be close together
      (is (> 10e-10 (l2 b-hat b))))))

(deftest gradient-minize-test
  (testing "basic gradient minimization works (with num diff)"
    (let [n 3
          f0 (fn [x]
               (let [q (column [3 5 4] (= SimpleMatrix (type x)))]
                 (+ (dot x x)
                    (dot x q)
                    3)))
          epsilon 10e-9
          f1 (diff/vector-numdiff f0)
          x0 (column [12 -3 22] true)
          xmin (gradient-minimize f0 f1 x0 100)
          expected  (column [-1.5 -2.5 -2.0] true)]
      (is (> 10e-10 (l2 xmin expected))))))
