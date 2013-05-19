(ns toychest.arithmetic-test
  (:refer-clojure :exclude [+ - * /])
  (:use toychest.arithmetic clojure.test)
  (:import [toychest.arithmetic Complex]
           [toychest.arithmetic DualNumber]))

(deftest real-add-test
  (testing "real addition"
    (let [a 3.0
          b -4.0
          c (+ a b)]
      (is (= Double (type c)))
      (is (= -1.0 c)))))

(deftest long-add-test
  (testing "adding longs"
    (is (= 5 (+ 3 2)))))

(deftest complex-add-test
  (let [a (Complex. 3.0 2.0)
        b (Complex. -4.0 1.0)]
    (testing "complex addition"
      (let [c (+ a b)
            d 4.0
            e (+ a d)]
        (is (= Complex (type c)))
        (is (= -1.0 (.real c)))
        (is (= 3.0 (.imag c)))
        (is (= Complex (type e)))
        (is (= 7.0 (.real e)))
        (is (= 2.0 (.imag e)))))))

(deftest dual-add-test
  (let [a (DualNumber. 1.0 3.0)
        b (DualNumber. 3.0 5.0)]
    (testing "dual number addition"
      (let [c (+ a b)]
        (is (= DualNumber (type c)))
        (is (= 4.0 (.real c)))
        (is (= 8.0 (.nilpotent c)))))
    (testing "dual number subtraction"
      (let [c (- a b)]
        (is (= DualNumber (type c)))
        (is (= -2.0 (.real c)))
        (is (= -2.0 (.nilpotent c)))))
    (testing "dual number multiplication"
      (let [c (* a b)]
        (is (= DualNumber (type c)))
        (is (= 3.0 (.real c)))
        (is (= 14.0 (.nilpotent c)))))))





;; TODO test other operations
