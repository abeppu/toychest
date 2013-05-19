(ns toychest.differentiation-test
  (:refer-clojure :exclude [+ - * /])
  (:use toychest.arithmetic clojure.test toychest.differentiation))



(deftest univariate-autodiff-test
  (testing "can do basic autodiff on functions of a single variable"
    (let [f (fn [x] (* (- x 3) (+ x 4))) ;; p1(x) = x^2 + x - 12
          f1 (f 1.0) ;;Dy/dx x^2 + x - 12 = 2x + 1
          d1 ((univariate-autodiff f) 1.0)]
      (is (= f1 -10.0))
      (is (= d1 3.0))))
  (testing "can do autodiff even where things aren't written like ordinary functions"
    (let [f (fn [x]
              (loop [curr x iter 0]
                (if (> iter 10)
                  curr
                  (recur (+ x (* x (sin x))) (inc iter)))))
          f1 (f 13.02)
          d1 ((univariate-autodiff f) 13.02)
          d2 ((univariate-numdiff f) 13.02)]
      (is (> 10e-6 (Math/abs (- d1 d2)))))))