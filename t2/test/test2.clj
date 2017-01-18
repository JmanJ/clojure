(ns test2
  (:use [task2])
  (:use [clojure.test]))

(defn compare_f [expected actual eps]
  (and
    (> actual (- expected eps))
    (< actual (+ expected eps))
    ))

(letfn [(f [x] x)]
  (deftest trapezoid_test
    (is (= 1/2 (trapezoid f 0 1)))
    (is (= (float 3/8) (trapezoid f 0.5 1)))
    (is (= (float 3/8) (trapezoid f 1 0.5)))
    ))

(let [eps 0.1
      f (integrate (fn [x] 1) 0.1)]
  (deftest const_function
    (is (function? f))
    (is (compare_f 0 (f 0) eps))
    (is (compare_f 50 (f 50) eps))
    (is (compare_f 51 (f 51) eps))
    (is (compare_f 5000 (f 5000) eps))
    )
  )

(let [eps 0.1
      f (integrate (fn [x] (* x x 2)) 0.1)]
  (deftest composite_function
    (is (compare_f 0 (f 0) eps))
    (is (compare_f 18 (f 3) eps))
    )
  )
