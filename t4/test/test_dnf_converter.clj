(ns test_dnf_converter
  (:use [logical_structure])
  (:use [dnf_converter])
  (:use [clojure.test]))

(deftest express_implic_test
  (is (= (disjn (invr (variable :x)) (variable :y)) (express_implic (impl (variable :x) (variable :y)))))
  (is (= (invr (disjn (invr (variable :x)) (variable :y))) (express_implic (invr (impl (variable :x) (variable :y))))))
  )

(deftest express_inversion_test
  (is (= (constant true) (express_inversion (constant true))))
  (is (= (disjn (variable :x) (variable :x)) (express_inversion (disjn (variable :x) (variable :x)))))
  (is (= (conjn (variable :x) (variable :x)) (express_inversion (conjn (variable :x) (variable :x)))))
  (is (= (conjn (variable :x) (variable :x) (variable :y)) (express_inversion (conjn (variable :x) (variable :x) (variable :y)))))

  (is (= (constant false) (express_inversion (invr (constant true)))))
  (is (= (constant true) (express_inversion (invr (invr (constant true))))))
  (is (= (constant true) (express_inversion (invr (constant false)))))

  (is (= (variable :x) (express_inversion (invr (invr (variable :x))))))
  (is (= (invr (variable :x)) (express_inversion (invr (invr (invr (variable :x)))))))
  (is (= (variable :x) (express_inversion (invr (invr (invr (invr (variable :x))))))))
  (is (= (variable :x) (express_inversion (variable :x))))

  (is (= (disjn (variable :x) (variable :x)) (express_inversion (invr (conjn (invr (variable :x)) (invr (variable :x)))))))
  (is (= (conjn (variable :x) (variable :x)) (express_inversion (invr (disjn (invr (variable :x)) (invr (variable :x)))))))

  (is (= (disjn (invr (variable :x)) (variable :y)) (express_inversion (invr (conjn (variable :x) (invr (variable :y)))))))
  (is (= (conjn (invr (variable :x)) (variable :y)) (express_inversion (invr (disjn (variable :x) (invr (variable :y)))))))

  (is (= (conjn (invr (variable :x)) (disjn (invr (variable :y)) (variable :z))) (express_inversion (invr (disjn (variable :x) (conjn (variable :y) (invr (variable :z))))))))

  )

(deftest associative_law_test
  (is (= (conjn (invr (variable :x)) (invr (variable :y)) (variable :z)) (associative_law conjn? (conjn (invr (variable :x)) (conjn (invr (variable :y)) (variable :z))))))
  (is (= (disjn (invr (variable :x)) (invr (variable :y)) (variable :z)) (associative_law disjn? (disjn (invr (variable :x)) (disjn (invr (variable :y)) (variable :z))))))
  (is (= (conjn (invr (variable :x)) (invr (variable :y)) (variable :z1) (variable :z2)) (associative_law conjn? (conjn (invr (variable :x)) (conjn (invr (variable :y)) (conjn (variable :z1) (variable :z2)))))))
  )

(deftest distributive_law_test
  (is (=
        (disjn (conjn (variable :y) (variable :x) (variable :w)) (conjn (variable :z) (variable :x) (variable :w)))
        (distributive_law (conjn (variable :x) (disjn (variable :y) (variable :z)) (variable :w)))
        ))
  (is (=
        (disjn (conjn (variable :y) (variable :x) (variable :w)) (conjn (variable :z) (variable :x) (variable :w)) (conjn (variable :v) (variable :x) (variable :w)))
        (distributive_law (conjn (variable :x) (disjn (variable :y) (variable :z) (variable :v)) (variable :w)))
        ))
  )

(deftest express_conjn_test
  (is (= (disjn (conjn (variable :a) (variable :a) (variable :d)) (conjn (variable :b) (variable :a) (variable :d)) (conjn (variable :c) (variable :a) (variable :b)) (conjn (variable :d) (variable :a) (variable :b)))
        (express_conjn (disjn (conjn (variable :a) (variable :b) (disjn (variable :c) (variable :d))) (conjn (variable :a) (variable :d) (disjn (variable :a) (variable :b)))))))
  )

(deftest dnf_test
  (is (=
        (disjn
          (conjn
            (variable :a) (variable :e) (variable :a))
          (conjn
            (variable :b) (variable :e) (variable :a))
          (conjn
            (variable :a) (invr (variable :d)) (variable :a))
          (conjn
            (variable :b) (invr (variable :d)) (variable :a))
          (conjn
            (variable :c) (constant true) (variable :a))
          (conjn
            (variable :d) (constant true) (variable :a))
          (conjn
            (variable :c) (invr (variable :b)) (variable :a))
          (conjn
            (variable :d) (invr (variable :b)) (variable :a)))

        (dnf (disjn
               (conjn
                 (variable :a)
                 (impl (variable :b) (constant true))
                 (disjn
                   (variable :c)
                   (variable :d)))
               (conjn
                 (variable :a)
                 (invr (conjn (variable :d) (invr (variable :e))))
                 (disjn
                   (variable :a)
                   (variable :b)))))))
  (is (= (constant true) (dnf (constant true))))

  )

(deftest assign_value_test
  (is (= (constant true) (assign_value :x true (variable :x))))
  (is (= (conjn (constant true) (constant true)) (assign_value :x true (conjn (constant true) (variable :x)))))
  (is (= (variable :y) (assign_value :x true (variable :y))))
  )