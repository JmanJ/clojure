(ns test_logical_structure
  (:use [logical_structure])
  (:use [clojure.test]))

(deftest constant-test
  (is (constant? (constant true)))
  (is (constant-value (constant true)))
  (is (not (constant? (variable :x))))
  )

(deftest variable-test
  (is (variable? (variable :x)))
  (is (= :x (variable-name (variable :x))))
  (is (same-variables? (variable :x) (variable :x)))
  (is (not (same-variables? (variable :x) (variable :y))))
  )

(deftest conjn-test
  (is (conjn? (conjn (variable :x) (variable :y) (variable :z))))
  (is (conjn? (conjn (disjn (variable :x) (variable :x1)) (variable :y) (variable :z))))
  )

(deftest disjn-test
  (is (disjn? (disjn (variable :x) (variable :y) (variable :z))))
  )

(deftest impl-test
  (is (impl? (impl (variable :x) (variable :y))))
  (is (impl? (impl (constant false) (variable :y))))
  )

(deftest invr-test
  (is (invr? (invr (constant true))))
  (is (not (invr? (conjn (invr (constant true)) (variable :x)))))
  )

(deftest atom-test
  (is (atom? (constant true)))
  (is (atom? (variable :x)))
  (is (not (atom? (invr (variable :x)))))
  )

(deftest primitive-test
  (is (primitive? (constant true)))
  (is (primitive? (variable :x)))
  (is (primitive? (invr (variable :x))))
  (is (not (primitive? (invr (conjn (variable :x) (constant true))))))
  )

(deftest conj_block-test
  (is (conj_block? (conjn (constant true) (variable :y) (constant false))))
  (is (not (conj_block? (conjn (constant true) (variable :y) (disjn (constant false) (variable :t))))))
  )

(deftest dnf-test
  (is (dnf? (disjn (variable :x) (conjn (constant true) (variable :y)) (constant false))))
  (is (not (dnf? (disjn (variable :x) (conjn (constant true) (variable :y)) (disjn (variable :z) (constant false))))))
  )