(ns test1
  (:use [task1])
  (:use [clojure.test]))

(deftest test_digits
  (is (= `("") (get_multi_list [1 2 3] 0)))
  (is (= `#{[1] [2] [3]} (into #{} (get_multi_list [1 2 3] 1))))
  (is (= `#{[1 2] [1 3] [2 1] [2 3] [3 1] [3 2]} (into #{} (get_multi_list [1 2 3] 2))))
  )

(deftest get_multi_list_aaa
  (is (= `("") (get_multi_list ["aaa"] 0)))
  (is (= `#{["aaa"]} (into #{} (get_multi_list ["aaa"] 1))))
  (is (= `() (get_multi_list ["aaa"] 2)))
  )

(deftest test_dif_obj
  (is (= `("") (get_multi_list [0 "a" 2] 0)))
  (is (= `#{[0] ["a"] [2]} (into #{} (get_multi_list [0 "a" 2] 1))))
  (is (= `#{[0 "a"] [0 2] ["a" 0] ["a" 2] [2 0] [2 "a"]} (into #{} (get_multi_list [0 "a" 2] 2))))
  (is (= `#{[1 false 1] [1 false "a"] [1 "a" 1] [1 "a" false] [false 1 false] [false 1 "a"] [false "a" 1]
            [false "a" false] ["a" 1 false] ["a" 1 "a"] ["a" false 1] ["a" false "a"]} (into #{} (get_multi_list [1 false "a"] 3))))
  )

(deftest empty_list
  (is (= `("") (get_multi_list [] 0)))
  (is (= `() (get_multi_list [] 1)))
  (is (= `() (get_multi_list [] 2)))
  )