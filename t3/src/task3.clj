(ns task3)

(defn trapezoid [f x1 x2]
  "Calculate  part of integral from x1 and x2 using trapezoid rule"
  (*
    (/
      (+ (f x1) (f x2))
      2)
    (Math/abs (- x2 x1))))

(defn integrate [f, delta]
  "Return function that calculate integral f(x) with delta, where x - input value of returned function"
  (defn trapezoid_by_pos [f pos]
    "Calculate  part of integral from pos and (pos-1) using trapezoid rule"
    (*
      (/
        (+ (f (* (dec pos) delta)) (f (* pos delta)))
        2)
      delta))
  (let [f_memo (memoize f)
        f_generator (map first (iterate
                                 #(identity [(+ (first %) (trapezoid_by_pos f_memo (second %))) (inc (second %))])
                                 [0 1]))]
    (fn [x]
      (let [func_seq f_generator
             steps_count (quot x delta)
            remainder (mod x delta)]
        (+
          (nth f_generator steps_count)
          (trapezoid f_memo (- x remainder) x))
        )
      )
    )
  )
