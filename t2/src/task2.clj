(ns task2)

(defn trapezoid [f x1 x2]
  "Calculate  part of integral from x1 and x2 using trapezoid rule"
  (*
    (/
      (+ (f x1) (f x2))
      2)
    (Math/abs (- x2 x1))))

(defn calculate_steps [f, steps, delta]
  "calculate integral on all steps (with loop-recur) and return result like summ (integral without remainder)"
  (loop [cur_step steps
         cur_sum 0]
    (if (zero? cur_step)
      cur_sum
      (let [x2 (* cur_step delta)
            x1 (- x2 delta)]
        (recur (dec cur_step) (+ cur_sum (trapezoid f x1 x2)))
        )
      )
    )
  )

(defn integrate [f, delta]
  "Return function that calculate integral f(x) with delta, where x - input value of returned function"
  (let [f_memo (memoize f)]
    (fn [x]
      (let [steps_count (quot x delta)
            remainder (mod x delta)]
          (+
            (calculate_steps f_memo steps_count delta)
            (trapezoid f_memo (- x remainder) x))
        ))))


(time ((integrate (fn [x] (* x x 2)) 0.1) 5))
(time ((integrate (fn [x] (* x x 2)) 0.1) 5))