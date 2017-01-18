(ns task1)

(defn get_multi_list
  [input_list n]
  (defn construct_word_list [current_result, current_word]
    (if (empty? current_word)
      (map #(conj [%]) input_list)
      (do
        (let
          [alphas (filter #(not (= % (last current_word))) input_list)
           new_words (map #(flatten (conj [current_word] [%])) alphas)]
          (concat current_result new_words)
          )
        )
      )
    )
    (reduce
      (fn [result_list _]
        (reduce construct_word_list `() result_list)
        )
      `("")
      (range 0 n))
  )
