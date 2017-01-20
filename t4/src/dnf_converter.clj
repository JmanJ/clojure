(ns dnf_converter
  (:use [logical_structure])
  )

(defn express_implic
  "Express all implications in logical expression expr."
  [expr]
  (if (impl? expr)
    (disjn (invr (express_implic (nth expr 1))) (express_implic (nth expr 2)))
    (if (or (constant? expr) (variable? expr))
      expr
      (cons (first expr) (map express_implic (rest expr)))
      )
    )
  )

(defmulti express_inversion
  "Express logical inversions by De Morgans rule and reduce double.
  Implementation is polymorphic."
  (fn [expr]
    (if (invr? expr)
      (first (second expr))
      :default
      )
    )
  )

(defmethod express_inversion :default [expr]
  "expr not inversion"
  (if (or (variable? expr) (constant? expr))
    expr
    (cons (first expr) (map express_inversion (rest expr)))
    )
  )

(defmethod express_inversion :invr [expr]
  "invr (invr expr) = expr"
  (express_inversion (second (second expr)))
  )

(defmethod express_inversion :disjn [expr]
  "invr (disjn expr) = conjn (invr expr)"
  (apply conjn (map #(express_inversion (invr %)) (rest (second expr))))
  )


(defmethod express_inversion :conjn [expr]
  "invr (conjn expr) = disjn (invr expr)"
  (apply disjn (map #(express_inversion (invr %)) (rest (second expr))))
  )

(defmethod express_inversion :var [expr]
  "invr var = invr var"
  expr)

(defmethod express_inversion :const [expr]
  "invr const (true false) = const (false true)"
  (if (true? (second (second expr)))
    (constant false)
    (constant true)
    )
  )

(defn associative_law
  "Moves conjunction or disjunction in expression on the higher level possible.
  conjn (a conjn (b conjn (c))) = conjn (a b c) , or disjn (a disjn (b disjn (c disjn (d)))) = disjn (a b c d).
  Predicate: conjn? or disjn?."
  [predicate expr]
  (if (atom? expr)
    expr
    (let [child_expressions (map (partial associative_law predicate) (rest expr))
           kwrd (first expr)]
      (if (predicate expr)
        (cons kwrd (reduce (fn [exp_type exp]
                             (if (predicate exp)
                               (concat exp_type (rest exp))
                               (concat exp_type (list exp))
                               )
                             )
                     `() child_expressions))
        expr
        )
      )
    )
  )

(defn distributive_law
  "Applies the distributive law for conjuctions and make a disjunction of several conjunctions.
  conjn (a disjn (b c)) = disjn (conjn (a b) conjn (a c))"
  [expr]
  (let [disjuction (first (filter disjn? (rest expr)))
        conj_part (remove (fn [x] (= x disjuction)) (rest expr))]
    (apply disjn (map (fn [x] (apply conjn x conj_part)) (rest disjuction)))
    )
  )

(defn express_conjn [expr]
  "Express conjunctions until given formula becomes dnf-formula."
  (if (dnf? expr)
    expr
    (express_conjn
      (if (primitive? expr)
          expr
          (if (conjn? expr)
            (if (every? primitive? (rest expr))
              expr
              (associative_law conjn? (distributive_law expr)))
            (let [wrong_conjn (first (filter                 ;disjn with conjn inside wich contains not all primitive
                                      (fn [x] (and (conjn? x) (not (every? primitive? (rest x)))))
                                      (rest expr)))
                  the_rest (remove (fn [x] (= x wrong_conjn)) (rest expr))]
              (associative_law disjn? (associative_law conjn?
                                 (apply disjn (distributive_law wrong_conjn) the_rest)))
              )
            )
        )
      )
    )
  )

(defn dnf [expr]
  "Converts any logical expression with constants, variables, conjunctions,
  disjunctions, inversions and implications to dnf-form."
  (express_conjn (express_inversion (express_implic expr))))

(defn assign_value
  "Assigns some fixed value to variable ``name'' in expression expr."
  [name value expr]
  {:pre [(and (keyword? name) (or (true? value) (false? value)))]}
  (if (and (variable? expr) (= name (variable-name expr)))
    (constant value)
    (if (atom? expr)
      expr
      (cons (first expr) (map (partial assign_value name value) (rest expr))))
    )
  )