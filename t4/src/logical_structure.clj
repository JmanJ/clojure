(ns logical_structure)

(defn variable
  "Constructor for named variable."
  [name]
  {:pre [(keyword? name)]}
  (list :var name))

(defn variable?
  "Checking expr type for variable."
  [expr]
  (= (first expr) :var))

(defn variable-name
  "Get variable name."
  [expr]
  {:pre [(variable? expr)]}
  (second expr))

(defn same-variables?
  "Checking variables for identity."
  [v1 v2]
  (and (variable? v1) (variable? v2) (=(variable-name v1) (variable-name v2))))

(defn constant
  "Constructor for constant: true or false."
  [val]
  {:pre [(or (true? val) (false? val))]}
  (list :const val))

(defn constant?
  "Checking expr type for constant."
  [expr]
  (= (first expr) :const))

(defn constant-value
  "Get constant value."
  [expr]
  {:pre [(constant? expr)]}
  (second expr))

(defn invr
  "Constructor for inversion."
  [expr]
  (list :invr expr))

(defn invr?
  "Checking expr type for inversion."
  [expr]
  (= (first expr) :invr))

(defn conjn
  "Constructor for conjunction."
  [& exprs]
  (cons :conjn exprs))

(defn conjn?
  "Checking expr type for conjunction."
  [expr]
  (= (first expr) :conjn))

(defn disjn
  "Constructor for disjunction."
  [& exprs]
  (cons :disjn exprs))

(defn disjn? [expr]
  "Checking expr type for disjunction."
  (= (first expr) :disjn))

(defn impl
  "Constructor for implication of expressions expr1 and expr2."
  [expr1 expr2]
  (list :impl expr1 expr2))

(defn impl?
  "Checking expr type for an implication."
  [expr]
  (= (first expr) :impl))

(defn atom?
  "Checking expr type for atom: variable or constant."
  [expr]
  (or (constant? expr) (variable? expr)))

(defn primitive?
  "Checking expr type for primitive: atom or inversion of atom."
  [expr]
  (or (atom? expr) (and (invr? expr) (atom? (second expr)))))

(defn conj_block?
  "Checking expr type for clause: primitive or conjunctions of primitives."
  [expr]
  (or (primitive? expr) (and (conjn? expr) (every? primitive? (rest expr)))))

(defn dnf?
  "Checking expr type for dnf-form: conj_block or disjunction of conj_block."
  [expr]
  (or (conj_block? expr) (and (disjn? expr) (every? conj_block? (rest expr)))))