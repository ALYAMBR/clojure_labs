(ns clojure4.core
  (:gen-class))

; TODO constants and variables
(defn variable
  [name]
  {:pre [(keyword? name)]}
  (list ::var name)
  )

(defn variable?
  [expr]
  (= (first expr) ::var))

(defn variable-name 
  [variable]
  (second variable))

(defn same-variables?
  [var1 var2]
  (and
   (variable? var1)
   (variable? var2)
   (= (variable-name var1)
      (variable-name var2))))

; TODO make constants

(defn get-value
  [logical-expr]
  ; TODO calculate value
  )

; TODO logical operators
(defn disjunction
  [expr & rest]
  (cons ::disjunction (cons expr rest)))

(defn disjunction?
  [expr]
  (= ::disjunction (first expr)))

(defn conjuction
  [expr & rest]
  (cons ::conjuction (cons expr rest)))

(defn conjuction?
  [expr]
  (= ::conjuction (first expr)))

(defn implication
  [expr & rest]
  (cons ::implication (cons expr rest)))

(defn implication?
  [expr]
  (= ::implication (first expr)))

(defn rejection
  [expr]
  (cons ::rejection expr))

(defn rejection?
  [expr]
  (= ::rejection (first expr)))

(defn calc-disjunction
  [x1 x2]
  (or (get-value x1) (get-value x2)))

(defn calc-conjuction
  [x1 x2]
  (and (get-value x1) (get-value x2)))

(defn calc-rejection
  [x]
  (not (get-value x)))

(defn calc-implication
  [x1 x2]
  (or (not (get-value x1)) (get-value x2)))

; TODO simplification rules



(def C1 (constant true))
println(C1)