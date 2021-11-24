(ns clojure4.core
  (:gen-class))

(defn print_expr [expr]
  (def s0 (str expr))
  (def s1 (clojure.string/replace s0 #":clojure4.core\/" ""))
  (def s2 (clojure.string/replace s1 #"\(const (\d+)\)" "$1"))
  (def s3 (clojure.string/replace s2 #"\(var :([a-zA-Z]+)\)" "$1"))
  (println s3))

; constants
(defn constant [num]
  {:pre [(number? num)]}
  (list ::const num))

(defn constant? [expr]
  (= (first expr) ::const))

(defn constant-value [v]
  (second v))

(defn same-constants? [v1 v2]
  (and
   (constant? v1)
   (constant? v2)
   (= (constant-value v1)
      (constant-value v2))))

; variables
(defn variable [name]
  {:pre [(keyword? name)]}
  (list ::var name))

(defn variable? [expr]
  (= (first expr) ::var))

(defn variable-name [v]
  (second v))

(defn same-variables? [v1 v2]
  (and
   (variable? v1)
   (variable? v2)
   (= (variable-name v1)
      (variable-name v2))))

(defn in?
  "true if collection contains element"
  [coll elem]
  (some #(= elem %) coll))

; disjunction
(defn disjunction 
  ([expr & rest]
   (cons ::disj (cons expr rest)))
  ([rest]
   (cons ::disj rest)))

; check a type for disjunction
(defn disjunction? [expr]
  (= ::disj (first expr)))

; conjuction
(defn conjuction
  ([rest]
   (cons ::conj rest))
  ([expr & rest]
   (cons ::conj (cons expr rest))))

; check a type for conjuction
(defn conjuction? [expr]
  (= ::conj (first expr)))

; list of expr arguments
(defn args [expr]
  (rest expr))

; inversed expr
(defn inversion [expr]
  (list ::not expr))
; check a type for an inversed expr
(defn inversion? [expr]
  (= ::not (first expr)))
(defn single_inversion? [expr]
  (and
     (inversion? expr)
     (not (conjuction? (second expr)))
     (not (disjunction? (second expr)))))
; implication
(defn implication [v1 & v2]
  (cons ::impl (cons v1 v2)))
; check a type for an implication 
(defn implication? [expr]
  (= ::impl (first expr)))

; check rules:
(defn inversed_expr? [expr]
  (and
   (inversion? expr)
   (or
    (conjuction? (second expr)) 
    (disjunction? (second expr))
    )))

(defn double_inversion? [expr]
  (and
   (inversion? expr)
   (inversion? (second expr))))

(defn distributable? [expr]
  (and
   (conjuction? expr)
   (> (count
       (filter
        #(= ::disj (first %))
        (args expr)))
      0)))

; utils
(defn concat_conj [expr & rest]
  (let [concated_expr (cons expr rest)]
    (if (= 1 (count concated_expr))
      (first concated_expr)
      (cons ::conj concated_expr))))


(defn concat_disj [expr & rest]
  (let [concated_expr (cons expr rest)]
    (if (= 1 (count concated_expr))
      (first concated_expr)
      (cons ::disj concated_expr))))


(defn append_disj_to_conj [expr disj_list]
  (let [top_operation (first expr)
        expr_args (case top_operation
                    ::not (list expr)
                    ::impl expr
                    ::disj (args expr)
                    ::conj (args expr)
                    (list expr))
        disj_list_args (if (variable? disj_list)
                         disj_list
                         (args disj_list))
        result (map
                #(apply conjuction (concat expr_args (list %)))
                disj_list_args)]
    result))


(declare dnf)
(declare my_dnf)

; transformation rules
(def dnf_rules
  (list
        ; replace all exotic operations with disj, conj, inversion.
   [(fn [expr] (implication? expr))
    (fn [expr]
      (let [first-arg (inversion (first (args expr)))
            second-arg (second (args expr))
            result (disjunction
                    first-arg
                    second-arg)]
        result))]

        ; put inversion inside to atomic logic parts
   [(fn [expr] (inversed_expr? expr))
    (fn [expr](if (conjuction? (second expr))
          (apply disjunction
                 (map
                  inversion
                  (args (second expr))))

          (apply conjuction
                 (map
                  inversion
                  (args (second expr))))))]

        ; remove double inversion
   [(fn [expr] (double_inversion? expr))
    (fn [expr] (second (second expr)))]

        ; distribution
   [(fn [expr] (distributable? expr))
    (fn [expr]
      (let [disj_list (first
             (filter
              #(= ::disj (first %))
              (args expr)))
            
            other (remove
             #(= disj_list %)
             (args expr))
            
            result (apply 
                    disjunction 
                    (first 
                     (map
                      #(append_disj_to_conj % disj_list)
                      other)))]
        result))]

        ; stop of reccursion
   [(fn [expr] (constant? expr))
    (fn [expr] expr)]

   [(fn [expr] (variable? expr))
    (fn [expr] expr)]


   [(fn [expr] (single_inversion? expr))
    (fn [expr]
      (let [result (inversion (dnf (second expr)))]
        result))]

   [(fn [expr] (inversion? expr))
    (fn [expr] expr)]


   [(fn [expr] (conjuction? expr))
    (fn [expr]
      (let [new_expr (distinct expr)
            contain_inversion
            (boolean
             (some
              #(in? (args new_expr) (inversion %))
              (args new_expr)))
            contain_false
            (boolean
             (in?
              (args new_expr)
              (constant 0)))]
        (if (or
             contain_inversion
             contain_false)
          (constant 0)
          (apply  concat_conj
                  (map
                   #(dnf %)
                   (args new_expr))))))]

   [(fn [expr] (disjunction? expr))
    (fn [expr]
      (let [new_expr (distinct expr)
            contain_inversion
            (boolean
             (some
              #(in? (args new_expr) (inversion %))
              (args new_expr)))
            contain_true
            (boolean
             (in?
              (args new_expr)
              (constant 1)))]
        (if (or
             contain_inversion
             contain_true)
          (constant 1)
          (apply  concat_disj
                  (map
                   #(dnf %)
                   (args new_expr))))))]))


(defn dnf [expr]
    (print_expr expr)
  ((some (fn [rule]
           (if ((first rule) expr)
             (second rule)
             false))
         dnf_rules)
   expr))


(defn my_dnf [expr]
  (let [prev expr curr (dnf expr)]
    (print_expr expr)
    (if (= prev curr)
      curr
      (my_dnf curr))))