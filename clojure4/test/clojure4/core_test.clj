(ns clojure4.core-test
  (:require [clojure.test :refer :all]
            [clojure4.core :refer :all]))

(deftest test_private_functions
  (println "------------------------")
  (testing "Метод нужный при реализации дистрибутивности")
  (is (=
       (list
        (conjuction
         (variable :A)
         (variable :B)
         (variable :C))
        (conjuction
         (variable :A)
         (variable :B)
         (variable :D)))

       (append_disj_to_conj
        (conjuction
         (variable :A)
         (variable :B))
        (disjunction
         (variable :C)
         (variable :D))))))


(deftest test_no_recurion
  (println "------------------------")
  (testing "Замена импликации на +,*,`")
  (is (=
       (disjunction
        (inversion (variable :A))
        (variable :B))
       (my_dnf
        (implication
         (variable :A)
         (variable :B)))))
  (println "------------------------")
  (is (=
       (disjunction
        (variable :A)
        (variable :B))
       (my_dnf
        (implication
         (inversion (variable :A))
         (variable :B)))))

  (testing "Раскрытие отрицание от выражения")
  (println "------------------------")
  (is (=
       (conjuction
        (inversion (variable :A))
        (inversion (variable :B)))
       (my_dnf
        (inversion
         (disjunction
          (variable :A)
          (variable :B))))))
  (println "------------------------")
  (is (=
       (disjunction
        (inversion (variable :A))
        (inversion (variable :B)))
       (my_dnf
        (inversion
         (conjuction
          (variable :A)
          (variable :B))))))
  (println "------------------------")
  (testing "Избавляемся от знаков двойного отрицания")
  (is (=
       (conjuction
        (variable :A)
        (variable :B))
       (my_dnf
        (inversion       ; 2 лишних
         (inversion   ; отрицания
          (conjuction
           (variable :A)
           (variable :B)))))))
  (println "------------------------")
  (is (=
       (conjuction
        (variable :A)
        (variable :B))
       (my_dnf
        (inversion               ; 4 лишних
         (inversion           ; отрицания
          (inversion       ; 4 лишних
           (inversion   ; отрицания
            (conjuction
             (variable :A)
             (variable :B)))))))))
  (println "------------------------")
  (testing "Используем закон дистрибутивности")
  (is (=
       (disjunction
        (conjuction
         (variable :A)
         (variable :B)
         (variable :C))
        (conjuction
         (variable :A)
         (variable :B)
         (variable :D)))
       (my_dnf
        (conjuction
         (conjuction
          (variable :A)
          (variable :B))
         (disjunction
          (variable :C)
          (variable :D))))))

  (println "------------------------")
  (testing "Идемпотентность конъюнкции и дизъюнкции")
  (is (=
       (conjuction
        (variable :X)
        (inversion (variable :Y)))
       (my_dnf
        (conjuction
         (variable :X)
         (inversion (variable :Y))
         (inversion (variable :Y))))))
  (println "------------------------")
  (is (=
       (disjunction
        (variable :X)
        (inversion (variable :Y)))
       (my_dnf
        (disjunction
         (variable :X)
         (inversion (variable :Y))
         (inversion (variable :Y))))))

  (println "------------------------")
  (testing "Закон исключающего третьего")
  (is (=
       (constant 1)
       (my_dnf
        (disjunction
         (variable :X)
         (inversion (variable :X))))))

  (println "------------------------")
  (testing "Закон противоречия")
  (is (=
       (constant 0)
       (my_dnf
        (conjuction
         (variable :X)
         (inversion (variable :X)))))))


(deftest test_complex_example
  (println "------------------------")
  (testing "Составной пример с википедии")
  (is (=
       (disjunction
        (conjuction
         (variable :X)
         (inversion (variable :Y)))
        (conjuction
         (variable :X)
         (inversion (variable :Y))
         (variable :Z)))
       (my_dnf
        (inversion
         (disjunction
          (implication
           (variable :X)
           (variable :Y))
          (inversion
           (implication
            (variable :Y)
            (variable :Z)))))))))


(deftest test_const
  (println "------------------------")
  (testing "проверяем, что (1 or ... = 1)")
  (is (=
       (constant 1)
       (my_dnf
        (disjunction
         (constant 1)
         (inversion
          (disjunction
           (implication
            (variable :X)
            (variable :Y))
           (inversion
            (implication
             (variable :Y)
             (variable :Z)))))))))
  (println "------------------------")
  (testing "проверяем, что (0 and ... = 0)")
  (is (=
       (constant 0)
       (my_dnf
        (conjuction
         (constant 0)
         (inversion
          (disjunction
           (implication
            (variable :X)
            (variable :Y))
           (inversion
            (implication
             (variable :Y)
             (variable :Z))))))))))


(run-tests)