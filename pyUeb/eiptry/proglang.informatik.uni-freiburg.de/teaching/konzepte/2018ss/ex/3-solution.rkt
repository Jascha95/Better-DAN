#lang racket
(require redex)
(require "subst.rkt")
(provide cbv cbn red n)

;; Our language
(define-language lambda
  (x variable-not-otherwise-mentioned)
  (op + - * /)
  (c number
     op
     (δ op number)
     )
  (v c
     (λ x e)
     )
  (e v
     x
     (e e)
     )
  (E (E e)
     (v E)
     hole
     )
  (E~ (E~ e)
      (c E~)
      hole
      )
  (E* (E* e)
      (e E*)
      hole
      )
  )

;; Utility function for the definition of arithmetic operators
(define-metafunction lambda
  delta : c e -> c
  [(delta op number) (δ op number)]
  [(delta (δ + number_0) number_1) ,(apply + (term (number_0 number_1)))]
  [(delta (δ - number_0) number_1) ,(apply - (term (number_0 number_1)))]
  [(delta (δ * number_0) number_1) ,(apply * (term (number_0 number_1)))]
  [(delta (δ / number_0) number_1) ,(apply / (term (number_0 number_1)))]
  )

(define cbv
  (reduction-relation
   lambda
   #:domain e
   (--> (in-hole E ((λ x_0 e_0) v_0))
        (in-hole E (subst (x_0 v_0) e_0))
        "β")
   (--> (in-hole E (c_0 v_0))
        (in-hole E (delta c_0 v_0))
        "δ")
   ))

(define cbn
  (reduction-relation
   lambda
   #:domain e
   (--> (in-hole E~ ((λ x_0 e_0) e_1))
        (in-hole E~ (subst (x_0 e_1) e_0))
        "β")
   (--> (in-hole E~ (c_0 v_0))
        (in-hole E~ (delta c_0 v_0))
        "δ")
   ))

(define red
  (reduction-relation
   lambda
   #:domain e
   (--> (in-hole E* ((λ x_0 e_0) e_1))
        (in-hole E* (subst (x_0 e_1) e_0))
        "β")
   (--> (in-hole E* (c_0 v_0))
        (in-hole E* (delta c_0 v_0))
        "δ")
   ))

(define n
  (term ((λ x (+ x)) ((+ 2) 1))))
