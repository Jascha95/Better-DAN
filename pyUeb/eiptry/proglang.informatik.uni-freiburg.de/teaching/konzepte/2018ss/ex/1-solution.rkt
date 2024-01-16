#lang racket
(require redex)

;; Our language
(define-language arith
  (e (op e e)
     number
     )
  (op + * -)
  )

;; Extended language with additional information to define the semantics
(define-extended-language Ev arith
  (E (op E e)
     (op v E)
     hole
     )
  (v number)
  )

;; Utility function for the definition of arithmetic operators
(define-metafunction Ev
  OP : op number ... -> number
  [(OP + number ...) ,(apply + (term (number ...)))]
  [(OP * number ...) ,(apply * (term (number ...)))]
  [(OP - number ...) ,(apply - (term (number ...)))]
  )

;; Small step
(define red
  (reduction-relation
   Ev
   #:domain e
   (--> (in-hole E (op number_1 number_2))
        (in-hole E (OP op number_1 number_2)))
   )
  )

;; Big step
(define-judgment-form Ev
  #:mode (==> I O)
  #:contract (==> e v)

  [(==> e_1 number_1)
   (==> e_2 number_2)
   -------------------------
   (==> (op e_1 e_2) (OP op number_1 number_2))]
  
  [-------------------------
   (==> v_0 v_0)]

  )

;; Various unit tests

(test-equal
 (judgment-holds (==> (+ 1 (* 4 4)) v) v)
 '(17)
 )

(test-->>
 red
 (term (+ 1 (* 2 3)))
 (term 7))

(test-->
 red
 (term (+ (- 3 1) (* 2 3)))
 (term (+ 2 (* 2 3))))

;; Property testing

; Tests that, on the given expression, big and small steps coincides.
(define (equiv-small-big? e)
  (equal?
   (judgment-holds (==> ,e v) v)
   (apply-reduction-relation* red e))
  )

; Generate expressions and check that they verify the property above.
(redex-check Ev e (equiv-small-big? (term e)))