#lang racket
(require redex)

;; The base language
(define-language arith
  ; Expressions
  (e (+ e e)
     number
     )
  ; Evaluation contexts
  (E (+ E e)
     (+ v E)
     hole
     )
  ; Values
  (v number)
  )

;; Small step relation reduction.
(define red
  (reduction-relation
   arith
   #:domain e
   (--> (in-hole E (+ v_1 v_2))
        (in-hole E ,(apply + (term (v_1 v_2))))
        ; Apply the "+" racket function to the terms v_1 and v_2
        )
   ))

;; Make these tests pass.

;(test-->>
;   red
;   (term (+ 1 (* 2 3)))
;   (term 7))
;
;(test-->
;   red
;   (term (+ (- 3 1) (* 2 3)))
;   (term (+ 2 (* 2 3))))