#lang racket
(require redex)

#|

Values      v   ::= number
Expressions e,f ::= e + f | e * f

|#

;; language of arith
(define-language arith

  ;; values
  (I integer)
  
  ;; expressions
  ((E F) I (E + F) (E * F))
  
  ;; evaluation contexts
  (C hole (C + E) (I + C) (C * E) (I * C))
)

(redex-match arith E (term (1 + 2)))
(redex-match arith E (term (2 * (1 + 2))))
(redex-match arith (in-hole C (4 * E)) (term ((4 * 2) + 2)))

(define red
 (reduction-relation arith
 (--> (in-hole C (I_0 + I_1))
      (in-hole C ,(+ (term I_0) (term I_1)))
      Plus
  )
 (--> (in-hole C (I_0 * I_1))
      (in-hole C ,(* (term I_0) (term I_1)))
      Times
  )
 )
)

;(traces red (term (2 + 2)))
;(traces red (term ((1 + (1 + 2)) * (2 + 2))))

(test-->> red 
          (term ((1 + (1 + 2)) * (2 + 2)))
          (term 16)
)

(test--> red 
          (term ((1 + (1 + 2)) * (2 + 2)))
          (term ((1 + 3) * (2 + 2)))
)