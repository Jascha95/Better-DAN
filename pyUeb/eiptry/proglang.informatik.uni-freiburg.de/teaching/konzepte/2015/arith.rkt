#lang racket
(require redex)

;; language of arith
(define-language arith
  
  ;; number values
  (I integer)
  
  ;; boolean values
  (B true false)
  
  ;; values
  ((U V W) I B)

  ;; arithemtic operators
  (OP1 ¬)
  (OP2 + - * < / ∧ ∨)
  
  ;; expressions
  ((E F G)
   V
   (OP1 E)
   (E OP2 F)
   (if E then F else G)
  )
      
  ((C) ;; evaluation context
   hole
   (C OP2 E)
   (V OP2 C)
   (OP1 C)
   (if C then F else G)
  )
)

;; test sntax

;(redex-match arith E (term (1 + 2)))
;;(redex-match arith E (term (1 ∧ 2))) ; not valid, but correct syntax
;(redex-match arith E (term (1 < 2)))
;(redex-match arith E (term (true ∧ false)))
;(redex-match arith E (term (¬ true)))

;(redex-match arith E (term ((1 + (1 + 2)) * (2 + 2))))
;(redex-match arith E (term (if (1 < 2) then (1 + 2) else (1 * 2))))

;(redex-match arith (in-hole C (4 * E)) (term (4 * (2 + 2))))
;(redex-match arith (in-hole C (1 + E)) (term ((1 + 2) < (4 * (2 + 2)))))
;(redex-match arith (in-hole C E) (term (if (1 < 2) then (1 + 2) else (1 * 2))))

;; contract transformations

(define-metafunction arith
  [(δ (I_1 + I_2)) ,(+ (term I_1) (term I_2))]
  [(δ (I_1 - I_2)) ,(- (term I_1) (term I_2))]
  [(δ (I_1 * I_2)) ,(* (term I_1) (term I_2))]
  [(δ (I_1 / I_2)) ,(/ (term I_1) (term I_2))]
  [(δ (I_1 < I_2)) ,(if (< (term I_1) (term I_2)) (term true) (term false))]
  [(δ (B_1 ∧ B_2)) ,(if (eq? (term B_1) (term true)) (term B_2) (term false))]
  [(δ (B_1 ∨ B_2)) ,(if (eq? (term B_1) (term true)) (term true) (term B_2))]
  [(δ (¬ B_1)) ,(if (eq? (term B_1) (term true)) (term false) (term true))]
)

(define arith-reduction
  (reduction-relation
   arith
   (--> (in-hole C (OP1 V))
        (in-hole C (δ (OP1 V)))
        δ1
   )
   (--> (in-hole C (V OP2 W))
        (in-hole C (δ (V OP2 W)))
        δ2
   )
   (--> (in-hole C (if true then F else G))
        (in-hole C F)
        If-true
   )
   (--> (in-hole C (if false then F else G))
        (in-hole C G)
        If-false
   )
))

;(traces arith-reduction (term (2 + 2)));
;(traces arith-reduction (term (true ∨ true)))
;;(traces arith-reduction (term (1 ∨ 2))) ; valid syntax, but  cannot be reduced
;(traces arith-reduction (term ((4 * 2) + 2)))
;(traces arith-reduction (term (4 < (2 + 2))))
;(traces arith-reduction (term (true ∨ (4 < (2 + 2)))))
;(traces arith-reduction (term ((1 + (1 + 2)) * (2 + 2))))
;(traces arith-reduction (term ((1 + 2) < (4 * (2 + 2)))))
;(traces arith-reduction (term (if (1 < 2) then (1 + 2) else (1 * 2))))