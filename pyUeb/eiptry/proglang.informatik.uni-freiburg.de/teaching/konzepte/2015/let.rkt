#lang racket
(require redex)

;; language of aritgmetic and boolean expressions
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
      
  ;; evaluation context
  ((C D)
   hole
   (C OP2 E)
   (V OP2 C)
   (OP1 C)
   (if C then F else G)
  )
)

;; extended language
(define-extended-language let arith
  
  ;; variables
  ((x y z) variable-not-otherwise-mentioned)
 
  ;; expressions
  ((E F G) .... x (let x = E in F))
  
  ;; evaluation context
  ((C D) .... (let x = C in F))
)

;; test sntax
;(redex-match let E (term (1 + 2)))
;(redex-match let E (term xx))
;(redex-match let E (term (let x = (1 + 1) in (2 + 2))))
;(redex-match let E (term (let x = (1 + 1) in (x + 2))))
;(redex-match let E (term (let x = 1 in (x + 1))))
;(redex-match let E (term (let x = 1 in (let y = 1 in (x + y)))))

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

;;; define reduction rules for arith
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

;;; define extended reduction rules for let
(define let-reduction
  (extend-reduction-relation arith-reduction
   let
   (--> (in-hole C (let x = V in F))
        (in-hole C (subst x V F))
        Let
   )
))

(require redex/tut-subst)

(define-metafunction let
  subst : x V E -> E
  [(subst x V (if E then F else G))
   (if (subst x V E) then (subst x V F) else (subst x V G))]
  [(subst x V (let x = E in F))
   (let x = E in F)]
  [(subst x V (let y = E in F))
   (let y = (subst x V E) in (subst x V F))]
  [(subst x V (E OP2 F))
   ((subst x V E) OP2 (subst x V F))]
  [(subst x V (OP1 E))
   (OP1 (subst x V E))]
  [(subst x V x)
   V]
  [(subst x V y)
   y]
)

(define x? (redex-match let x))
  
;(traces arith-reduction (term (2 + 2)));
;(traces arith-reduction (term (true ∨ true)))
;;(traces arith-reduction (term (1 ∨ 2))) ; valid syntax, but  cannot be reduced
;(traces arith-reduction (term ((4 * 2) + 2)))
;(traces arith-reduction (term (4 < (2 + 2))))
;(traces arith-reduction (term (true ∨ (4 < (2 + 2)))))
;(traces arith-reduction (term ((1 + (1 + 2)) * (2 + 2))))
;(traces arith-reduction (term ((1 + 2) < (4 * (2 + 2)))))
;(traces arith-reduction (term (if (1 < 2) then (1 + 2) else (1 * 2))))

;(traces let-reduction (term (1 + 2)))
;(traces let-reduction (term xx))
;(traces let-reduction (term (let x = (1 + 1) in (2 + 2))))
;(traces let-reduction (term (let x = (1 + 1) in (x + 2))))
;(traces let-reduction (term (let x = 1 in (x + 1))))
;(traces let-reduction (term (let x = 1 in (let y = 1 in (x + y)))))

;(test--> arith-reduction (term (false ∧ true)) (term false))
;(test--> arith-reduction (term (false ∨ true)) (term true))
;(test--> arith-reduction (term (¬ true)) (term false))
;(test--> arith-reduction (term (¬ false)) (term true))


;(traces let-reduction (term (let x = 1 in (let x = 2 in x))))

;(test-->> let-reduction
;  (term (let y = 1 in (let y = 2 in y)))
;  2
;  )
