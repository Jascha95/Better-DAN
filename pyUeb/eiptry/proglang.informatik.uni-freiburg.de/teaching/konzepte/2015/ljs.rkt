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
  (Op1 BoolOp1)
  (BoolOp1 ¬)
  
  (Op2 NumOp2 BoolOp2)
  (NumOp2 + - * /)
  (BoolOp2 < ∧ ∨)
  
  ;; expressions
  ((E F G)
   V
   (Op1 E)
   (E Op2 F)
   (if E then F else G)
  )
      
  ;; evaluation context
  ((C D)
   hole
   (C Op2 E)
   (V Op2 C)
   (OP1 C)
   (if C then F else G)
  )
)

;; extended contracts with evaluation context
(define-extended-language let arith
  
  ;; variables
  ((x y z) variable-not-otherwise-mentioned)
 
  ;; expressions
  ((E F G) .... x (let x = E in F))
  
  ;; evaluation context
  ((C D) .... (let x = C in F))
)

(define-extended-language errlet let
 
  ;; values
  ((U V W) .... NaN Udef)
  
  ;; behavior
  ((R S T) V Err)
)

(define-extended-language ljs errlet
 
  ;; values
  ((U V W) .... (λ x E))
  
  ;; expressions
  ((E F G) .... (E F))
  
   ;; evaluation context
  ((C D) .... (C F) (V C))
)

;; test sntax

;(redex-match let E (term (1 + 2)))
;(redex-match let E (term xx))
;(redex-match let E (term (let x = (1 + 1) in (2 + 2))))
;(redex-match let E (term (let x = (1 + 1) in (x + 2))))
;(redex-match let E (term (let x = 1 in (x + 1))))
;(redex-match let E (term (let x = 1 in (let y = 1 in (x + y)))))

;(redex-match errlet (in-hole C (4 * E)) (term (4 * (2 + 2))))
;(redex-match errlet (in-hole C (E + 1)) (term (NaN + 1)))

;(redex-match errlet E (term NaN))
;(redex-match errlet E (term Udef))

;(redex-match errlet E (term (NaN + 1)))

;(redex-match ljs E (term (λ x (1 + 2))))
;(redex-match ljs E (term (λ x (x + 1))))
;(redex-match ljs E (term (λ x (λ y (x + y)))))

;(redex-match ljs E (term ((λ x (1 + 2)) 1)))
;(redex-match ljs E (term ((λ x (x + 1)) 1)))
;(redex-match ljs E (term (((λ x (λ y (x + y))) 1) 1)))

;(redex-match ljs E (term (let plus = (λ x (λ y (x + y))) in ((plus 1) 1))))

;; contract transformations

(require redex/tut-subst)

(define-metafunction ljs
  subst : x V E -> E
  [(subst x V (if E then F else G))
   (if (subst x V E) then (subst x V F) else (subst x V G))]
  [(subst x V (λ x E))
   (λ x E)]
  [(subst x V (λ y E))
   (λ y (subst x V E))]
  [(subst x V (E F))
   ((subst x V E) (subst x V F))]
  [(subst x V (let x = E in F))
   (let x = E in F)]
  [(subst x V (let y = E in F))
   (let y = (subst x V E) in (subst x V F))]
  [(subst x V (E Op2 F))
   ((subst x V E) Op2 (subst x V F))]
  [(subst x V (Op1 E))
   (OP1 (subst x V E))]
  [(subst x V x)
   V]
  [(subst x V y)
   y]
   [(subst x V W)
   W]
)

;(define-metafunction ljs
;  beta : x V E -> E
;  [(beta x V E)
;   ,(subst/proc x? (list (term x)) (list (term V)) (term E))])

(define x? (redex-match let x))

(define-metafunction arith
  [(δNum (I_1 + I_2)) ,(+ (term I_1) (term I_2))]
  [(δNum (I_1 - I_2)) ,(- (term I_1) (term I_2))]
  [(δNum (I_1 * I_2)) ,(* (term I_1) (term I_2))]
  [(δNum (I_1 / I_2)) ,(/ (term I_1) (term I_2))]  
  [(δNum (I_1 < I_2)) ,(if (< (term I_1) (term I_2)) (term true) (term false))]
)

(define-metafunction errlet
  [(δNum/ (I_1 / 0)) NaN] ;; error rule
  [(δNum/ (I_1 NumOp2 I_2)) (δNum (I_1 NumOp2 I_2))]
  [(δNum/ (V_1 < V_2)) Err]
  [(δNum/ any) NaN]
)

(define-metafunction arith  
  [(δBool (B_1 ∧ B_2)) ,(if (eq? (term B_1) (term true)) (term B_2) (term false))]
  [(δBool (B_1 ∨ B_2)) ,(if (eq? (term B_1) (term true)) (term true) (term B_2))]
  [(δBool (¬ B_1)) ,(if (eq? (term B_1) (term true)) (term false) (term true))]
)

(define-metafunction errlet  
  [(δBool/ (B_1 BoolOp2 B_2)) (δBool (B_1 BoolOp2 B_2))]
  [(δBool/ (BoolOp1 B_1)) (δBool (BoolOp B_1))]
  [(δBool/ any) Err]
)

(define-metafunction errlet  
  [(δCast NaN) false];
  [(δCast Udef) false];
  [(δCast false) false];
  [(δCast 0) false];
  [(δCast I) true];
  [(δCast true) true];
  [(δCast any) Err]
)

;; x --> Udef | if x is not defined
;; δ --> NaN | if computation fails to produce a number
;; δ --> Err | if compuation fauls; but all values will be seen as true|false

;; False ::= false, 0, NaN, Udef
;; True  ::= true, Integer\0
;; Err is similar to an Exception

(define arith-reduction
  (reduction-relation
   arith
   (--> (in-hole C (BoolOp1 V))
        (in-hole C (δBool (BoolOp1 V)))
        δ-BoolOp1
   )
   (--> (in-hole C (V BoolOp2 W))
        (in-hole C (δBool (V BoolOp2 W)))
        δ-BoolOp2
   )
   (--> (in-hole C (V NumOp2 W))
        (in-hole C (δNum (V NumOp2 W)))
        δ-NumOp
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

(define let-reduction
  (extend-reduction-relation arith-reduction
   let
   (--> (in-hole C (let x = V in F))
        (in-hole C (subst x V F))
        Let
   )
))

(define errlet-reduction
  (extend-reduction-relation let-reduction
   errlet
   ;;;;;;;;;;
   ;   (--> (in-hole C Err)
   ;        Err
   ;        Err
   ;   ) ;; rule not possible because of cycles
   ;;;;;;;;;;
   ;;;;;;;;;; ERROR RULES
   (--> (in-hole C (BoolOp1 Err))
        Err
        δ-Op1-Err
   )
   (--> (in-hole C (Err Op2 W))
        Err
        δ-Op2-LErr
   )
   (--> (in-hole C (V Op2 Err))
        Err
        δ-Op2-RErr
   )
   (--> (in-hole C (if Err then F else G))
        Err
        If-Err
   )
   (--> (in-hole C (let x = Err in E))
        Err
        Let-Err
   )
   ;;;;;;;;;; 
   (--> (in-hole C x)
        (in-hole C Udef)
        Udef
   )
   (--> (in-hole C (BoolOp1 V))
        (in-hole C (δBool/ (BoolOp1 (δCast V))))
        δ-BoolOp1
   )
   (--> (in-hole C (V BoolOp2 W))
        (in-hole C (δBool/ ((δCast V) BoolOp2 (δCast W))))
        δ-BoolOp2
   )
   (--> (in-hole C (V NumOp2 W))
        (in-hole C (δNum/ (V NumOp2 W)))
        δ-NumOp
   )
))

(define ljs-reduction
  (extend-reduction-relation errlet-reduction
   ljs
   (--> (in-hole C ((λ x E) V))
        (in-hole C (subst x V E))
        App
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

;(traces let-reduction (term (1 + 2)))
;(traces let-reduction (term xx))
;(traces let-reduction (term (let x = (1 + 1) in (2 + 2))))
;(traces let-reduction (term (let x = (1 + 1) in (x + 2))))
;(traces let-reduction (term (let x = 1 in (x + 1))))
;(traces let-reduction (term (let x = 1 in (let y = 1 in (x + y)))))

;(traces errlet-reduction (term x))
;(traces errlet-reduction (term (NaN + 2)))
;(traces errlet-reduction (term (Udef + 2)))

;(traces errlet-reduction (term (Err + 2))) ;; TODO
;(traces errlet-reduction (term (x + 2)))

(traces ljs-reduction (term (λ x (1 + 2))))
(traces ljs-reduction  (term (λ x (x + 1))))
(traces ljs-reduction  (term (λ x (λ y (x + y)))))

(traces ljs-reduction (term ((λ x (1 + 2)) 1)))
(traces ljs-reduction  (term ((λ x (x + 1)) 1)))
(traces ljs-reduction  (term (( (λ x (λ y (x + y))) 1) 1)))

(traces ljs-reduction  (term (let plus = (λ x (λ y (x + y))) in ((plus 1) 1))))
