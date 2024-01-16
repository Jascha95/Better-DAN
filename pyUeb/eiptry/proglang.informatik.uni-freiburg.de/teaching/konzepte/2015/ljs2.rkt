#lang racket
(require redex)

;;  _____             _             
;; / ____|           | |            
;;| (___  _   _ _ __ | |_ __ ___  __
;; \___ \| | | | '_ \| __/ _` \ \/ /
;; ____) | |_| | | | | || (_| |>  < 
;;|_____/ \__, |_| |_|\__\__,_/_/\_\
;;         __/ |                    
;;        |___/                     

;;          _ _   _    
;; __ _ _ _(_) |_| |_  
;;/ _` | '_| |  _| ' \ 
;;\__,_|_| |_|\__|_||_|
                     
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
  
  (Op2 NumOp2 BoolOp2 CmpOp2)
  (NumOp2 + - * /)
  (BoolOp2 ∧ ∨)
  (CmpOp2 < >)
  
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
   (Op1 C)
   (if C then F else G)
  )
)

;; _     _   
;;| |___| |_ 
;;| / -_)  _|
;;|_\___|\__|         

(define-extended-language let arith
  
  ;; variables
  ((x y z) variable-not-otherwise-mentioned)
 
  ;; expressions
  ((E F G) .... x (let x = E in F))
  
  ;; evaluation context
  ((C D) .... (let x = C in F))
)
;;             _     _   _     _   
;; ___ _ _ _ _| |___| |_| |___| |_ 
;;/ -_) '_| '_| / -_)  _| / -_)  _|
;;\___|_| |_| |_\___|\__|_\___|\__|

(define-extended-language errlet let
 
  ;; values
  ((U V W) .... NaN Udef)
  
  ;; behavior
  ((R S T) V Err)
)

;; _  _    
;;| |(_)___
;;| || (_-<
;;|_|/ /__/
;; |__/    

(define-extended-language ljs errlet
 
  ;; values
  ((U V W) .... (λ x E))
  
  ;; expressions
  ((E F G) .... (E F))
  
   ;; evaluation context
  ((C D) .... (C F) (V C))
)

;; _                     _     _  _    
;;| |_ _  _ _ __  ___ __| |___| |(_)___
;;|  _| || | '_ \/ -_) _` |___| || (_-<
;; \__|\_, | .__/\___\__,_|   |_|/ /__/
;;     |__/|_|                 |__/    

(define-extended-language typed-ljs ljs
  
  ;; types
  (τ Int Bool NaN Udef Err (→ τ τ))
  
  ;; type environment
  [Γ · (x : τ Γ)]
 
  ;; values
  ((U V W) .... (λ (x τ) E))
  
  ;; expressions
  ((E F G) .... (E F))
  
   ;; evaluation context
  ((C D) .... (C F) (V C))
)

;; _                     _     _  _    ___ 
;;| |_ _  _ _ __  ___ __| |___| |(_)__|_  )
;;|  _| || | '_ \/ -_) _` |___| || (_-</ / 
;; \__|\_, | .__/\___\__,_|   |_|/ /__/___|
;;     |__/|_|                 |__/        

(define-extended-language typed-ljs2 typed-ljs
  
  ;; types
  (τ .... (τ_1 • τ_2))
   
  ;; values
  ((U V W) .... (V • W))
  
  ;; expressions
  ((E F G) .... (E • F) (left E) (right E) (cast τ E))
  
   ;; evaluation context
  ((C D) .... (C • F) (V • C) (left C) (right C) (cast τ C))
)

;; test sntax
(redex-match typed-ljs2 E (term (1 • 2)))
(redex-match typed-ljs2 E (term ((1 + 1) • (2 + 2))))
(redex-match typed-ljs2 E (term (left ((1 + 1) • (2 + 2)))))
(redex-match typed-ljs2 E (term (right ((1 + 1) • (2 + 2)))))

(redex-match typed-ljs2 E (term (cast Int 1)))
(redex-match typed-ljs2 E (term (cast Int true)))

(redex-match typed-ljs2 E (term (cast Bool (1 + 1))))
(redex-match typed-ljs2 E (term (cast (Int • Bool) (1 + 1))))
(redex-match typed-ljs2 E (term (cast (→ Int Bool) (1 + 1))))

;; __  __      _                __                  _   _                 
;;|  \/  |    | |              / _|                | | (_)                
;;| \  / | ___| |_ __ _ ______| |_ _   _ _ __   ___| |_ _  ___  _ __  ___ 
;;| |\/| |/ _ \ __/ _` |______|  _| | | | '_ \ / __| __| |/ _ \| '_ \/ __|
;;| |  | |  __/ || (_| |      | | | |_| | | | | (__| |_| | (_) | | | \__ \
;;|_|  |_|\___|\__\__,_|      |_|  \__,_|_| |_|\___|\__|_|\___/|_| |_|___/

(require redex/tut-subst)

(define-metafunction typed-ljs2
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
  [(subst x V (E • F))
   ((subst x V E) • (subst x V F))]
  [(subst x V (left E))
   (left (subst x V E))]
  [(subst x V (right E))
   (right (subst x V E))]
  [(subst x V (cast τ E))
   (cast τ (subst x V E))]
)

(define x? (redex-match let x))

(define-metafunction arith
  [(δNum (I_1 + I_2)) ,(+ (term I_1) (term I_2))]
  [(δNum (I_1 - I_2)) ,(- (term I_1) (term I_2))]
  [(δNum (I_1 * I_2)) ,(* (term I_1) (term I_2))]
  [(δNum (I_1 / I_2)) ,(/ (term I_1) (term I_2))]  
  [(δNum (I_1 < I_2)) ,(if (< (term I_1) (term I_2)) (term true) (term false))]
  [(δNum (I_1 > I_2)) ,(if (> (term I_1) (term I_2)) (term true) (term false))]
)

(define-metafunction errlet
  [(δNum/ (I_1 / 0)) NaN] ;; error rule
  [(δNum/ (I_1 NumOp2 I_2)) (δNum (I_1 NumOp2 I_2))]
  [(δNum/ (I_1 CmpOp2 I_2)) (δNum (I_1 CmpOp2 I_2))]
  [(δNum/ (V_1 < V_2)) Err]
  [(δNum/ (V_1 > V_2)) Err]
  [(δNum/ any) NaN]
)

(define-metafunction arith  
  [(δBool (B_1 ∧ B_2)) ,(if (eq? (term B_1) (term true)) (term B_2) (term false))]
  [(δBool (B_1 ∨ B_2)) ,(if (eq? (term B_1) (term true)) (term true) (term B_2))]
  [(δBool (¬ B_1)) ,(if (eq? (term B_1) (term true)) (term false) (term true))]
)

(define-metafunction errlet  
  [(δBool/ (B_1 BoolOp2 B_2)) (δBool (B_1 BoolOp2 B_2))]
  [(δBool/ (BoolOp1 B_1)) (δBool (BoolOp1 B_1))]
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

;; _____          _            _   _             
;;|  __ \        | |          | | (_)            
;;| |__) |___  __| |_   _  ___| |_ _  ___  _ __  
;;|  _  // _ \/ _` | | | |/ __| __| |/ _ \| '_ \ 
;;| | \ \  __/ (_| | |_| | (__| |_| | (_) | | | |
;;|_|  \_\___|\__,_|\__,_|\___|\__|_|\___/|_| |_|

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
   (--> (in-hole C (V CmpOp2 W))
        (in-hole C (δNum (V CmpOp2 W)))
        δ-CmpOp
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
   (--> (in-hole C (V CmpOp2 W))
        (in-hole C (δNum/ (V CmpOp2 W)))
        δ-CmpOp
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

(define typed-reduction
  (extend-reduction-relation errlet-reduction
   typed-ljs2
   (--> (in-hole C (left (V • W)))
        (in-hole C V)
        Left
   )
   (--> (in-hole C (right (V • W)))
        (in-hole C W)
        Right
   )
   (--> (in-hole C (cast τ V))
        (in-hole C V)
        Cast
   )
   (--> (in-hole C ((λ (x τ) E) V))
        (in-hole C (subst x V E))
        TypedApp
   )
))

;; test reduction
;(traces typed-reduction (term (left (1 • 2))))
;(traces typed-reduction (term (right (1 • 2))))
;(traces typed-reduction (term ((1 + 1) • (2 + 2))))
;(traces typed-reduction (term (left ((1 + 1) • (2 + 2)))))

;(traces typed-reduction (term (cast (→ Int Bool) (1 + 1))))

;;(traces typed-reduction (term ((λ (x Int) x) 0)))

;;      _           _                                 _   
;;     | |         | |                               | |  
;;     | |_   _  __| | __ _  ___ _ __ ___   ___ _ __ | |_ 
;; _   | | | | |/ _` |/ _` |/ _ \ '_ ` _ \ / _ \ '_ \| __|
;;| |__| | |_| | (_| | (_| |  __/ | | | | |  __/ | | | |_ 
;; \____/ \__,_|\__,_|\__, |\___|_| |_| |_|\___|_| |_|\__|
;;                     __/ |                              
;;                    |___/                               

(define-judgment-form
  typed-ljs2
  #:mode (types I I O)
  #:contract (types Γ E τ)
  
  [---------------------
   (types (x : τ Γ) x τ)]
  
  [(types Γ x τ_1)
   ------------------------------------
   (types (y : τ_2 Γ) x τ_1)]
 
  [--------------------
   (types Γ I Int)]
  
  [--------------------
   (types Γ B Bool)]
  
  [--------------------
   (types Γ NaN NaN)]
  
  [--------------------
   (types Γ Udef Udef)]
  
  [--------------------
   (types Γ Err Err)]
  
 [(types Γ E Bool) 
   -----------------------
   (types Γ (BoolOp1 E) Bool)]
  
  [(types Γ E Bool) 
   (types Γ F Bool)
   -----------------------
   (types Γ (E BoolOp2 F) Bool)]
  
  [(types Γ E Int) 
   (types Γ F Int)
   -----------------------
   (types Γ (E CmpOp2 F) Bool)]
  
  [(types Γ E Int) 
   (types Γ F Int)
   -----------------------
   (types Γ (E NumOp2 F) Int)]

  [(types Γ E Bool)
   (types Γ F τ)
   (types Γ G τ)
   -----------------------------
   (types Γ (if E then F else G) τ)]
  
   [(types (x : τ_1 Γ) E τ_2)
   -----------------------------
   (types Γ (λ (x τ_1) E) (→ τ_1 τ_2))]
  
  [(types Γ E (→ τ_1 τ_2))
   (types Γ F τ_1)
   -------------------------
   (types Γ (E F) τ_2)]
  
  [(types Γ E τ_1)
   (types (x : τ_1 Γ) F τ_2)
   -------------------------
   (types Γ (let x = E in F) τ_2)]
  
  [(types Γ E τ_1) 
   (types Γ F τ_2)
   -----------------------
   (types Γ (E • F) (τ_1 • τ_2))]
  
  [(types Γ E (τ_1 • τ_2)) 
   -----------------------
   (types Γ (left E) τ_1)]
  
  [(types Γ E (τ_1 • τ_2)) 
   -----------------------
   (types Γ (right E) τ_2)]
  
;  [-----------------------
;   (types Γ (cast τ E) τ)]
)

;; test judgement
(judgment-holds (types · (1 • 1) (Int • Int)))
(judgment-holds (types · (1 • true) (Int • Bool)))
(judgment-holds (types · ((1 + 1) • (true ∧ false)) (Int • Bool)))

(judgment-holds (types · (left ((1 + 1) • (true ∧ false))) Int))
(judgment-holds (types · (right ((1 + 1) • (true ∧ false))) Bool))

(judgment-holds (types · (cast Int 1) Int))
(judgment-holds (types · (cast Bool 1) Bool))
(judgment-holds (types · ((cast Int true) + (cast Int false)) Int))

(judgment-holds (types · (cast Int ((λ (x Int) x) (cast Int true))) Int))

(judgment-holds (types · ((λ (x Int) x) 0) Int))

;; _____                 _                   _______        _   
;;|  __ \               | |                 |__   __|      | |  
;;| |__) |__ _ _ __   __| | ___  _ __ ___      | | ___  ___| |_ 
;;|  _  // _` | '_ \ / _` |/ _ \| '_ ` _ \     | |/ _ \/ __| __|
;;| | \ \ (_| | | | | (_| | (_) | | | | | |    | |  __/\__ \ |_ 
;;|_|  \_\__,_|_| |_|\__,_|\___/|_| |_| |_|    |_|\___||___/\__|
                                                              
(define (types? E) (not (null? (judgment-holds (types · ,E τ) τ))))
(define v? (redex-match typed-ljs2 V))
(define (reduces? E) (not (null? (apply-reduction-relation typed-reduction (term ,E)))))
(define (progress-holds? E) (if (types? E) (or (v? E) (reduces? E)) #t))
(redex-check typed-ljs2 E (progress-holds? (term E)) #:attempts 100000)