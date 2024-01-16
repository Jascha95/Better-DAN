#lang racket
(require redex)

;;  _____             _                               _ 
;; / ____|           | |                             | |
;;| (___  _   _ _ __ | |_ __ ___  __   __ _ _ __   __| |
;; \___ \| | | | '_ \| __/ _` \ \/ /  / _` | '_ \ / _` |
;; ____) | |_| | | | | || (_| |>  <  | (_| | | | | (_| |
;;|_____/ \__, |_| |_|\__\__,_/_/\_\  \__,_|_| |_|\__,_|
;;         __/ |                                        
;;        |___/                                         
;;  _____                            _   _            _                       _           
;; / ____|                          | | (_)          | |                     (_)          
;;| (___   ___ _ __ ___   __ _ _ __ | |_ _  ___    __| | ___  _ __ ___   __ _ _ _ __  ___ 
;; \___ \ / _ \ '_ ` _ \ / _` | '_ \| __| |/ __|  / _` |/ _ \| '_ ` _ \ / _` | | '_ \/ __|
;; ____) |  __/ | | | | | (_| | | | | |_| | (__  | (_| | (_) | | | | | | (_| | | | | \__ \
;;|_____/ \___|_| |_| |_|\__,_|_| |_|\__|_|\___|  \__,_|\___/|_| |_| |_|\__,_|_|_| |_|___/
                                                                                        
                                                                                        
(define-language λjs
  
  ;; constants 
  (c natural)
  
  ;; object
  (o ((x v) ...))
  
  ;; locations
  (l natural)
  
  ;; variables
  ((x y z) variable-not-otherwise-mentioned)
  
  ;; operations
  (op + *)
  
  ;; expressions
  ((e f g)
   c
   x
   (ref l)
   (op e f)
   (λ (x τ) e)
   (e f)
   new
   (get e x)
   (set e x f)
  )
  
  ;; types
  ;(τ Number (→ τ τ) Object)
  (τ Number (→ τ τ) ((x τ) ...))
  
  ;; type environment
  [Γ · (x : τ Γ)]
    
  ;; state
  (σ ((l o) ...))
  
  ;; values
  ((u v w) c (λ (x τ) e) (ref l))
      
  ;; evaluation context
  ((C D)
   hole
   (op C f)
   (op v C)
   (C f)
   (v C)
   (get C x)
   (set C f g)
   (set (ref l) x C)
  )
)

;; test sntax
;(redex-match λjs e (term 1))
;(redex-match λjs e (term x))

;(redex-match λjs e (term (+ 1 1)))
;(redex-match λjs e (term (* 1 1)))

;(redex-match λjs e (term (λ (x Number) 1)))
;(redex-match λjs e (term ((λ (x Number) 1) 1)))

;(redex-match λjs e (term ((λ (x Number) (+ x 1)) 1)))
;(redex-match λjs e (term (((λ (x Number) (λ (y Number) (+ x y))) 1) 1)))
;(redex-match λjs e (term ((λ (x (→ Number Number)) (x 1)) (λ (x Number) x))))

;(redex-match λjs e (term new))
;(redex-match λjs e (term (get 1 x)))
;(redex-match λjs e (term (set 1 x 1)))
                           
(define-metafunction λjs
  subst : x v e -> e
  [(subst x v (λ (x τ) e)) (λ (x τ) e)]
  [(subst x v (λ (y τ) e)) (λ (y τ) (subst x v e))]
  [(subst x v (e f)) ((subst x v e) (subst x v f))]
  [(subst x v (op e f)) (op (subst x v e) (subst x v f))]
  [(subst x v x) v]
  [(subst x v y) y]
  [(subst x v c) c]
  [(subst x v (ref l)) (ref l)]
  [(subst x v new) new]
  [(subst x v (get e y)) (get (subst x v e) y)]
  [(subst x v (set e y f)) (set (subst x v e) y (subst x v f))]
)

(define-metafunction λjs
  [(δ (+ v w)) ,(+ (term v) (term w))]
  [(δ (* v w)) ,(* (term w) (term v))]
)

(define-metafunction λjs
  newloc : σ -> l
  [(newloc ()) ,1]
  [(newloc ((l o) ...)) ,(+ 1 (apply max (term (l ...))))]
)

;; _____          _            _   _             
;;|  __ \        | |          | | (_)            
;;| |__) |___  __| |_   _  ___| |_ _  ___  _ __  
;;|  _  // _ \/ _` | | | |/ __| __| |/ _ \| '_ \ 
;;| | \ \  __/ (_| | |_| | (__| |_| | (_) | | | |
;;|_|  \_\___|\__,_|\__,_|\___|\__|_|\___/|_| |_|

(define λjs-reduction
  (reduction-relation
   λjs
   (--> (σ (in-hole C (op v w)))
        (σ (in-hole C (δ (op v w))))
        "δ"
   )
   (--> (σ (in-hole C ((λ (x τ) e) v)))
        (σ (in-hole C (subst x v e)))
        "App"
   )
   (--> (((l o) ... ) (in-hole C new))
        (((l o) ... ((newloc ((l o) ... )) ())) (in-hole C (ref (newloc ((l o) ... )))))
        "New"
   )
   (--> (((l_1 o_1) ... (l_2 ((x u) ... (y v) (z w) ...)) (l_3 o_3) ...) (in-hole C (get (ref l_2) y)))
        (((l_1 o_1) ... (l_2 ((x u) ... (y v) (z w) ...)) (l_3 o_3) ...) (in-hole C v))
        "Get"
   )
   (--> (((l_1 o_1) ... (l_2 ((x u) ... (y v_1) (z w) ...)) (l_3 o_3) ...) (in-hole C (set (ref l_2) y v_2)))
        (((l_1 o_1) ... (l_2 ((x u) ... (y v_2) (z w) ...)) (l_3 o_3) ...)  (in-hole C (ref l_2)))
        "Update"
   )
   (--> (((l_1 o_1) ... (l_2 ((x u) ... )) (l_3 o_3) ...) (in-hole C (set (ref l_2) y v_2)))
        (((l_1 o_1) ... (l_2 ((x u) ... (y v_2))) (l_3 o_3) ...) (in-hole C (ref l_2)))
        "Add"
        (side-condition (not (member (term y) (term (x ...)))))
   )
))

;; test reduction
;(traces λjs-reduction (term (() 1)))
;(traces λjs-reduction (term (() x)))

;(traces λjs-reduction (term (() (+ 1 1))))
;(traces λjs-reduction (term (() (* 1 1))))

;(traces λjs-reduction (term (() (λ (x Number) 1))))
;(traces λjs-reduction (term (() ((λ (x Number) 1) 1))))

;(traces λjs-reduction (term (() ((λ (x Number) (+ x 1)) 1))))
;(traces λjs-reduction (term (() (((λ (x Number) (λ (y Number) (+ x y))) 1) 1))))
;(traces λjs-reduction (term (() ((λ (x (→ Number Number)) (x 1)) (λ (x Number) x)))))

;(traces λjs-reduction (term (() new)))
;(traces λjs-reduction (term (() (set new x 1))))
;(traces λjs-reduction (term (() ((λ (y ()) ((λ (x Number) (get y x)) (set y x 1)) ) new))))

;;      _           _                                 _   
;;     | |         | |                               | |  
;;     | |_   _  __| | __ _  ___ _ __ ___   ___ _ __ | |_ 
;; _   | | | | |/ _` |/ _` |/ _ \ '_ ` _ \ / _ \ '_ \| __|
;;| |__| | |_| | (_| | (_| |  __/ | | | | |  __/ | | | |_ 
;; \____/ \__,_|\__,_|\__, |\___|_| |_| |_|\___|_| |_|\__|
;;                     __/ |                              
;;                    |___/                               

(define-judgment-form
  λjs
  #:mode (types I I I O)
  #:contract (types Γ σ e τ) 

  [---------------------
   (types (x : τ Γ) σ x τ)]
  
  [(types Γ σ x τ_1)
   ------------------------------------
   (types (y : τ_2 Γ) σ x τ_1)]
 
  [(types Γ σ e Number) 
   (types Γ σ f Number)
   -----------------------
   (types Γ σ (op e f) Number)]
  
  [--------------------
   (types Γ σ c Number)]

   [(types (x : τ_1 Γ) σ e τ_2)
   -----------------------------
   (types Γ σ (λ (x τ_1) e) (→ τ_1 τ_2))]
  
  [(types Γ σ e (→ τ_1 τ_2))
   (types Γ σ f τ_1)
   -------------------------
   (types Γ σ (e f) τ_2)]
  
  [(types Γ σ e (→ τ_1 τ_2))
   (types Γ σ f τ_1)
   -------------------------
   (types Γ σ (e f) τ_2)]
  

  [
   -------------------------
   (types Γ σ new ())]
  
  [
   (types Γ σ e ((x τ_1) ... (y τ_2) (z τ_e3) ...))
   -------------------------
   (types Γ σ (get e y) τ_2)]
 
  [
   (types Γ σ f τ)
   (types Γ σ e ((x τ_1) ... (y τ_2) (z τ_e3) ...))
   -------------------------
   (types Γ σ (set e y f) ((x τ_1) ... (y τ) (z τ_e3) ...))]
 
  [
   (types Γ σ f τ)
   (types Γ σ e ((x τ_1) ...))
   -------------------------
   (types Γ σ (set e y f) ((x τ_1) ... (y τ)))]
)
;
;; test judgement
;(judgment-holds (types · () 1 Number))

;(judgment-holds (types · () (+ 1 1) Number))
;(judgment-holds (types · () (* 1 1) Number))

;(judgment-holds (types · () (λ (x Number) 1) (→ Number Number)))
;(judgment-holds (types · () ((λ (x Number) 1) 1) Number))

;(judgment-holds (types · () ((λ (x Number) (+ x 1)) 1) Number))
;(judgment-holds (types · () (((λ (x Number) (λ (y Number) (+ x y))) 1) 1) Number))
;(judgment-holds (types · () ((λ (x (→ Number Number)) (x 1)) (λ (x Number) x)) Number))

;(judgment-holds (types · () new ()))
;(judgment-holds (types · () (set new x 1) ((x Number))))
;(judgment-holds (types · () ((λ (y ()) ((λ (x ((x Number))) (get x x)) (set y x 1)) ) new) Number))

;(judgment-holds (types · () ((λ (y ()) y) new) ()))
;(judgment-holds (types · () ((λ (y ()) (set y x 1)) new) ((x Number))))
;(judgment-holds (types · () ((λ (y ()) (set y x 1)) new) ((x Number))))

;(traces λjs-reduction (term (() new)))
;(traces λjs-reduction (term (() (set new x 1))))
;(traces λjs-reduction (term (() )))

;; _____                 _                   _______        _   
;;|  __ \               | |                 |__   __|      | |  
;;| |__) |__ _ _ __   __| | ___  _ __ ___      | | ___  ___| |_ 
;;|  _  // _` | '_ \ / _` |/ _ \| '_ ` _ \     | |/ _ \/ __| __|
;;| | \ \ (_| | | | | (_| | (_) | | | | | |    | |  __/\__ \ |_ 
;;|_|  \_\__,_|_| |_|\__,_|\___/|_| |_| |_|    |_|\___||___/\__|
                                                              
(define (types? e) (not (null? (judgment-holds (types · () e τ) τ))))
(define v? (redex-match λjs v))
(define (reduces? e) (not (null? (apply-reduction-relation λjs-reduction (term (() e))))))
(define (progress-holds? e) (if (types? e) (or (v? e) (reduces? e)) #t))
(redex-check λjs e (progress-holds? (term e)) #:attempts 100000)