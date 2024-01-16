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
  ((x y z) variable-not-otherwise-mentioned this proto)
  
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
   (new e)
   (get e x)
   (set e x f)
   (mcall e x f)
   (λ z (x τ) e)
  )
  
  ;; types
  ;(τ Number (→ τ τ) Object)
  (τ Number (→ τ τ) ((x τ) ...))
  
  ;; type environment
  [Γ · (x : τ Γ)]
    
  ;; state
  (σ ((l o) ...))
  
  ;; values
  ((u v w) c (λ (x τ) e) (ref l) (λ z (x τ) e))
      
  ;; evaluation context
  ((C D)
   hole
   (op C f)
   (op v C)
   (C f)
   (v C)
   (new C)
   (get C x)
   (set C x f)
   (set (ref l) x C)
  
   (mcall C x f)
   (mcall (ref l) x C)
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

;(redex-match λjs e (term (new 1)))
;(redex-match λjs e (term (get 1 x)))
;(redex-match λjs e (term (set 1 x 1)))

;(redex-match λjs e (term (λ z (x Number) 1)))
;(redex-match λjs e (term (λ z (x Number) (+ x 1)))) 

;(redex-match λjs e (term this))

(define-metafunction λjs
  subst : x v e -> e
  [(subst x v (λ (x τ) e)) (λ (x τ) e)]
  [(subst x v (λ (y τ) e)) (λ (y τ) (subst x v e))]
  [(subst x v (e f)) ((subst x v e) (subst x v f))]
  [(subst x v (op e f)) (op (subst x v e) (subst x v f))]
  [(subst x v x) v]
  [(subst x v y) y]
  [(subst x v c) c]
  [(subst x v this) this]
  [(subst x v (ref l)) (ref l)]
  [(subst x v (new e)) (new (subst x v e))]
  [(subst x v (get e y)) (get (subst x v e) y)]
  [(subst x v (set e y f)) (set (subst x v e) y (subst x v f))]
  [(subst x v (mcall e y f)) (set (subst x v e) y (subst x v f))]
  
  [(subst x v (λ z (x τ) e)) (λ z (x τ) e)]
  [(subst x v (λ z (y τ) e)) (λ z (y τ) (subst x v e))] 
  [(subst z v (λ z (x τ) e)) (λ z (x τ) e)] 

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
   (--> (((l o) ... ) (in-hole C (new v)))
        (((l o) ... ((newloc ((l o) ... )) ((proto v)))) (in-hole C (ref (newloc ((l o) ... )))))
        "New"
   )
   
   (--> (((l_1 o_1) ... (l_2 ((x u) ... (y v) (z w) ...)) (l_3 o_3) ...) (in-hole C (get (ref l_2) y)))
        (((l_1 o_1) ... (l_2 ((x u) ... (y v) (z w) ...)) (l_3 o_3) ...) (in-hole C v))
        "Get"
   )
   (--> (((l_1 o_1) ... (l_2 ((proto (ref l)) (x u) ... )) (l_3 o_3) ...) (in-hole C (get (ref l_2) y)))
        (((l_1 o_1) ... (l_2 ((proto (ref l)) (x u) ... )) (l_3 o_3) ...) (in-hole C (get (ref l) y)))
        "Proto"
        (side-condition (not (member (term y) (term (x ...)))))
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
   (--> (((l_1 o_1) ... (l_2 ((x u) ... (y v) (z w) ...)) (l_3 o_3) ...) (in-hole C (mcall (ref l_2) y v_y)))
        (((l_1 o_1) ... (l_2 ((x u) ... (y v) (z w) ...)) (l_3 o_3) ...) (in-hole C ((subst this (ref l_2) v) v_y)))
        "MCall"
   )
   (--> (σ (in-hole C ((λ z (x τ) e) v)))
        (σ (in-hole C (subst x v (subst z (λ z (x τ) e) e))))
        "Rec"
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

;(traces λjs-reduction (term (() (λ z (x Number) 1))))
;(traces λjs-reduction (term (() (λ z (x Number) x))))
;(traces λjs-reduction (term (() (λ z (x Number) z))))

;(traces λjs-reduction (term (() ((λ z (x Number) 1) 1))))
;(traces λjs-reduction (term (() ((λ z (x Number) x) 1))))
;(traces λjs-reduction (term (() ((λ z (x Number) z) 1))))

(traces λjs-reduction (term (() (new 0))))

(traces λjs-reduction (term (() (mcall (set (new 0) x (λ (y Number) (+ y 1))) x 1))))
(traces λjs-reduction (term (() (mcall (set (set (new 0) x (λ (y Number) (+ (get this z) y))) z 4711) x 1))))

(traces λjs-reduction (term (() (get (set (new 0) x 1) x))))
(traces λjs-reduction (term (() (get (set (new (set (new 0) y 2)) x 1) y))))