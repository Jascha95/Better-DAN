#lang racket
(require redex)
(require "subst.rkt")

;; Our language
(define-language stlc
  (x variable-not-otherwise-mentioned)
  (op + - * /)
  (c integer
     op
     (δ op integer)
     )
  (v c
     (λ x τ e)
     )
  (e v
     x
     (e e)
     )
  (τ int
     (-> τ τ)
     t)
  (t variable-not-otherwise-mentioned)
  (E (E e)
     (v E)
     hole
     )
  (b (x τ))
  (Γ (b ...))
  (U ø (τ τ U))
  (S ø (t τ S))
  )

;; Utility function for the definition of arithmetic operators
(define-metafunction stlc
  delta : c e -> c
  [(delta op integer) (δ op integer)]
  [(delta (δ + integer_0) integer_1) ,(apply + (term (integer_0 integer_1)))]
  [(delta (δ - integer_0) integer_1) ,(apply - (term (integer_0 integer_1)))]
  [(delta (δ * integer_0) integer_1) ,(apply * (term (integer_0 integer_1)))]
  [(delta (δ / integer_0) integer_1) ,(apply / (term (integer_0 integer_1)))]
  )

(define-metafunction stlc
  find : Γ x -> τ
  [(find (_ ... (x_0 τ_0) _ ...) x_0) τ_0]
  )

(define-metafunction stlc
  add : Γ b -> Γ
  [(add (b_0 ... (x_2 _) b_1 ...) (x_2 any))
   (b_0 ... (x_2 any) b_1 ...)]
  [(add (b_0 ...) (x_k any))
   ((x_k any) b_0 ...)]
  )

(define red
  (reduction-relation
   stlc
   #:domain e
   (--> (in-hole E ((λ x_0 _ e_0) v_0))
        (in-hole E (subst (x_0 v_0) e_0))
        "β")
   (--> (in-hole E (c_0 v_0))
        (in-hole E (delta c_0 v_0))
        "δ")
   ))

(define-judgment-form stlc
  #:mode (⊢ I I O)
  #:contract (⊢ Γ e τ)

  [-----------------
   (⊢ _ integer int)]
  
  [-----------------
   (⊢ _ op (-> int (-> int int)))]
  
  [-----------------
   (⊢ _ (δ op integer) (-> int int))]

  [(⊢ Γ e_0 (-> τ_0 τ_1))
   (⊢ Γ e_1 τ_0)
   -----------------
   (⊢ Γ (e_0 e_1) τ_1)]

  [(⊢ (add Γ (x_0 τ_0)) e τ_1)
   -------------------
   (⊢ Γ (λ x_0 τ_0 e) (-> τ_0 τ_1))]

  [-----------------
   (⊢ (_ ... (x_0 τ_0) _ ...) x_0 τ_0)]
  )

;; Some useful predicates

(define (types? e)
  (not (null? (judgment-holds (⊢ () ,e τ) τ))))
(define v? (redex-match stlc v))
(define (reduces? e)
  (not (null? (apply-reduction-relation
               red
               e))))

;; Progress
(define (progress-holds? e)
  (if (types? e)
      (or (v? e)
          (reduces? e))
      #t))
(redex-check stlc e (progress-holds? (term e)))

(define (has-type e τ_0)
  (equal? (judgment-holds (⊢ () ,e τ) τ) τ_0))

;; Preservation
(define (preservation-holds? e)
  (if (and (types? e) (reduces? e))
      (let ([t (judgment-holds (⊢ () ,e τ) τ)])
        (and (not (null? t))
             (andmap (λ (e2) (has-type e2 t))
                     (apply-reduction-relation red e))
             )
        )
      #t)
  )
(redex-check stlc e (preservation-holds? (term e)))

;; Unification

(define-metafunction stlc
  unif : U -> S
  [(unif ø) ø]

  [(unif (τ_0 τ_0 U))
   (unif U)]

  [(unif (t τ U))
   (t τ (unif (subst (t τ) U)))
   (side-condition (not (member (term t) (term τ))))]
  
  [(unif ((-> τ_1 τ_2) (-> τ_3 τ_4) U))
   (unif (τ_1 τ_3 (τ_2 τ_4 U)))]

  [(unif (τ t U))
   (unif (t τ U))]

  )

; To write: type inference
