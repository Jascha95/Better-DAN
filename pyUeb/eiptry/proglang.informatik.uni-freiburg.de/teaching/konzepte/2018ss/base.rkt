#lang racket
(require redex)

;; Our language
(define-language lambdajs
  (x variable-not-otherwise-mentioned)
  (field variable-not-otherwise-mentioned)

  (v number
     (λ (x τ) e)
     (object (field v) ...)
     )
  (e v
     x
     (e e)
     (object (field e) ...)
     (set e field e)
     (get e field)
     )
  (τ int
     (object (field τ) ...)
     (-> τ τ)
     t)

  (t variable-not-otherwise-mentioned)
  
  (g ((x v) ...))
  (Γ ((x τ) ...))
  )

(define red
  (reduction-relation
   lambdajs
   #:domain (g e)
   ;; Reduction rules here
   
   ))

(define-judgment-form lambdajs
  #:mode (⊢ I I I)
  #:contract (⊢ Γ e τ)

  ;; Typing rules here
  )


;;;;; RANDOM TESTING

;; Some useful predicates

(define (types? e)
  (not (null? (judgment-holds (⊢ () ,e τ) τ))))
(define v? (redex-match lambdajs v))
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
(redex-check lambdajs e (progress-holds? (term e)))

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
(redex-check lambdajs e (preservation-holds? (term e)))
