#lang racket
(require redex)

(define-language while
  ;; Constants 
  (c number
     boolean
     )
  ;; Variables
  ((x y z) variable-not-otherwise-mentioned)
  ;; Operations
  (op + * - / < >)
  ;; Expressions
  (e c
     x
     (op e e)
  )
  ;; Statements
  (s (set x e)
     skip
     (s ...)
     (if e s s)
     (while e s)
     (throw r)
     (try s cases)
     )
  (cases ((r s) ...))

  ;; Return values
  (v c
     r)
  ;; Errors
  (r div0
     typeerror
     )

  ;; Environment
  (b (x c))
  (σ (b ...))
  )

(define-metafunction while
  OP : op v v -> v
  ; division by 0
  [(OP / number_1 0) div0]
  ; Arith operators
  [(OP + number_1 number_2) ,(apply + (term (number_1 number_2)))]
  [(OP * number_1 number_2) ,(apply * (term (number_1 number_2)))]
  [(OP - number_1 number_2) ,(apply - (term (number_1 number_2)))]
  [(OP / number_1 number_2) ,(apply / (term (number_1 number_2)))]
  ; Comparison operators
  [(OP < number_1 number_2)
   ,(if (< (term number_1) (term number_2)) (term #true) (term #false))]
  [(OP > number_1 number_2)
   ,(if (> (term number_1) (term number_2)) (term #true) (term #false))]
  ; Error propagation
  [(OP _ _ r) r]
  [(OP _ r _) r]
  ; Type error
  [(OP _ _ _) typeerror]
  )

(define-metafunction while
  STORE : x c σ -> σ
  [(STORE x_0 c_0 (b_1 ... (x_0 _) b_2 ...)) (b_1 ... (x_0 c_0) b_2 ...)]
  [(STORE x_0 c_0 (b_1 ...))                 ((x_0 c_0) b_1 ...)]
  )

;; Big step

; Expressions
(define-judgment-form while
  #:mode (=e=> I I O)
  #:contract (=e=> σ e v)
  
  [(=e=> σ e_1 v_1)
   (=e=> σ e_2 v_2)
   -------------------------
   (=e=> σ (op e_1 e_2) (OP op v_1 v_2))]

  [-------------------------
   (=e=> ( b_1 ... (x_0 c_0) b_2 ... ) x_0 c_0)]
  
  [-------------------------
   (=e=> _ c_0 c_0)]

  )

(define-judgment-form while
  #:mode (=r=> I I O O)
  #:contract (=r=> σ s σ r)

  [(=e=> σ_0 e_0 r_0)
   -----------------------------
   (=r=> σ_0 (if e_0 s_1 s_2) σ_0 r_0)]

  [(=e=> σ_0 e_0 r_0)
   --------------------------
   (=r=> σ_0 (while e_0 s) σ_0 r_0)]

  [(=e=> σ_0 e_0 r_0)
   -------------------------
   (=r=> σ_0 (set x e_0) σ_0 r_0)]
  
  [----------------------------
   (=r=> σ_0 (throw r_0) σ_0 r_0)]
  
  [(=r=> σ_0 s_0 σ_1 r_1)
   -----------------------------
   (=r=> σ_0 (s_0 s_1 ...) σ_1 r_1)]
  
  [(=s=> σ_0 s_0 σ_1)
   (=r=> σ_1 (s_1 ...) σ_2 r_1)
   -----------------------------
   (=r=> σ_0 (s_0 s_1 ...) σ_2 r_1)]

  [(=e=> σ_0 e #true)
   (=r=> σ_0 s_1 σ_1 r_1)
   -----------------------------
   (=r=> σ_0 (if e s_1 s_2) σ_1 r_1)]

  [(=e=> σ_0 e #false)
   (=r=> σ_0 s_2 σ_1 r_1)
   -----------------------------
   (=r=> σ_0 (if e s_1 s_2) σ_1 r_1)]

  [(=e=> σ_0 e #true)
   (=r=> σ_0 s σ_1 r_1)
   --------------------------
   (=r=> σ_0 (while e s) σ_1 r_1)]
  
  [(=e=> σ_0 e #true)
   (=s=> σ_0 s σ_1)
   (=r=> σ_1 (while e s) σ_2 r_1)
   --------------------------
   (=r=> σ_0 (while e s) σ_2 r_1)]
  
  [(=r=> σ_0 s_1 σ_1 r_1)
   (where ((r_!_1 _) ...) cases)
   -----------------------
   (=r=> σ_0 (try s_1 cases) σ_1 r_1)]
  
  [(=r=> σ_0 s_1 σ_1 r_1)
   (where (any ... (r_1 s_0) any ...) cases)
   (=r=> σ_1 s_0 σ_2 r_2)
   -----------------------
   (=r=> σ_0 (try s_1 cases) σ_2 r_2)]
  )
  
(define-judgment-form while
  #:mode (=s=> I I O)
  #:contract (=s=> σ s σ)

  [-------------------------
   (=s=> σ skip σ)]
  
  [(=e=> σ e c)
   -------------------------
   (=s=> σ (set x e) (STORE x c σ))]

  [(=s=> σ_0 s_0 σ_1)
   (=s=> σ_1 (s_1 ...) σ_2)
   -----------------------------
   (=s=> σ_0 (s_0 s_1 ...) σ_2)]

  [(=e=> σ_0 e #true)
   (=s=> σ_0 s_1 σ_1)
   -----------------------------
   (=s=> σ_0 (if e s_1 s_2) σ_1)]

  [(=e=> σ_0 e #false)
   (=s=> σ_0 s_2 σ_1)
   -----------------------------
   (=s=> σ_0 (if e s_1 s_2) σ_1)]

  [(=e=> σ_0 e #true)
   (=s=> σ_0 s σ_1)
   (=s=> σ_1 (while e s) σ_2)
   --------------------------
   (=s=> σ_0 (while e s) σ_2)]
  
  [(=e=> σ_0 e_0 #false)
   --------------------------
   (=s=> σ_0 (while e_0 s) σ_0)]
  
  [-----------------------------
   (=s=> σ () σ)]

  [(=s=> σ_0 s_1 σ_1)
   -----------------------
   (=s=> σ_0 (try s_1 _) σ_1)]
  
  [(=r=> σ_0 s_1 σ_1 r_1)
   (where (any ... (r_1 s_0) any ...) cases)
   (=s=> σ_1 s_0 σ_2)
   -----------------------
   (=s=> σ_0 (try s_1 cases) σ_2)]
  )

(test-equal
 (judgment-holds (=e=> () (+ 1 (* 4 4)) v) v)
 '(17)
 )

(test-equal
 (judgment-holds (=e=> () (> 4 3) v) v)
 '(#true)
 )

(test-equal
 (judgment-holds (=e=> ((x 2)) (> x 4) v) v)
 '(#false)
 )

(test-equal
 (judgment-holds (=e=> ((x 3)) (+ 1 x) v) v)
 '(4)
 )

(test-equal
 (judgment-holds (=e=> () (+ 1 (/ 4 0)) v) v)
 '(div0)
 )

(test-equal
 (judgment-holds (=e=> () (+ 1 (> 4 0)) v) v)
 '(typeerror)
 )

(test-equal
 (judgment-holds (=s=> [(x 2)] (skip (set y 3) skip) σ) σ)
 '(((y 3) (x 2)))
 )

(test-equal
 (judgment-holds (=s=> ((y 2))
                       (if (> y 4) (set x 3) (set z 1))
                       σ) σ)
 '(((z 1) (y 2)))
 )

(test-equal
 (judgment-holds (=s=> ((a 2)) ((set b 3) (if (> b 2) (set c #t) (set c #f))) σ) σ)
 '(((c #t) (b 3) (a 2)))
 )

(test-equal
 (judgment-holds (=s=> ((x 2))
                       ((set y 3)
                        (if (< y 4) (set x 3) (set z 1))
                        )
                       σ) σ)
 '(((y 3) (x 3)))
 )

