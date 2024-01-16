(module inferrer-sw  (lib "eopl.ss" "eopl")

  (require "drscheme-init.scm")
  (require "lang.scm")
  (require "data-structures.scm")
  (require "unifier-sw.scm")
  (require "substitutions.scm")

  (provide type-of-program)

  ;;;;;;;;;;;;;;;; The Type Checker ;;;;;;;;;;;;;;;;

  (define equations? 
    (list-of (pair-of type? type?)))
  
  ;; Answer = Type * Equations
  
  (define-datatype answer answer?
    (an-answer                       
      (type type?)
      (equations equations?)))

  (define answer-type
    (lambda (a)
      (cases answer a
        (an-answer (t eqs) t))))
  
  (define answer-equations
    (lambda (a)
      (cases answer a
        (an-answer (t eqs) eqs))))
  
  ;; type-of-program : Program -> Type
  ;; Page: 267
  (define type-of-program
    (lambda (pgm)
      (cases program pgm
        (a-program (exp1)
          (cases answer (generate-equations exp1 (init-tenv))
            (an-answer (ty eqs)
              (let ((subst (solve-equations eqs (empty-subst))))
                (apply-subst-to-type ty subst))))))))

  ;; generate-equations : Exp * Tenv -> Answer
  ;; Page: 267--270
  (define generate-equations
    (lambda (exp tenv)
      (cases expression exp
        
        (const-exp (num) (an-answer (int-type) '()))
      
        (zero?-exp (exp1)
          (let ((a (generate-equations exp1 tenv)))
            (an-answer (bool-type) (cons (cons (answer-type a) (int-type)) (answer-equations a)))))
 
        (diff-exp (exp1 exp2)
          (let ((a1 (generate-equations exp1 tenv))
                (a2 (generate-equations exp2 tenv)))
            (an-answer (int-type) (append (list (cons (answer-type a1) (int-type))
                                                (cons (answer-type a2) (int-type)))
                                          (answer-equations a1) (answer-equations a2)))))

        (if-exp (exp1 exp2 exp3)
          (let ((a1 (generate-equations exp1 tenv))
                (a2 (generate-equations exp2 tenv))
                (a3 (generate-equations exp3 tenv)))
            (an-answer (answer-type a2) (append (list (cons (answer-type a1) (bool-type))
                                                      (cons (answer-type a2) (answer-type a3)))
                                                (answer-equations a1)
                                                (answer-equations a2)
                                                (answer-equations a3)))))

        (var-exp (var) (an-answer (apply-tenv tenv var) '()))

        (let-exp (var exp1 body)
          (let* ((a1 (generate-equations exp1 tenv))
                 (a2 (generate-equations body (extend-tenv var (answer-type a1) tenv))))
            (an-answer (answer-type a2) (append (answer-equations a1) (answer-equations a2)))))

        (proc-exp (var otype body)
          (let* ((arg-type (otype->type otype))
                 (a (generate-equations body (extend-tenv var arg-type tenv))))
            (an-answer (proc-type arg-type (answer-type a))
                       (answer-equations a))))

        (call-exp (rator rand)
          (let ((a-rator (generate-equations rator tenv))
                (a-rand (generate-equations rand tenv))
                (result-type (fresh-tvar-type)))
            (an-answer result-type (cons (cons (answer-type a-rator) (proc-type (answer-type a-rand) result-type))
                                         (append (answer-equations a-rator) (answer-equations a-rand))))))

        (letrec-exp (proc-result-otype proc-name 
                      bvar proc-arg-otype 
                      proc-body
                      letrec-body)
          (let* ((proc-result-type
                  (otype->type proc-result-otype)) 
                 (proc-arg-type
                  (otype->type proc-arg-otype))
                 (tenv-for-letrec-body
                  (extend-tenv 
                   proc-name
                   (proc-type proc-arg-type proc-result-type)
                   tenv))
                 (a-proc-body (generate-equations proc-body
                                                  (extend-tenv
                                                   bvar proc-arg-type tenv-for-letrec-body)))
                 (a-letrec-body (generate-equations letrec-body tenv-for-letrec-body)))
            (an-answer (answer-type a-letrec-body)
                       (cons (cons (answer-type a-proc-body) proc-result-type)
                             (append (answer-equations a-proc-body) (answer-equations a-letrec-body))))))
        )))

  ;; solve-equtions: Equations * Substitution -> Substitution
  (define solve-equations
    (lambda (eqs subst)
      (if (null? eqs)
          subst
          (let* ((eq (car eqs))
                 (t1 (car eq))
                 (t2 (cdr eq))
                 (rest-eqs (cdr eqs))
                 (next-subst (unifier (apply-subst-to-type t1 subst) 
                                      (apply-subst-to-type t2 subst)
                                      subst)))
            (solve-equations rest-eqs next-subst)))))
                 
    ;;;;;;;;;;;;;;;; type environments ;;;;;;;;;;;;;;;;
    
  ;; why are these separated?

  (define-datatype type-environment type-environment?
    (empty-tenv-record)
    (extended-tenv-record
      (sym symbol?)
      (type type?)
      (tenv type-environment?)))
    
  (define empty-tenv empty-tenv-record)
  (define extend-tenv extended-tenv-record)
    
  (define apply-tenv 
    (lambda (tenv sym)
      (cases type-environment tenv
        (empty-tenv-record ()
          (eopl:error 'apply-tenv "Unbound variable ~s" sym))
        (extended-tenv-record (sym1 val1 old-env)
          (if (eqv? sym sym1) 
            val1
            (apply-tenv old-env sym))))))
  
  (define init-tenv
    (lambda ()
      (extend-tenv 'x (int-type) 
        (extend-tenv 'v (int-type)
          (extend-tenv 'i (int-type)
            (empty-tenv))))))

  ;; fresh-tvar-type : () -> Type
  ;; Page: 265  
  (define fresh-tvar-type
    (let ((sn 0))
      (lambda ()
        (set! sn (+ sn 1))
        (tvar-type sn))))

  ;; otype->type : OptionalType -> Type
  ;; Page: 265
  (define otype->type
    (lambda (otype)
      (cases optional-type otype
        (no-type () (fresh-tvar-type))
        (a-type (ty) ty))))

  )
