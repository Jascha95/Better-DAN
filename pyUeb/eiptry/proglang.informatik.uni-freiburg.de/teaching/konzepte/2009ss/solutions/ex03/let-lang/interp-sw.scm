(module interp-sw (lib "eopl.ss" "eopl")
  
  ;; interpreter for the LET language.  The \commentboxes are the
  ;; latex code for inserting the rules into the code in the book.
  ;; These are too complicated to put here, see the text, sorry.

  (require "drscheme-init.scm")

  (require "lang-sw.scm")
  (require "data-structures.scm")
  (require "environments.scm")

  (provide value-of-program value-of)

;;;;;;;;;;;;;;;; the interpreter ;;;;;;;;;;;;;;;;

  ;; value-of-program : Program -> ExpVal
  ;; Page: 71
  (define value-of-program 
    (lambda (pgm)
      (cases program pgm
        (a-program (exp1)
          (value-of exp1 (init-env))))))

  ;; value-of : Exp * Env -> ExpVal
  ;; Page: 71
  (define value-of
    (lambda (exp env)
      (cases expression exp

        ;\commentbox{ (value-of (const-exp \n{}) \r) = \n{}}
        (const-exp (num) (num-val num))

        ;\commentbox{ (value-of (var-exp \x{}) \r) = (apply-env \r \x{})}
        (var-exp (var) (apply-env env var))

        ;\commentbox{\diffspec}
        (diff-exp (exp1 exp2)
          (let ((val1 (value-of exp1 env))
                (val2 (value-of exp2 env)))
            (let ((num1 (expval->num val1))
                  (num2 (expval->num val2)))
              (num-val
                (- num1 num2)))))

        ;\commentbox{\zerotestspec}
        (zero?-exp (exp1)
          (let ((val1 (value-of exp1 env)))
            (let ((num1 (expval->num val1)))
              (if (zero? num1)
                (bool-val #t)
                (bool-val #f)))))
              
        ;\commentbox{\ma{\theifspec}}
        (if-exp (exp1 exp2 exp3)
          (let ((val1 (value-of exp1 env)))
            (if (expval->bool val1)
              (value-of exp2 env)
              (value-of exp3 env))))

        ;\commentbox{\ma{\theletspecsplit}}
        
        ;; SW
        (let-exp (vars exps body)       
          (let ((vals (map (lambda (exp) (value-of exp env)) exps)))
            (value-of body
              (extend-env* vars vals env))))

        ;; SW
        (let*-exp 
         (vars exps body)
         (if (not (disjoint vars))
             (eopl:error 'value-of "list of variables ~s in let* expression not disjoint"
                         vars)
             (letrec ((build-env
                       (lambda (vars exps env)
                         (if (null? vars)
                             env
                             (let ((x (car vars))
                                   (v (value-of (car exps) env)))
                               (build-env (cdr vars) (cdr exps) 
                                          (extend-env x v env)))))))
               (value-of body (build-env vars exps env)))))
        )))

  ;; SW
  ; extend-env*: ListOf(Var) * ListOf(ExpVal) * Env -> Env
  ; usage: extends an environment with bindings for multiple variables. The
  ; list of variables and values must have the same length and the
  ; list of variables must be disjoint.
  (define extend-env*
    (lambda (vars vals env)
      (letrec ((aux
                (lambda (vars vals env)
                  (if (null? vars)
                      (if (null? vals)
                          env
                          (eopl:error 'extend-env* "more values than variables"))
                      (if (null? vals)
                          (eopl:error 'extend-env* "more variables than values")
                          (aux (cdr vars) (cdr vals) 
                               (extend-env (car vars) (car vals) env)))))))
        (if (not (disjoint vars))
            (eopl:error 'extend-env* "list of variables ~s not disjoint" vars)
            (aux vars vals env)))))
  
  ; disjoint: List -> Bool
  ; usage: returns #t iff the values in the given list are pairwise disjoint.
  (define disjoint
    (lambda (lst)
      (letrec ((aux
                (lambda (lst found)
                  (if (null? lst)
                      #t
                      (if (member (car lst) found)
                          #f
                          (aux (cdr lst) (cons (car lst) found)))))))
        (aux lst '()))))
  )

