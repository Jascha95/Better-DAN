(module interp-sw (lib "eopl.ss" "eopl")
  
  ;; interpreter for the EXPLICIT-REFS language

  (require "drscheme-init.scm")

  (require "lang.scm")
  (require "data-structures.scm")
  (require "environments.scm")
  (require "store-sw.scm")
  
  (provide value-of-program value-of instrument-let instrument-newref)

;;;;;;;;;;;;;;;; switches for instrument-let ;;;;;;;;;;;;;;;;

  (define instrument-let (make-parameter #f))

  ;; say (instrument-let #t) to turn instrumentation on.
  ;;     (instrument-let #f) to turn it off again.

;;;;;;;;;;;;;;;; the interpreter ;;;;;;;;;;;;;;;;

  ;; value-of-program : Program -> ExpVal
  ;; Page: 110
  (define value-of-program 
    (lambda (pgm)
      ;; SW: removed (initialize-store!)               ; new for explicit refs.
      (cases program pgm
        (a-program (exp1)
          (value-of exp1 (init-env) (empty-store))))))

  ;; value-of : Exp * Env * Store -> AnswerOf(ExpVal)
  ;; Page: 113
  (define value-of
    (lambda (exp env store)
      (cases expression exp

        ;\commentbox{ (value-of (const-exp \n{}) \r) = \n{}}
        (const-exp (num) (make-answer (num-val num) store))

        ;\commentbox{ (value-of (var-exp \x{}) \r) = (apply-env \r \x{})}
        (var-exp (var) (make-answer (apply-env env var) store))

        ;\commentbox{\diffspec}
        (diff-exp (exp1 exp2)
          (let* ((a1 (value-of exp1 env store))
                 (a2 (value-of exp2 env (get-store a1))))
            (let ((num1 (expval->num (get-value a1)))
                  (num2 (expval->num (get-value a2))))
              (make-answer (num-val (- num1 num2)) (get-store a2)))))
      
        ;\commentbox{\zerotestspec}
        (zero?-exp (exp1)
          (let ((answ (value-of exp1 env store)))
            (let ((num1 (expval->num (get-value answ))))
              (if (zero? num1)
                (make-answer (bool-val #t) (get-store answ))
                (make-answer (bool-val #f) (get-store answ))))))
              
        ;\commentbox{\ma{\theifspec}}
        (if-exp (exp1 exp2 exp3)
          (let ((answ (value-of exp1 env store)))
            (if (expval->bool (get-value answ))
              (value-of exp2 env (get-store answ))
              (value-of exp3 env (get-store answ)))))

        ;\commentbox{\ma{\theletspecsplit}}
        (let-exp (var exp1 body)       
          (let ((answ (value-of exp1 env store)))
            (value-of body
              (extend-env var (get-value answ) env)
              (get-store answ))))
        
        (proc-exp (var body)
          (make-answer (proc-val (procedure var body env)) store))

        (call-exp (rator rand)
          (let* ((a1 (value-of rator env store))
                 (proc (expval->proc (get-value a1)))
                 (a2 (value-of rand env (get-store a1))))
            (apply-procedure proc (get-value a2) (get-store a2))))

        (letrec-exp (p-names b-vars p-bodies letrec-body)
          (value-of letrec-body
            (extend-env-rec* p-names b-vars p-bodies env)
            store))

        (begin-exp (exp1 exps)
          (letrec 
            ((value-of-begins
               (lambda (e1 es store)
                 (let ((answ (value-of e1 env store)))
                   (if (null? es)
                     answ
                     (value-of-begins (car es) (cdr es) (get-store answ)))))))
            (value-of-begins exp1 exps store)))

        (newref-exp (exp1)
          (let* ((a1 (value-of exp1 env store))
                 (a2 (newref (get-store a1) (get-value a1))))
            (make-answer (ref-val (get-value a2)) (get-store a2))))

        (deref-exp (exp1)
          (let ((answ (value-of exp1 env store)))
            (let ((ref1 (expval->ref (get-value answ))))
              (make-answer (deref (get-store answ) ref1) (get-store answ)))))

        (setref-exp (exp1 exp2)
          (let* ((a1 (value-of exp1 env store))
                 (ref (expval->ref (get-value a1)))
                 (a2 (value-of exp2 env (get-store a1)))
                 (new-store (setref (get-store a2) ref (get-value a2))))
            (make-answer (num-val 23) new-store)))
        )))

  ;; apply-procedure : Proc * ExpVal * Store -> AnswerOf(ExpVal)
  ;; SW: added store
  ;;
  ;; uninstrumented version
  ;;   (define apply-procedure
  ;;    (lambda (proc1 arg)
  ;;      (cases proc proc1
  ;;        (procedure (bvar body saved-env)
  ;;          (value-of body (extend-env bvar arg saved-env))))))

  ;; instrumented version
  (define apply-procedure
    (lambda (proc1 arg store)
      (cases proc proc1
        (procedure (var body saved-env)
	  (let ((r arg))
	    (let ((new-env (extend-env var r saved-env)))
	      (if (instrument-let)
		(begin
		  (eopl:printf
		    "entering body of proc ~s with env =~%"
		    var)
		  (pretty-print (env->list new-env))
                  (eopl:printf "store =~%")
                  (pretty-print (store->readable (get-store-as-list)))
                  (eopl:printf "~%"))
              (value-of body new-env store))))))))


  ;; store->readable : Listof(List(Ref,Expval)) 
  ;;                    -> Listof(List(Ref,Something-Readable))
  (define store->readable
    (lambda (l)
      (map
        (lambda (p)
          (cons
            (car p)
            (expval->printable (cadr p))))
        l)))
 
  )
  


  
