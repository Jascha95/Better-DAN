(module environments-sw (lib "eopl.ss" "eopl") 
  
  ;; builds environment interface, using data structures defined in
  ;; data-structures.scm. 

  (require "data-structures-sw.scm")

  (provide init-env empty-env extend-env apply-env)

;;;;;;;;;;;;;;;; initial environment ;;;;;;;;;;;;;;;;
  
  ;; init-env : () -> Env
  ;; usage: (init-env) = [i=1, v=5, x=10]
  ;; (init-env) builds an environment in which i is bound to the
  ;; expressed value 1, v is bound to the expressed value 5, and x is
  ;; bound to the expressed value 10.
  ;; Page: 69

  (define init-env 
    (lambda ()
      (extend-env 
       'i (num-val 1)
       (extend-env
        'v (num-val 5)
        (extend-env
         'x (num-val 10)
         (empty-env))))))

;;;;;;;;;;;;;;;; environment constructors and observers ;;;;;;;;;;;;;;;;

  ;; SW
  ;; lookup-proc-binding : Var * ListOf(ProcBinding) -> ProcBinding + 'undefined
  ;; (lookup-procbinding f lst) search for a procedure with name f in lst. 
  ;; Returns 'undefined if no such procedure exists.
  (define lookup-proc-binding
    (lambda (f lst)
      (if (null? lst)
          'undefined
          (cases proc-binding (car lst)
            (procedure-binding (id vars body)
              (if (eq? id f)
                  (car lst)
                  (lookup-proc-binding f (cdr lst))))))))
  
  ;; Page: 86
  (define apply-env
    (lambda (env search-sym)
      (cases environment env
        (empty-env ()
          (eopl:error 'apply-env "No binding for ~s" search-sym))
        (extend-env (var val saved-env)
	  (if (eqv? search-sym var)
	    val
	    (apply-env saved-env search-sym)))
        (extend-env-rec (procs saved-env)
          ;; SW
          (let ((lookup-res (lookup-proc-binding search-sym procs)))
            (if (proc-binding? lookup-res)
                (cases proc-binding lookup-res
                  (procedure-binding (id vars body)
                    (proc-val (procedure vars body env))))      
                (apply-env saved-env search-sym)))))))
    
  )