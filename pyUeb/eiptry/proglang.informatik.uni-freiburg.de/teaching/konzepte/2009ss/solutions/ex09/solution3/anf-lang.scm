(module anf-lang (lib "eopl.ss" "eopl")                
  
  ;; output language from the cps converter

  (require "drscheme-init.scm")
  (require "csp-in-lang.scm")
  ;; listof?
  ;; usage: checks whether a scheme value is a list such that
  ;;        all elements fulfill the predicate given
  (define listof?
    (lambda (pred?)
      (lambda (v)
        (if (list? v)
            (or (null? v) (and (pred? (car v)) (listof? (cdr v))))
            #f))))
  
  (define-datatype anf-atomic anf-atomic?
    (anf-number (number number?))
    (anf-var (var symbol?))
    (anf-proc 
     (vars (listof? symbol?))
     (body anf-exp?)))
  
  (define-datatype anf-simple anf-simple?
    (anf-atomic->simple (exp anf-atomic?))
    (anf-diff
     (e1 anf-atomic?)
     (e2 anf-atomic?))
    (anf-zero? (exp anf-atomic?)))
  
  (define-datatype anf-exp anf-exp?
    (anf-simple->exp (exp anf-simple?))
    (anf-let 
     (var symbol?)
     (exp anf-exp?)
     (body anf-exp?))
    (anf-letrec
     (fs (listof? symbol?))
     (xss (listof? (listof? symbol?)))
     (bodies (listof? anf-exp?))
     (body anf-exp?))
    (anf-if
     (e1 anf-simple?)
     (e2 anf-exp?)
     (e3 anf-exp?))
    (anf-app
     (rator anf-simple?)
     (rands (listof? anf-simple?))))
     
  (define cps-in->anf
    (lambda (exp)
      (cases expression exp
        (const-exp (n) (anf-simple->exp (anf-atomic->simple (anf-number n))))
        (diff-exp (e1 e2) ...)
                  
        (sum-exp (es) ...)
        (zero?-exp (e) ...)
        (if-exp (e1 e2 e3) ...)
        (letrec-exp (fs xss bodies body) ...)
        (var-exp (x) ...)
        (let-exp (x e1 e2) ...)
        (proc-exp (xs e) ...)
        (call-exp (e es) ...))))
  )
