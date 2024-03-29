(module data-structures-sw (lib "eopl.ss" "eopl")

  ;; data structures for letrec-lang.

  (require "lang-sw.scm")                  ; for expression?

  (provide (all-defined))               ; too many things to list

;;;;;;;;;;;;;;;; expressed values ;;;;;;;;;;;;;;;;

;;; an expressed value is either a number, a boolean or a procval.

  (define-datatype expval expval?
    (num-val
      (value number?))
    (bool-val
      (boolean boolean?))
    (proc-val 
      (proc proc?)))

;;; extractors:

  ;; expval->num : ExpVal -> Int
  (define expval->num
    (lambda (v)
      (cases expval v
	(num-val (num) num)
	(else (expval-extractor-error 'num v)))))

  ;; expval->bool : ExpVal -> Bool
  (define expval->bool
    (lambda (v)
      (cases expval v
	(bool-val (bool) bool)
	(else (expval-extractor-error 'bool v)))))

  ;; expval->proc : ExpVal -> Proc
  (define expval->proc
    (lambda (v)
      (cases expval v
	(proc-val (proc) proc)
	(else (expval-extractor-error 'proc v)))))

  (define expval-extractor-error
    (lambda (variant value)
      (eopl:error 'expval-extractors "Looking for a ~s, found ~s"
	variant value)))

;;;;;;;;;;;;;;;; procedures ;;;;;;;;;;;;;;;;

  ;; SW
  ;; list-of?
  ;; usage: checks whether a scheme value is a list such that
  ;;        all elements fulfill the predicate given
  (define list-of?
    (lambda (pred?)
      (lambda (v)
        (if (list? v)
            (or (null? v) (and (pred? (car v)) (list-of? (cdr v))))
            #f))))
  
  (define symbol-list? (list-of? symbol?))
     
  ;; proc? : SchemeVal -> Bool
  ;; procedure : ListOf(Var) * Exp * Env -> Proc
  (define-datatype proc proc?
    (procedure
      (bvars symbol-list?)  ;; SW
      (body expression?)
      (env environment?)))

  ;; SW
  (define-datatype proc-binding proc-binding?
    (procedure-binding
     (id symbol?)
     (bvars symbol-list?)
     (body expression?)))
  
  (define proc-binding-list? (list-of? proc-binding?))
  
  ;; Page: 86
  (define-datatype environment environment?
    (empty-env)
    (extend-env 
      (bvar symbol?)
      (bval expval?)
      (saved-env environment?))
    (extend-env-rec
     (procs proc-binding-list?) ;; SW
     (saved-env environment?)))

)