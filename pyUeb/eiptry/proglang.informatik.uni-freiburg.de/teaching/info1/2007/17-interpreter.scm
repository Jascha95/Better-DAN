;;; Mini-Scheme

; p ::= f*

; f ::= d | e

; d ::= (define v e)

; e ::= v
;     | (lambda (v1 ... vn) e)
;     | (e e1 ... en)
;     | (quote r)
;     | (if e e e)
;     | (set! v e)
;     | (begin e1 ... en)

; program-run : p env -> list(value)
(define program-run
  (lambda (f* env)
    (if (empty? f*)
        empty
	(let ((f (first f*))
	      (f* (rest f*)))
	  (if (definition? f)
	      (begin (evaluate-definition f env)
		     (program-run f* env))
	      (let* ((v (evaluate-expression f env))
		     (v* (program-run f* env)))
		(make-pair v v*)))))))

; evaluate-definition : d env -> unspecified
(define evaluate-definition
  (lambda (d env)
    (let ((x (definition-variable d))
	  (e (definition-expression d)))
      (let ((v (evaluate-expression e env)))
	(env-extend env x v)))))

; evaluate-expression : e env -> value
(define evaluate-expression
  (lambda (e env)
    (cond
     ((expression-variable? e)
      (env-lookup env e))
     ((expression-literal? e)
      e)
     ((expression-quote? e)
      (quote-value e))
     ((expression-if? e)
      (let ((c (if-condition e))
	    (t (if-consequent e))
	    (f (if-alternative e)))
	(if (evaluate-expression c env)
	    (evaluate-expression t env)
	    (evaluate-expression f env))))
     ((expression-lambda? e)
      (let ((x* (lambda-vars e))
	    (body (lambda-body e)))
	(make-procedure body x* env)))
     (else ;must be an application
	    (let ((e (application-rator e))
		  (e* (application-rands e)))
	      (let ((proc (evaluate-expression e env)))
		(let ((args (map (lambda (e) (evaluate-expression e env)) e*)))
		  (apply-procedure proc args))))))))

;(define apply-procedure
;  (lambda (proc args)
;    (apply proc args)))

;(define make-procedure
;  (lambda (body x* env)
;    (lambda v*
;      (let ((env (env-extend* env x* v*)))
;	(evaluate-expression body env)))))

(define-record-procedures-2 closure
  make-closure closure?
  (closure-exp closure-vars closure-env))

(define make-procedure
  make-closure)

(define apply-procedure
  (lambda (proc args)
    (cond
     ((closure? proc)
      (let ((body (closure-exp proc))
	    (x*   (closure-vars proc))
	    (env  (closure-env proc)))
	(let ((env (env-extend* env x* args)))
	  (evaluate-expression body env))))
     (else
      (apply proc args)))))

; syntax tests
(define test-form
  (lambda (sym)
    (lambda (exp)
      (and (pair? exp)
	   (equal? (first exp) sym)))))

(define definition? (test-form 'define))

(define expression-variable? symbol?)
(define expression-literal? (lambda (exp)
			      (or (number? exp)
				  (boolean? exp)
				  (string? exp))))
(define expression-lambda? (test-form 'lambda))
(define expression-quote? (test-form 'quote))
(define expression-if? (test-form 'if))
(define expression-set? (test-form 'set!))
(define expression-begin? (test-form 'begin))

; selectors for syntax
(define definition-variable (lambda (def) (first (rest def))))
(define definition-expression (lambda (def) (first (rest (rest def)))))

(define variable-name (lambda (varexp) varexp))
(define literal-value (lambda (litexp) litexp))
(define lambda-vars (lambda (lamexp) (first (rest lamexp))))
(define lambda-body (lambda (lamexp) (first (rest (rest lamexp)))))
(define quote-value (lambda (quoexp) (first (rest quoexp))))
(define if-condition (lambda (ifexp) (first (rest ifexp))))
(define if-consequent (lambda (ifexp) (first (rest (rest ifexp)))))
(define if-alternative (lambda (ifexp) (first (rest (rest (rest ifexp))))))
(define set-variable (lambda (setexp) (first (rest setexp))))
(define set-expression (lambda (setexp) (first (rest (rest setexp)))))
(define begin-expressions (lambda (begexp) (rest begexp)))
(define application-rator (lambda (appexp) (first appexp)))
(define application-rands (lambda (appexp) (rest appexp)))

; an entry is a pair of a variable name and a value
(define-record-procedures-2 entry
  make-entry entry?
  (entry-var entry-value))

; a frame is a pair of an enclosing frame (or #f for the toplevel frame) and
; an (updateable) list of entries
(define-record-procedures-2 frame
  make-frame frame?
  (frame-enclosing (frame-entries set-frame-entries!)))

; a location is an updateable value
(define-record-procedures-2 location
  make-location location?
  ((location-content set-location-content!)))

; extend the environment with a new entry (destructively)
(define env-extend
  (lambda (env x v)
    (set-frame-entries!
     env (make-pair (make-entry x v)
		    (frame-entries env)))))

; extend the environment with a new entry group (non-destructively)
(define env-extend*
  (lambda (env x* v*)
    (let ((env (make-frame env (list))))
      (letrec ((loop
		(lambda (x* v*)
		  (if (empty? x*)
		      env
		      (begin (env-extend env (first x*) (first v*))
			     (loop (rest x*) (rest v*)))))))
	(loop x* v*)))))

; lookup an entry in the environment
; env-lookup : env var -> value
(define env-lookup
  (lambda (env x)
    (let ((env (frame-enclosing env))
	  (entries (frame-entries env)))
      (letrec ((loop
		(lambda (entries)
		  (if (empty? entries)
		      (env-lookup env x)
		      (let ((entry (first entries))
			    (entries (rest entries)))
			(if (equal? x (entry-var entry))
			    (entry-value entry)
			    (loop entries)))))))
	(loop entries)))))


; values
(define make-unspecified
  (lambda ()
    #f))

; initiale Umgebung
(define initial-env
  (make-frame #f 
              (list (make-entry '+ +)
                    (make-entry '- -)
                    (make-entry '* *)
                    (make-entry '/ /)
                    (make-entry '= =)
                    (make-entry 'odd? odd?)
                    (make-entry 'not not)
                    (make-entry 'zero? zero?))))

; Interpreter mit locations
; eval-exp : expression env -> value
(define eval-exp
  (lambda (exp env)
    (letrec
	((eval
	  (lambda (exp)
	    (cond
	     ((expression-variable? exp)
	      (location-content (env-lookup env (variable-name exp))))
	     ((expression-literal? exp)
	      (literal-value exp))
	     ((expression-if? exp)
	      (if (eval (if-condition exp))
		  (eval (if-consequent exp))
		  (eval (if-alternative exp))))
	     ((expression-lambda? exp)
	      (let ((vars (lambda-vars exp))
		    (body (lambda-body exp)))
		(make-procedure body vars env)))
	     ((expression-begin? exp)
	      (let ((e* (begin-expressions exp)))
		(letrec ((loop
			  (lambda (e* v)
			    (if (empty? e*)
				v
				(let ((v (eval (first e*))))
				  (loop (rest e*) v))))))
		  (loop e* (make-unspecified)))))
	     ((expression-set? exp)
	      (let ((x (set-variable exp))
		    (e (set-expression exp)))
		(let* ((val (eval e))
		       (loc (env-lookup env x)))
		  (set-location-content! loc val))))
	     (else			;must be application
	      (let ((rator (eval (application-rator exp)))
		    (rands (map eval (application-rands exp))))
		(apply-procedure-loc rator rands)))))))
       (eval exp))))

; create a binding from a variable to a location filled with an initial value
; make-binding : variable value -> entry
(define make-binding
  (lambda (var arg)
    (make-entry var (make-location arg))))

; apply a procedure to a list of arguments
; apply-procedure-loc : procedure list(value) -> value
(define apply-procedure-loc
  (lambda (proc args)
    (cond
     ((closure? proc)
      (let ((body (closure-exp proc))
	    (vars (closure-vars proc))
	    (env  (closure-env proc)))
	(let ((new-env
	       (make-frame
		env
		(map make-binding vars args))))
	  (eval-exp body new-env))))
     (else
      (apply proc args)))))

; initiale Umgebung mit locations
(define initial-env-loc
  (make-frame #f 
              (list (make-binding '+ +)
                    (make-binding '- -)
                    (make-binding '* *)
                    (make-binding  '/ /)
                    (make-binding '= =)
                    (make-binding 'odd? odd?)
                    (make-binding 'not not)
                    (make-binding 'zero? zero?))))

