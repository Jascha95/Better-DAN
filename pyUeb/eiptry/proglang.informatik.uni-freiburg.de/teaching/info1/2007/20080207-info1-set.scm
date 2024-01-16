;; Die ersten drei Zeilen dieser Datei wurden von DrScheme eingefügt. Sie enthalten Metadaten
;; über die Sprachebene dieser Datei in einer Form, die DrScheme verarbeiten kann.
#reader(lib "DMdA-advanced-reader.ss" "deinprogramm")((modname 20080207-info1-set) (read-case-sensitive #f) (teachpacks ()) (deinprogramm-settings #9(#f write repeating-decimal #t #t none datum #f ())))
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

; Datentyp für closures besteht aus Prozedurrumpf, Variablen und Umgebung
(define-record-procedures-2 closure
  make-closure closure?
  (closure-exp closure-vars closure-env))

; a location is an updateable value
(define-record-procedures-2 location
  make-location location?
  ((location-content set-location-content!)))


; make-procedure : expr list(var) env -> closure
(define make-procedure
  make-closure)

; environment = Umgebung
; gibt die Bindung von Variablennamen an Werte an

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
	(env-extend env x (make-location v))))))

; extend the environment with a new entry (destructively)
(define env-extend
  (lambda (env x v)
    (set-frame-entries!
     env (make-pair (make-entry x v)
		    (frame-entries env)))))

; werte einen Mini-Scheme Ausdruck aus
; evaluate-expression : expr environment -> value
(define evaluate-expression
  (lambda (exp env)
    (letrec 
        ((eval 
          (lambda (exp)
            (cond
              ((expression-variable? exp)
               (variable-value env (variable-name exp)))
              ((expression-literal? exp)
               (literal-value exp))
              ((expression-if? exp)
               (if (eval (if-condition exp))
                   (eval (if-consequent exp))
                   (eval (if-alternative exp))))
              ((expression-lambda? exp)
               (make-procedure (lambda-body exp)
                               (lambda-vars exp)
                               env))
              ((expression-set? exp)
               (let ((var (set-variable exp))
                     (body (set-expression exp)))
                 (let ((v (eval body))
                       (loc (env-lookup env var)))
                   (set-location-content! loc v))))
              ((expression-begin? exp)
               (let ((e* (begin-expressions exp)))
		(letrec ((loop
			  (lambda (e* v)
			    (if (empty? e*)
				v
				(let ((v (eval (first e*))))
				  (loop (rest e*) v))))))
		  (loop e* #f))))
              (else
               (let ((op-value (eval (application-rator exp)))
                     (operands (map eval (application-rands exp))))
                 (apply-procedure op-value operands)))))))
      (eval exp))))

; wende eine Prozedur auf eine Liste von Argumenten an
; apply-procedure : (value ... -> value) list(value) -> value
(define apply-procedure
  (lambda (proc args)
    (cond
      ((closure? proc)
       (let ((body (closure-exp proc))
             (vars (closure-vars proc))
             (env (closure-env proc)))
         (let ((new-env (make-frame env (map make-binding vars args))))
           (evaluate-expression body new-env))))
      (else 
       (apply proc args)))
    ))

; make-binding : var value -> entry
(define make-binding
  (lambda (var val)
    (make-entry var (make-location val))))

; liefert den Wert einer Variablen
; variable-value : environment symbol -> value
(define variable-value
  (lambda (env exp)
    (location-content (env-lookup env exp))))

; initiale Umgebung
(define initial-env
  (make-frame #f 
              (list (make-binding '+ +)
                    (make-binding '- -)
                    (make-binding '* *)
                    (make-binding '/ /)
                    (make-binding '= =)
                    (make-binding 'odd? odd?)
                    (make-binding 'not not)
                    (make-binding 'zero? zero?))))
