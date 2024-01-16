;; Die ersten drei Zeilen dieser Datei wurden von DrScheme eingefügt. Sie enthalten Metadaten
;; über die Sprachebene dieser Datei in einer Form, die DrScheme verarbeiten kann.
#reader(lib "DMdA-advanced-reader.ss" "deinprogramm")((modname 20080205-info1) (read-case-sensitive #f) (teachpacks ()) (deinprogramm-settings #9(#f write repeating-decimal #t #t none datum #f ())))
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


; make-procedure : expr list(var) env -> (value ... -> value)
(define make-procedure
  (lambda (body vars env)
    (lambda vals
      (let ((new-env (make-frame env (map make-entry vars vals))))
        (evaluate-expression body new-env)))))

; environment = Umgebung
; gibt die Bindung von Variablennamen an Werte an


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
              (else
               (let ((op-value (eval (application-rator exp)))
                     (operands (map eval (application-rands exp))))
                 (apply-procedure op-value operands)))))))
      (eval exp))))

; wende eine Prozedur auf eine Liste von Argumenten an
; apply-procedure : (value ... -> value) list(value) -> value
(define apply-procedure
  (lambda (proc args)
    (apply proc args)))

; liefert den Wert einer Variablen
; variable-value : environment symbol -> value
(define variable-value
  env-lookup)

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

