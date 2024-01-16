;; Die ersten drei Zeilen dieser Datei wurden von DrScheme eingefügt. Sie enthalten Metadaten
;; über die Sprachebene dieser Datei in einer Form, die DrScheme verarbeiten kann.
#reader(lib "DMdA-advanced-reader.ss" "deinprogramm")((modname 20100114-interpreter) (read-case-sensitive #f) (teachpacks ((lib "world.ss" "teachpack" "deinprogramm"))) (deinprogramm-settings #(#f write repeating-decimal #f #t none datum #f ((lib "world.ss" "teachpack" "deinprogramm")))))
; variable
; Typprädikat
(define expression-variable?
  symbol?)
; Selektor
(: variable-name (symbol -> symbol))
(define variable-name
  (lambda (x) x))

; literals
; Typprädikat
(define expression-literal?
  (lambda (x)
    (or (number? x) (string? x) (boolean? x))))
; Selektor
(define literal-value
  (lambda (x) x))

; Test für zusammengestzte Ausdrücke
(define test-form
  (lambda (sym)
    (lambda (x)
      (and (pair? x) 
           (equal? (first x) sym)))))

; Quote
; Prädikat
(define expression-quote?
  (test-form 'quote))
; Selektor
(define quote-value
  (lambda (exp)
    (first (rest exp))))

; Conditional
; Prädikat
(define expression-if?
  (test-form 'if))
; Selektoren
(define if-condition
  (lambda (x) (first (rest x))))
(define if-consequent
  (lambda (x) (first (rest (rest x)))))
(define if-alternative
  (lambda (x) (first (rest (rest (rest x))))))

; Funktionsanwendung
; kein Prädikat
; Selektoren
(define application-rator
  first)
(define application-rands
  (lambda (x) (rest x)))

; Prozeduranwendung
(define apply-procedure-0
  (lambda (fun args)
    (apply fun args)))

(define-record-procedures-2 entry
  make-entry entry?
  (entry-var entry-value))

(define-record-procedures-2 frame
  make-frame frame?
  (frame-enclosing (frame-entries set-frame-entries!)))

; lookup variable value in environment
(: lookup (symbol frame -> %v))
(define lookup
  (lambda (v f)
    (if (frame? f)
        (letrec ((look
                  (lambda (entries)
                    (if (empty? entries)
                        (lookup v (frame-enclosing f))
                        (let ((entry (first entries))
                              (entries (rest entries)))
                          (if (equal? (entry-var entry) v)
                              (entry-value entry)
                              (look entries)))))))
          (look (frame-entries f)))
        (violation (string-append "variable "
                                  (symbol->string v)
                                  " not found")))))

; Referenzen
(define-record-procedures-parametric-2 ref ref-of
  make-ref ref?
  ((get-ref set-ref!)))


; Initiale Umgebung mit vordefinierten Bezeichnern
(: initial-env frame)
(define initial-env
  (make-frame #f ; kein umnschließender Gültigkeitsbereich
              (list (make-entry '+ +)
                    (make-entry '- -)
                    (make-entry '* *)
                    (make-entry '/ /)
                    (make-entry '= =)
                    (make-entry 'make-ref make-ref)
                    (make-entry 'get-ref get-ref)
                    (make-entry 'set-ref! set-ref!)
                    (make-entry 'zero? zero?))))

; Lambda Ausdrücke
; Prädikat
(define expression-lambda?
  (test-form 'lambda))
; Selektoren
(define lambda-vars
  (lambda (x) (first (rest x))))
(define lambda-body
  (lambda (x) (first (rest (rest x)))))



; Definition von Prozedurwerten
(define make-procedure-0
  (lambda (exp vars env)
    (lambda vals ;Liste der Parameterwerte, beliebig lang
      (eval-exp exp
                (make-frame env 
                            (map make-entry
                                 vars
                                 vals))))))

; Closures: Datenstruktur zur Repräsentatiuon von Funktionen
(define-record-procedures-2 closure
  make-closure closure?
  (closure-exp closure-vars closure-env))

; Def von Prozedurwerten
(define make-procedure
  (lambda (exp vars env)
    (make-closure exp vars env)))

; Funktionsanwendung
(define apply-procedure
  (lambda (fun vals)
    (if (closure? fun)
        (let ((exp (closure-exp fun))
              (vars (closure-vars fun))
              (env (closure-env fun)))
          (eval-exp exp
                    (make-frame env 
                                (map make-entry vars vals))))
        ; must be a primitive function
        (apply fun vals))))

; Syntax fü+r Toplevel Definitionen
; Prädikat
(define definition?
  (test-form 'define))
; Selektoren
(define definition-variable
  (lambda (e)(first (rest e))))
(define definition-expression
  (lambda (e) (first (rest ( rest e)))))

; Syntax für (begin ...)
; Prädikat
(define expression-begin?
  (test-form 'begin))
; Selektor
(define begin-expressions
  (lambda (b) (rest b)))

; Auswertung von Ausdrücken

(define eval-exp
  (lambda (exp env)
    (letrec 
        ((eval
          (lambda (exp)
            (cond
              ((expression-variable? exp)
               (lookup (variable-name exp) env))
              ((expression-literal? exp)
               (literal-value exp))
              ((expression-quote? exp)
               (quote-value exp))
              ((expression-if? exp)
               (if (eval (if-condition exp))
                   (eval (if-consequent exp))
                   (eval (if-alternative exp))))
              ((expression-begin? exp)
               (let ((exps (begin-expressions exp)))
                 (letrec ((loop
                           (lambda (exps result)
                             (if (empty exps)
                                 result
                                 (let ((val (eval (first exps))))
                                   (loop (rest exps) val))))))
                   (loop exps #f))))
              ((expression-lambda? exp)
               (make-procedure (lambda-body exp) (lambda-vars exp) env))
              (else
               (apply-procedure (eval (application-rator exp))
                                (map eval (application-rands exp)))))
            )))
      (eval exp))))

; erweitere frame um neue Bindung
(: env-extend (frame symbol %v -> %unspecified))
; Effekt: erweitere env
(define env-extend 
  (lambda (env x v)
    (let ((entries (frame-entries env)))
      (set-frame-entries! env (make-pair (make-entry x v)
                                         entries)))))

; Auswerten einer Definition
(: evaluate-definition ((predicate definition?) frame -> %unspecified))
; Effekt: erweitert env um neuen Eintrag
(define evaluate-definition
  (lambda (d env)
    (let ((x (definition-variable d))
          (e (definition-expression d)))
      (let ((v (eval-exp e env)))
        (env-extend env x v)))))

; Hauptfunktion des Interpreters
; (: run-program ((list form) -> (list %v)))
(define run-program 
  (lambda (forms)
    (let ((env (make-frame initial-env (list))))
      (letrec ((loop
                (lambda (forms)
                  (if (empty? forms)
                      empty
                      (let ((form (first forms))
                            (forms (rest forms)))
                        (if (definition? form)
                            (begin (evaluate-definition form env)
                                   (loop forms))
                            (make-pair (eval-exp form env)
                                       (loop forms))))))))
        (loop forms)))))


(define forms1 empty)
(define forms2 '(
                 (((lambda (f) (lambda (x) (f (f x))))
                   (lambda (y) (* 2 y))) 
                  10)
                 ))
(define forms3 '(
                 (((lambda (f) (lambda (x) (f (f x))))
                   (lambda (y) (* 2 y))) 
                  10)
                 (+ 17 4)
                 ))
(define forms4 '(
                 (define x 24)
                 (+ x 4)
                 ))
(define forms5 '(
                 (define inc (lambda (x) (+ x 1)))
                 (inc 24)
                 (inc 31)
                 ))
(define forms6 '(
                 (define fib (lambda (n)
                               (if (= n 0) 0
                                   (if (= n 1) 1
                                       (+ (fib (- n 1))
                                          (fib (- n 2)))))))
                 (fib 0)
                 (fib 1)
                 (fib 5)
                 (fib 6)
                 ))
(define forms7 '(
                 (define account (make-ref 50))
                 (get-ref account)
                 (set-ref! account 41)
                 (get-ref account)
                 
                 ))




























