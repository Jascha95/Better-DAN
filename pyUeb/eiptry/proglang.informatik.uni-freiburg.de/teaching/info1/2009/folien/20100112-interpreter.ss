;; Die ersten drei Zeilen dieser Datei wurden von DrScheme eingefügt. Sie enthalten Metadaten
;; über die Sprachebene dieser Datei in einer Form, die DrScheme verarbeiten kann.
#reader(lib "DMdA-advanced-reader.ss" "deinprogramm")((modname 20100112-interpreter) (read-case-sensitive #f) (teachpacks ((lib "world.ss" "teachpack" "deinprogramm"))) (deinprogramm-settings #(#f write repeating-decimal #f #t none datum #f ((lib "world.ss" "teachpack" "deinprogramm")))))
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
(define apply-procedure
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
        (violation "variable not found"))))

; Initiale Umgebung mit vordefinierten Bezeichnern
(: initial-env frame)
(define initial-env
  (make-frame #f ; kein umnschließender Gültigkeitsbereich
              (list (make-entry '+ +)
                    (make-entry '- -)
                    (make-entry '* *)
                    (make-entry '/ /)
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
              ((expression-lambda? exp)
               (lambda vals ;Liste der Parameterwerte, beliebig lang
                 (eval-exp (lambda-body exp)
                           (make-frame env 
                                       (map make-entry
                                            (lambda-vars exp)
                                            vals)))))
              (else
               (apply-procedure (eval (application-rator exp))
                                (map eval (application-rands exp)))))
            )))
      (eval exp))))































