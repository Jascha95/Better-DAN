;; Die ersten drei Zeilen dieser Datei wurden von DrScheme eingefügt. Sie enthalten Metadaten
;; über die Sprachebene dieser Datei in einer Form, die DrScheme verarbeiten kann.
#reader(lib "DMdA-assignments-reader.ss" "deinprogramm")((modname lambda) (read-case-sensitive #f) (teachpacks ((lib "turtle.ss" "installed-teachpacks"))) (deinprogramm-settings #(#f write repeating-decimal #t #t none explicit #f ((lib "turtle.ss" "installed-teachpacks")))))
; Ein Lambda-Ausdruck ist eins der folgenden:
; - eine Variable (var)
; - eine Anwendung (app)
; - eine Abstraktion (abs)
(define-contract lambda-term (mixed var app abs))

; Eine Variable des Lambda-Kalkuels ist ein Wert
;   (make-var x)
; wobei x ein String ist, der den Namen der Variable wiedergibt.
(define-record-procedures var make-var var? (var-name))
(: make-var (string -> var))

; Eine Anwendung im Lambda-Kalkuel ist ein Wert
;   (make-app operator operand)
; wobei operator und operand zwei Lambda-Ausdruecke sind,
; die den Operator und den Operanden der Anwendung darstellen.
(define-record-procedures app make-app app? (app-operator app-operand))
(: make-app (lambda-term lambda-term -> app))

; Eine Abstraktion im Lambda-Kalkuel ist ein Wert
;   (make-abs x body)
; wobei x ein String ist, der den Namen der gebundenen Variable
; wiedergibt, und body ein Lambda-Ausdruck ist, der den Rumpf
; der Abstraktion darstellt.
(define-record-procedures abs make-abs abs? (abs-var-name abs-body))
(: make-abs (string lambda-term -> abs))

; Test ob ein Lambda-Ausdruck vorliegt.
(: lambda-term? (%a -> boolean))
(define lambda-term?
  (lambda (x)
    (or (var? x) (app? x) (abs? x))))

; Konvertiert einen Lambda-Ausdruck in einen String.
(: lambda-term->string (lambda-term -> string))
(define lambda-term->string
  (lambda (e)
    (cond
      ((var? e) (var-name e))
      ((app? e) (string-append "(" (lambda-term->string (app-operator e)) " "
                               (lambda-term->string (app-operand e)) ")"))
      ((abs? e) (string-append "(lambda(" (abs-var-name e) ") " 
                               (lambda-term->string (abs-body e)) ")")))))
