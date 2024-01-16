;; Die ersten drei Zeilen dieser Datei wurden von DrScheme eingefügt. Sie enthalten Metadaten
;; über die Sprachebene dieser Datei in einer Form, die DrScheme verarbeiten kann.
#reader(lib "DMdA-advanced-reader.ss" "deinprogramm")((modname mutable-test-demo) (read-case-sensitive #f) (teachpacks ()) (deinprogramm-settings #(#f write repeating-decimal #f #t none datum #f ())))
;----------------------------------------------------------------------
; Beispiel fuer Tests mit mutablen Daten ohne globale Variablen.
; 
; Getestet wird eine Prozedur, die einen mutablen Zaehler erhoeht.
; 
; Falls Sie dies im BP lesen: fuer die Aufgaben des 10. BP-Blatts
; brauchen Sie keine solchen Zaehler. Das einzige, was Sie aus diesem
; Quellcode brauchen koennen, ist die Idee, wie man ohne globale mutable
; Daten testet.
;----------------------------------------------------------------------

; ein beschraenkter Zaehler ist ein Zaehler mit Obergrenze (natural)
; und mutablem Zaehlerstand (natural).
; er zaehlt bei 0 los und erreicht maximal max-value. 
(define-record-procedures-2  bounded-counter 
  really-make-bounded-counter bounded-counter?
  (bounded-counter-max-value (get-bounded-counter-value set-bounded-counter-value!)))

; erzeugt einen Zaehler, der mit 0 initialisiert ist und das Maximum max hat.
(: make-counter (natural -> bounded-counter))
(define make-counter
  (lambda (max) 
    (really-make-bounded-counter max 0)))

; versucht, den Wert im Zaehler zu erhoehen. Wenn er schon den maximalen Wert hat,
; bleibt er auf Maximum; sonst wird er um 1 erhoeht (Seiteneffekt). 
; Gibt #t zurueck genau dann wenn der Wert erhoeht wurde.
(define increase-counter! 
  (lambda (ctr)
    ...))
          
    
; Testfall fuer increase-counter!
; Testdaten erzeugen mit let oder let*, Schritte des Tests mit begin
; Test: nach einmaligem Erhoehen soll der Zaehler 1 enthalten
(check-expect 
 (let* ((myctr (make-counter 2)))
   (begin (increase-counter! myctr)
          (get-bounded-counter-value myctr)))
 1)

; Testfall fuer increase-counter!
; Test: increase-counter! erhoeht nicht ueber das Maximum hinaus.
(check-expect 
 (let* ((myctr (make-counter 2)))
   (begin (increase-counter! myctr)
          (increase-counter! myctr)
          (increase-counter! myctr)
          (get-bounded-counter-value myctr)))
 2)

; Testfall fuer increase-counter!
; Test: der dritte increase-counter!-Aufruf auf einem frischen Zaehler mit Maximum 2
; soll #f zurueckliefern
(check-expect 
 (let* ((myctr (make-counter 2)))
   (begin (increase-counter! myctr)
          (increase-counter! myctr)
          (increase-counter! myctr)))
 #f)

; Maechtige, manchmal zu maechtige Variante:
; Test: nach dreimaligem Erhoehen soll der Zaehler 2 enthalten, und der letzte 
; increase-counter!-Aufruf soll #f zurueckliefern. 
(check-expect 
 (let* ((myctr (make-counter 2))
        (_egal1 (increase-counter! myctr))
        (_egal2 (increase-counter! myctr))
        (return-of-3rd-call (increase-counter! myctr))
        (value-after (get-bounded-counter-value myctr)))
   (list return-of-3rd-call value-after)) ; zwei Werte zum Pruefen abgreifen
 
 ; und mit Erwartungswerten vergleichen -- geht nicht mit check-within
 ; und verleitet zum Schreiben von Riesen-Tests, die zu viel auf einmal tun! Sparsam einsetzen!
 (list #f 2))



    