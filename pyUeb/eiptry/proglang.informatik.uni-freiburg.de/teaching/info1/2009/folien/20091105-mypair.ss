;; Die ersten drei Zeilen dieser Datei wurden von DrScheme eingefügt. Sie enthalten Metadaten
;; über die Sprachebene dieser Datei in einer Form, die DrScheme verarbeiten kann.
#reader(lib "DMdA-vanilla-reader.ss" "deinprogramm")((modname 20091105-mypair) (read-case-sensitive #f) (teachpacks ((lib "image.ss" "teachpack" "deinprogramm"))) (deinprogramm-settings #(#f write repeating-decimal #t #t none explicit #f ((lib "image.ss" "teachpack" "deinprogramm")))))
; Ein Paar von A und B ist ein Wert
; (make-mypair a b)
; wobei a und b jeweils Werte aus A bzw. B sind.

(define-record-procedures-parametric mypair mypair-of
  make-mypair mypair?
  (myfirst myrest))

; liefert Prozeduren mit folgenden Vertragen
(: make-mypair (%a %b -> (mypair-of %a %b)))
(: mypair? (%value -> boolean))
(: myfirst ((mypair-of %a %b) -> %a))
(: myrest ((mypair-of %a %b) -> %b))

; Testdaten
(: p1 (mypair-of integer string))
(define p1 (make-mypair 42 "marvin"))

(check-expect (myfirst p1) 42)
(check-expect (myrest p1) "marvin")

; Testdaten
;(: p2 (mypair-of integer string))
;(define p2 (make-mypair #t #f))
;(check-expect (myfirst p2) #t)

; Paar nur aus integer und string erzeugen
;(: mypair-maker (integer string -> (mypair-of integer string)))
;(define mypair-maker 
;  (lambda (i s) (make-mypair i s)))
;(check-expect (myfirst (mypair-maker #t #f)) #t)



; zweite Komponente von integer/string Paaren
;(: mypair-tester ((mypair-of integer string) -> string))
;(define mypair-tester
;  (lambda (p) (myrest p)))
;(check-expect (mypair-tester p2) #f)

; Eine Liste von As ist eins der folgenden
; - die leere Liste
; - ein Paar (aus einem A-Wert und einer Liste von As)
; Name: (mylist-of %a)

(define mylist (contract (mixed (predicate empty?) mypair)))

(: myempty mylist)
(define myempty empty)
(check-expect myempty empty)

(define mylist-of
  (lambda (A)
    (contract (mixed (predicate empty?)
                     (mypair-of A (mylist-of A))))))

(: myempty2 (mylist-of integer))
(define myempty2 empty)
(check-expect myempty2 empty)

(: mylist1 (mylist-of integer))
(define mylist1 (make-mypair 42 empty))
(check-expect (myfirst mylist1) 42)

(define mylist2 (make-mypair "55" mylist1))

;(: mylist3 (mylist-of integer))
;(define mylist3 (make-mypair 100 mylist2))
;(check-expect (myfirst mylist3) 100)