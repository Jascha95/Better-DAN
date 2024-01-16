;; Die ersten drei Zeilen dieser Datei wurden von DrScheme eingefügt. Sie enthalten Metadaten
;; über die Sprachebene dieser Datei ein einer Form, die DrScheme verarbeiten kann.
#reader(lib "DMdA-beginner-reader.ss" "deinprogramm")((modname 20071108-info1-2) (read-case-sensitive #f) (teachpacks ()) (deinprogramm-settings #9(#f write repeating-decimal #t #t none explicit #f ())))
; Die Identitat: Argument unverandert zuruckgeben
; identity : value -> value
(define identity
  (lambda (x)
    x))
; Die konstante Funktion: Das erste Argument unverandert zuruckgeben
; const : value value -> value
(define const
  (lambda (x y)
    x))

; Projektion: ein Argument auswahlen
; proj : number value value -> value
(define proj
  (lambda (i x1 x2)
    (cond
      ((= i 1) x1)
      ((= i 2) x2))))


; Ein Paar ist ein Wert
; (make-pair a b)
; wobei a und b jeweils beliebige Werte sind.

(define-record-procedures pair
  make-pair pair?
  (first rest))

; make-pair :  A B -> pair(A,B)
; A ||-> value
; B ||-> list
; make-pair : value list -> pair(value,list)

; A || -> A
; B ||-> list(A)
; make-pair : A list(A) -> pair(A, list(A))

(define liste-0 empty)
(define liste-1 (make-pair 1 liste-0))
(define liste-2 (make-pair 2 liste-1))
(define liste-3 (make-pair 3 liste-2))

; Alle Elemente einer Liste aufaddieren
; list-sum : list(number) -> number
(define list-sum
  (lambda (xs)
    (cond
      ((empty? xs)
       0)
      ((pair? xs)
       (+ (first xs)
          (list-sum (rest xs)))))))
