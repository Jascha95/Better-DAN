;; Die ersten drei Zeilen dieser Datei wurden von DrScheme eingefügt. Sie enthalten Metadaten
;; über die Sprachebene dieser Datei in einer Form, die DrScheme verarbeiten kann.
#reader(lib "DMdA-assignments-reader.ss" "deinprogramm")((modname |20090113|) (read-case-sensitive #f) (teachpacks ((lib "world.ss" "teachpack" "deinprogramm"))) (deinprogramm-settings #(#f write repeating-decimal #t #t none explicit #f ((lib "world.ss" "teachpack" "deinprogramm")))))
(define-record-procedures-parametric-2 (ref X)
  make-ref ref?
  ((get-ref set-ref!)))

; Zustandsvariable: aktueller Kontostand
(: balance (ref real))
(define balance (make-ref 90))
; vom Konto einen gewissen Betrag abheben und anzeigen, ob dies m¨oglich war
(: withdraw (real -> boolean))
; Effekt: ver¨andert die Zustandsvariable in balance
(define withdraw
  (lambda (amount)
    (if (>= (get-ref balance) amount)
        (begin
          (set-ref! balance (- (get-ref balance) amount))
          #t)
        #f)))

; Ein Bankkonto ist ein Wert
; (make-account b)
; wobei b : real der Kontostand ist (ver¨anderlich)
(define-record-procedures-2
  account
  make-account account?
  ((account-balance set-account-balance!)))
; Den Kontostand ¨andern
(: set-account-balance! (account real -> %unspecified))
; Effekt: (set-account-balance! a n) setzt den Kontostand auf n
; Geld abheben und anzeigen, ob das m¨oglich war
(: account-withdraw (account real -> boolean))
; Effekt: (account-withdraw a n) ¨andert den Kontostand von a
(define account-withdraw
  (lambda (a n)
    (if (>= (account-balance a) n)
        (begin
          (set-account-balance! a (- (account-balance a) n))
          #t)
        #f)))