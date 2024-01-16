;; Die ersten drei Zeilen dieser Datei wurden von DrScheme eingefügt. Sie enthalten Metadaten
;; über die Sprachebene dieser Datei in einer Form, die DrScheme verarbeiten kann.
#reader(lib "DMdA-beginner-reader.ss" "deinprogramm")((modname |20081023|) (read-case-sensitive #f) (teachpacks ()) (deinprogramm-settings #(#f write repeating-decimal #t #t none explicit #f ())))
; die Zahl pi
(: pi number)
(define pi (* 4 (atan 1)))

; eine Zahl quadrieren
(: square (number -> number))
(define square
  (lambda (x)
    (* x x)))
(check-expect (square 0) 0)
(check-expect (square 9) 81)
(check-expect (square -13) 169)
(check-expect (square 5/2) 25/4)

; berechne die Fläche eines Kreises
(: disk-area (number -> number))
(define disk-area
  (lambda (radius)
    (* pi radius radius)))

(check-expect (disk-area 0) 0)
(check-within (disk-area 1) 3.14159 1e-5)
(check-within (disk-area 2) 12.56637 1e-5)

; Parkplatzproblem lösen
(: cars-in-parking-lot (natural natural -> natural))
(define cars-in-parking-lot
  (lambda (nr-of-vehicles nr-of-wheels)
    (- (/ nr-of-wheels 2) nr-of-vehicles)))
; Testfälle
(check-expect (cars-in-parking-lot 0 0) 0)
(check-expect (cars-in-parking-lot 1 4) 1)
(check-expect (cars-in-parking-lot 2 4) 0)
(check-expect (cars-in-parking-lot 1 2) 0)
(check-expect (cars-in-parking-lot 3 9) 0)

