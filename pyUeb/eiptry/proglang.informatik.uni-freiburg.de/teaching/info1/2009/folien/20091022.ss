;; Die ersten drei Zeilen dieser Datei wurden von DrScheme eingefügt. Sie enthalten Metadaten
;; über die Sprachebene dieser Datei in einer Form, die DrScheme verarbeiten kann.
#reader(lib "DMdA-beginner-reader.ss" "deinprogramm")((modname |20091022|) (read-case-sensitive #f) (teachpacks ((lib "world.ss" "teachpack" "deinprogramm"))) (deinprogramm-settings #(#f write repeating-decimal #t #t none explicit #f ((lib "world.ss" "teachpack" "deinprogramm")))))
(* (+ 2 2) (/ (* (+ 3 5) (/ 30 10)) 2))


(define answer (- (* 4 13) 10))

(define pi (* 4 (atan 1)))

(define x 13)

(lambda (x) (* x x))

(define square 
  (lambda (x) 
    (* x x)))

(square (+ 17 4))


; Fläche einer Scheibe berechnen
(: disk-area (number -> number))
(define disk-area
  (lambda (radius)
    (* pi (square radius))))
(check-expect (disk-area 0) 0)
(check-within (disk-area 1) 3.14159 1e-05)

; Volumen eines Zylinders berechnen
(: cylinder-volume (number number -> number))
(define cylinder-volume
  (lambda (radius height)
    (* height (disk-area radius))))
(check-within (cylinder-volume 1 1) 3.14159 1e-5)

