;; Die ersten drei Zeilen dieser Datei wurden von DrScheme eingefügt. Sie enthalten Metadaten
;; über die Sprachebene dieser Datei in einer Form, die DrScheme verarbeiten kann.
#reader(lib "DMdA-beginner-reader.ss" "deinprogramm")((modname |20091029|) (read-case-sensitive #f) (teachpacks ((lib "world.ss" "teachpack" "deinprogramm"))) (deinprogramm-settings #(#f write repeating-decimal #t #t none explicit #f ((lib "world.ss" "teachpack" "deinprogramm")))))
; feststellen, ob Temperatur mild ist
(: temperature-mild? (real -> boolean))
(define temperature-mild?
  (lambda (t)
    (<= 4 t 12)))

(check-expect (temperature-mild? 0) #f)
(check-expect (temperature-mild? 10) #t)
(check-expect (temperature-mild? 15) #f)


; feststellen, ob Temperatur unangenehm
(: temperature-uncomfortable? (real -> boolean))
(define temperature-uncomfortable?
  (lambda (t)
    (not (<= -10 t 40))))

(check-expect (temperature-uncomfortable? 0) #f)
(check-expect (temperature-uncomfortable? 45) #t)


(define doppelkeks 
  (make-chocolate-cookie 22 28))

; Gewicht eines Schokokekses bestimmen
(: chocolate-cookie-weight (chocolate-cookie -> number))
(define chocolate-cookie-weight
  (lambda (c)
    (+ (chocolate-cookie-chocolate c)
       (chocolate-cookie-cookie c))))

(check-expect (chocolate-cookie-weight doppelkeks) 50)
(check-expect (chocolate-cookie-weight (make-chocolate-cookie 17 4)) 21)

; Punkte in der Ebene
(define-record-procedures cartesian
  make-cartesian cartesian?
  (cartesian-x cartesian-y))

(define origin (make-cartesian 0 0))
(define p1 (make-cartesian 3 4))

(define square (lambda (x) (* x x)))

; Abstand vom Ursprung bestimmen
(: distance-to-origin (cartesian -> number))
(define distance-to-origin
  (lambda (xy)
    (sqrt (+ (square (cartesian-x xy))
             (square (cartesian-y xy))))))

(check-expect (distance-to-origin origin) 0)
(check-expect (distance-to-origin p1) 5)

; Koordinate verschieben
(: cartesian-move (cartesian number number -> cartesian))
(define cartesian-move
  (lambda (c dx dy)
    (make-cartesian (+ dx (cartesian-x c))  (+ dy (cartesian-y c)))))

(check-expect (cartesian-move origin 3 4) p1)