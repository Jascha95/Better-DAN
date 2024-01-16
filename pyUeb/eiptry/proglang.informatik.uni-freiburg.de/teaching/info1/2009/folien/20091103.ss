;; Die ersten drei Zeilen dieser Datei wurden von DrScheme eingefügt. Sie enthalten Metadaten
;; über die Sprachebene dieser Datei in einer Form, die DrScheme verarbeiten kann.
#reader(lib "DMdA-beginner-reader.ss" "deinprogramm")((modname |20091103|) (read-case-sensitive #f) (teachpacks ((lib "world.ss" "teachpack" "deinprogramm"))) (deinprogramm-settings #(#f write repeating-decimal #t #t none explicit #f ((lib "world.ss" "teachpack" "deinprogramm")))))
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

; Ein Marmelade-Creme-Keks ist ein Wert
; (make-jelly-cream-cookie x y z)
; wobei x, y und z Zahlen sind, die den Creme-, Marmeladen-
; bzw. Keks-Anteil darstellen.

(define-record-procedures jelly-cream-cookie
  make-jelly-cream-cookie jelly-cream-cookie?
  (jelly-cream-cookie-jelly jelly-cream-cookie-cream jelly-cream-cookie-cookie))

(define jcc-1 (make-jelly-cream-cookie 40 1 10))

; Gewicht eines Marmelade-Creme-Kekses
(: jelly-cream-cookie-weight (jelly-cream-cookie -> real))
(define jelly-cream-cookie-weight
  (lambda (c)
    (+ (jelly-cream-cookie-jelly c)
       (jelly-cream-cookie-cream c)
       (jelly-cream-cookie-cookie c))))
(check-expect (jelly-cream-cookie-weight jcc-1) 51)

; Ein Keks ist eine der folgenden Alternativen
; - ein Schokokeks
; - ein Marmelade-Creme-Keks
; Name: cookie
(define cookie
  (contract (mixed chocolate-cookie jelly-cream-cookie)))

; Gewicht eines Kekses bestimen
(: cookie-weight (cookie -> real))
(define cookie-weight
  (lambda (c)
    (cond
      ((chocolate-cookie? c)
       (chocolate-cookie-weight c))
      ((jelly-cream-cookie? c) 
       (jelly-cream-cookie-weight c)))))
(check-expect (cookie-weight doppelkeks) 50)
(check-expect (cookie-weight jcc-1) 51)









