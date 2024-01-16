;; Die ersten drei Zeilen dieser Datei wurden von DrScheme eingefügt. Sie enthalten Metadaten
;; über die Sprachebene dieser Datei in einer Form, die DrScheme verarbeiten kann.
#reader(lib "DMdA-beginner-reader.ss" "deinprogramm")((modname |20081030|) (read-case-sensitive #f) (teachpacks ()) (deinprogramm-settings #(#f write repeating-decimal #t #t none explicit #f ())))
(make-chocolate-cookie 20 20)

(define my-cookie (make-chocolate-cookie 10 20 ))
my-cookie
(chocolate-cookie-chocolate my-cookie)
(chocolate-cookie-cookie my-cookie)
(define your-cookie (make-chocolate-cookie 20 30))

; gewicht eines Schokokeks
(: chocolate-cookie-weight (chocolate-cookie -> real))
(define chocolate-cookie-weight
  (lambda (cookie)
    (+ (chocolate-cookie-chocolate cookie)
       (chocolate-cookie-cookie cookie))))
(check-expect (chocolate-cookie-weight my-cookie) 30)
(check-expect (chocolate-cookie-weight your-cookie) 50)

; Eine kartesische Koordinate in Ebene ist ein Wert
;  (make-cartesian x y)
; wobei 
;   x : real 
;   y : real
; die Koordinatenwerte sind
(define-record-procedures cartesian
  make-cartesian cartesian?
  (cartesian-x cartesian-y))

(define origin (make-cartesian 0 0))
(define point1 (make-cartesian 3 4))

; abstand zum Koordinatenursprung bestimmen+
(: distance-to-0 (cartesian -> real))
(define distance-to-0
  (lambda (p)
    (distance-x-y-0 (cartesian-x p) (cartesian-y p))))
(check-expect (distance-to-0 (make-cartesian 0 0)) 0)
(check-within (distance-to-0 (make-cartesian 1 1)) 1.414 1e-3)

; abstand zum Ursprung aus x/y Koordinaten
(: distance-x-y-0 (real real -> real))
(define distance-x-y-0
  (lambda (x y)
    (sqrt (+ (* x x)(* y y)))))

; Records als Ergebnisse

; verschiebe ein kartesische Koordinate
(: cartesian-move (cartesian real real -> cartesian))
(define cartesian-move 
  (lambda (p dx dy)
    (make-cartesian (+ dx (cartesian-x p)) (+ dy (cartesian-y p)))))

(check-expect (cartesian-move origin 3 4) point1)

; gemischte Datentypen

(define-record-procedures jelly-cream-cookie
  make-jelly-cream-cookie jelly-cream-cookie?
  (jelly-cream-cookie-jelly jelly-cream-cookie-cream jelly-cream-cookie-cookie))

; gewicht eines j-c-c
(: jelly-cream-cookie-weight (jelly-cream-cookie -> real))
(define jelly-cream-cookie-weight
  (lambda (c)
    (+ (jelly-cream-cookie-jelly c) (jelly-cream-cookie-cream c) (jelly-cream-cookie-cookie c))))
(check-expect (jelly-cream-cookie-weight (make-jelly-cream-cookie 2 3 4)) 9)

; Ein Keks ist ...
; Sorte: cookie
(define-contract cookie (mixed chocolate-cookie jelly-cream-cookie))

; gewicht eines Keks
(: cookie-weight (cookie -> real))
(define cookie-weight
  (lambda(c)
    (cond
      ((chocolate-cookie? c)
       (chocolate-cookie-weight c))
      ((jelly-cream-cookie? c)
       (jelly-cream-cookie-weight c)))))
(check-expect (cookie-weight (make-chocolate-cookie 1 1)) 2)
(check-expect (cookie-weight (make-jelly-cream-cookie 1 2 3)) 6)

; Gemischte Daten erzeugen

; Bobs Lieblingskeks isst Schokokekse
; Philippas Lieblingskeks isst Marmeladen-Kreme-Kekse

; liefere jedem sein Lieblingskeks
(: favorite-cookie ((one-of "Bob" "Philippa") -> cookie))
(define favorite-cookie
  (lambda (person)
    (cond
      ((string=? person "Bob")
       (make-chocolate-cookie 50 1))
      ((string=? person "Philippa")
       (make-jelly-cream-cookie 1 1 10))
      (else 0))))

(check-expect (favorite-cookie "Bob") (make-chocolate-cookie 50 1))
(check-expect (favorite-cookie "Philippa")(make-jelly-cream-cookie 1 1 10))
(check-expect (favorite-cookie "Jim") my-cookie)
