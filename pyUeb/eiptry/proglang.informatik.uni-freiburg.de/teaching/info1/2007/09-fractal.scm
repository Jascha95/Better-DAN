;; Die ersten drei Zeilen dieser Datei wurden von DrScheme eingefügt. Sie enthalten Metadaten
;; über die Sprachebene dieser Datei in einer Form, die DrScheme verarbeiten kann.
#reader(lib "DMdA-vanilla-reader.ss" "deinprogramm")((modname 09-sierpinski) (read-case-sensitive #f) (teachpacks ((lib "world.ss" "teachpack" "deinprogramm"))) (deinprogramm-settings #9(#f write repeating-decimal #t #t none explicit #f ((lib "world.ss" "teachpack" "deinprogramm")))))
; die Sierpinski Fläche der Ordnung n

; Farbe der Sierpinski Flächen
; sp-color : color
(define sp-color "blue")

; S(0): gleichseitiges Dreieck
; S(n+1): 
;   3 Kopien von S(n)
;   jeweils halb so hoch
;   zwei zentriert nebeneinander, eine zentriert darüber
; zeichne Sierpinski der Ordnung n mit gegebener Höhe
; sierpinski : nat nat -> image
(define sierpinski
  (lambda (n h)
    (if (= n 0)
        (triangle h "solid" sp-color)
        (let ((sn-1 (sierpinski (- n 1) (/ h 2))))
          (above 
           sn-1
           (beside
            sn-1 sn-1 "center")
           "center")))))

; Variation mit Quadrat
; sq : nat nat -> image
(define sq
  (lambda (n h)
    (if (= n 0)
        (rectangle h h "solid" sp-color)
        (let ((sn-1 (sq (- n 1) (/ h 2))))
          (above sn-1 (beside sn-1 sn-1 "center") "center")))))

; ****************************************************************
; das Box Fraktal: 
;  box(0) : Quadrat
;  box(n+1) : fünf Kopien von box(n) angeordnet zu einem Kreuz
; box : nat nat -> image
(define box
  (lambda (n h)
    (if (= n 0)
        (rectangle h h "solid" sp-color)
        (let ((sn-1 (box (- n 1) (/ h 3))))
          (above sn-1 
                 (above
                  (beside sn-1 (beside sn-1 sn-1 "center") "center")
                  sn-1
                  "center")
                 "center")))))

; ********************************************************
(define koch-w 601)
(define koch-h 601)
(define delta-theta-deg 60)
(define delta-theta (* 4 (atan 1) (/ delta-theta-deg 180)))
(define cos-theta (cos delta-theta))
(define sin-theta (sin delta-theta))

; die Koch Kurve
;  koch(0) : Linie
;  koch(n+1) : ersetze jede Linie in koch(n) durch _/\_
(define koch 
  (lambda (n x0 y0 r theta)
    (let* ((dx (* r (cos theta)))
           (dy (* r (sin theta)))
           (x1 (+ x0 dx))
           (y1 (+ y0 dy)))
    (if (= n 0)
        (line koch-w koch-h x0 y0 x1 y1 sp-color)
        (let* ((xm1 (+ x0 (/ dx 3)))
               (ym1 (+ y0 (/ dy 3)))
               (xm2 (- x1 (/ dx 3)))
               (ym2 (- y1 (/ dy 3)))
               (r1 (/ r 3))
               (theta1 (- theta delta-theta))
               (dx1 (* r1 (cos theta1)))
               (dy1 (* r1 (sin theta1)))
               (xp (+ xm1 dx1))
               (yp (+ ym1 dy1))
               (theta2 (+ theta delta-theta)))
          (overlay
           (overlay (koch (- n 1) x0 y0 r1 theta)
                    (koch (- n 1) xm2 ym2 r1 theta)
                    0 0)
           (overlay (koch (- n 1) xm1 ym1 r1 theta1)
                    (koch (- n 1) xp yp r1 theta2)
                    0 0)
           0 0))))))

; ******************************************************************
; die Hilbert-Kurve

; Eine Direction ist ein Wert
;  (make-dir dx dy)
; wobei dx, dy in {0, 1, -1} sind.
; Es ist immer eins von dx, dy 0, niemals beide.
(define-record-procedures dir
  make-dir dir?
  (dir-dx dir-dy))

; Eine Position ist ein Wert
;  (make-pos x y)
; wobei x, y : number, intendiert als 2d-Koordinaten.
(define-record-procedures pos
  make-pos pos?
  (pos-x pos-y))

; links abbiegen
; turn-left : dir -> dir
(define turn-left
  (lambda (dir)
    (make-dir (dir-dy dir) (- (dir-dx dir)))))

; rechts abbiegen
; turn-right : dir -> dir
(define turn-right
  (lambda (dir)
    (make-dir (- (dir-dy dir)) (dir-dx dir))))

; dir-upwards : dir
(define dir-upwards (make-dir 0 -1))

; bewege eine Position um einen Betrag in die angegebene Richtung
; move : pos num dir
(define move
  (lambda (p s d)
    (make-pos (+ (pos-x p) (* s (dir-dx d)))
              (+ (pos-y p) (* s (dir-dy d))))))



(define hilbert-color "black")
(define hilbert-w 512)
(define hilbert-h 512)

(define hilbert-line
  (lambda (p0 p1)
    (line hilbert-w hilbert-h
          (pos-x p0) (pos-y p0)
          (pos-x p1) (pos-y p1)
          hilbert-color)))

; Hilfsfunktion zum Zeichnen der Hilbertkurve
; hilbert : nat pos number dir (dir -> dir) (dir -> dir) image (pos image -> image) -> image
(define hilbert 
  (lambda (n p0 s d0 tl tr img cont)
    (if (= n 0)
        (cont p0 img)
        (let* ((d1 (tr d0))
               (d2 (tr d1)))
          (hilbert
           (- n 1) p0 s (tr d0) tr tl img
           (lambda (p0 img)
             (let* ((p1 (move p0 s d0))
                    (img1 (overlay img (hilbert-line p0 p1) 0 0)))
               (hilbert 
                (- n 1) p1 s d0 tl tr img1
                (lambda (p1 img)
                  (let* ((p2 (move p1 s d1))
                         (img2 (overlay img (hilbert-line p1 p2) 0 0)))
                    (hilbert
                     (- n 1) p2 s d0 tl tr img2
                     (lambda (p2 img)
                       (let* ((p3 (move p2 s d2))
                              (img3 (overlay img (hilbert-line p2 p3) 0 0)))
                         (hilbert
                          (- n 1) p3 s (tr d2) tr tl img3 
                          cont))))))))))))))

; berechne die Hilbertkurve der Ordnung n
; hilbert-start : nat -> image
(define hilbert-start
  (lambda (n)
    (let* ((s (hilbert-start-size n))
           (p (hilbert-start-position s n)))
      (hilbert n p s dir-upwards 
               turn-left turn-right
               (rectangle hilbert-w hilbert-h "solid" "white")
               (lambda (p0 img)
                 img)))))

; berechne die Länge einer Linie in Hilbert(n)
; hilbert-start-size : nat -> number
(define hilbert-start-size 
  (lambda (n)
    (if (= n 0)
        hilbert-w
        (/ (hilbert-start-size (- n 1)) 2))))

; berechne die Startposition der Hilbertkurve
; hilbert-start-position : number nat -> pos
(define hilbert-start-position
  (lambda (s n)
    (make-pos (/ s 2) (- hilbert-h (/ s 2)))))