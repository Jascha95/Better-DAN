;; Die ersten drei Zeilen dieser Datei wurden von DrScheme eingefügt. Sie enthalten Metadaten
;; über die Sprachebene dieser Datei in einer Form, die DrScheme verarbeiten kann.
#reader(lib "DMdA-vanilla-reader.ss" "deinprogramm")((modname 20091222-fraktal) (read-case-sensitive #f) (teachpacks ((lib "image.ss" "teachpack" "deinprogramm") (lib "turtle.ss" "teachpack" "deinprogramm"))) (deinprogramm-settings #(#f write repeating-decimal #t #t none explicit #f ((lib "image.ss" "teachpack" "deinprogramm") (lib "turtle.ss" "teachpack" "deinprogramm")))))
; sierpinski Dreieck
(: sierpinski (natural real -> image))
(define sierpinski
  (lambda (n h)
    (if (zero? n)
        (triangle h "solid" "black")
        (let ((sn (sierpinski (- n 1) (/ h 2))))
          (above sn 
                 (beside sn sn "center")
                 "center")))))

; Kastenfraktal
(: box (natural real -> image))
(define box 
  (lambda (n h)
    (if (zero? n)
        (rectangle h h "solid" "black")
        (let ((bn (box (- n 1) (/ h 3))))
          (above bn (above
                     (beside bn (beside bn bn "center") "center")
                     bn "center")
                 "center")))))

; Kochkurve
(: koch (natural real -> turtle))
(define koch
  (lambda (n r)
    (if (zero? n)
        (draw r)
        (let ((kn (koch (- n 1) (/ r 3))))
          (sequence kn
                    (turn 60)
                    kn
                    (turn -120)
                    kn
                    (turn 60)
                    kn)))))