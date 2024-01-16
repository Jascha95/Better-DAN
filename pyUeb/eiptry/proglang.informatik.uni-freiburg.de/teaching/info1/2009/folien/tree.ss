;; Die ersten drei Zeilen dieser Datei wurden von DrScheme eingefügt. Sie enthalten Metadaten
;; über die Sprachebene dieser Datei in einer Form, die DrScheme verarbeiten kann.
#reader(lib "DMdA-vanilla-reader.ss" "deinprogramm")((modname xmastree) (read-case-sensitive #f) (teachpacks ((lib "image.ss" "teachpack" "deinprogramm"))) (deinprogramm-settings #(#f write repeating-decimal #t #t none explicit #f ((lib "image.ss" "teachpack" "deinprogramm")))))
(define wood
  (lambda (n)
    (triangle n "solid" "brown")))

(define leaf
  (lambda (n)
    (above (circle 2 "solid" "yellow")
    (above (rectangle 2 5 "solid" "red")
           (triangle n "solid" "green")
           "center")
    "center")))


(define stem
  (lambda (n s)
    (if (zero? n)
        (leaf s)
        (above (beside (stem (- n 1) (* 1/2 s))
                       (beside (stem (- n 1) (* 3/4 s))
                               (stem (- n 1)(* 1/2 s))
                               "bottom")
                       "bottom")
                       (wood s) "center"))))



(define stem2
  (letrec
      ((stem0 
        (lambda (n s)
          (let ((im (stem n s)))
            (overlay (rectangle (image-width im) (image-height im) "solid" "black")
                     im
                     "center" "center"))))
       (stem
        (lambda (n s)
          (if (zero? n)
              (leaf s)
              (above (overlay (stem (- n 1) (* 3/4 s))
                              (beside (stem (- n 1) (* 1/2 s))
                                      (pad (stem (- n 1)(* 1/2 s)) (round (* 3/16 s)) 0 0 0)
                                      "bottom")
                              "center" "bottom")
                     (wood s) "center")))))
    stem0))



(define grow
  (lambda (n s)
    (if (zero? n)
        (triangle s "solid" "green")
        (above (beside (overlay (grow (- n 1) (* 1/2 s))
                                (rectangle (* 1/2 s) 4 "solid" "brown") "left" "bottom")
                       (overlay (grow (- n 1) (* 1/2 s))
                                (rectangle (* 1/2 s) 4 "solid" "brown") "right" "bottom")
                       "bottom")
               (rectangle 4 s "solid" "brown")
               "center"))))
