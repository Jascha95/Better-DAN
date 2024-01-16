;; Die ersten drei Zeilen dieser Datei wurden von DrScheme eingefügt. Sie enthalten Metadaten
;; über die Sprachebene dieser Datei in einer Form, die DrScheme verarbeiten kann.
#reader(lib "DMdA-vanilla-reader.ss" "deinprogramm")((modname 20081216-1) (read-case-sensitive #f) (teachpacks ((lib "world.ss" "teachpack" "deinprogramm"))) (deinprogramm-settings #(#f write repeating-decimal #t #t none explicit #f ((lib "world.ss" "teachpack" "deinprogramm")))))
(define sierpinski
  (lambda (n size)
    (if (zero? n)
        (triangle size "solid" "black")
        (let ((sn-1 (sierpinski (- n 1) (* 1/2 size))))
          (above sn-1
                 (beside sn-1 sn-1 "bottom")
                 "center")))))

(define kasten
  (lambda (n size)
    (if (zero? n)
        (rectangle (round size) (round size) "solid" "black")
        (let ((kn-1 (kasten (- n 1) (* 1/3 size))))
          (above kn-1
                 (above
                  (beside kn-1 (beside kn-1 kn-1 "center") "center")
                  kn-1 "center")
                 "center")))))