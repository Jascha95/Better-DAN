;; Die ersten drei Zeilen dieser Datei wurden von DrScheme eingefügt. Sie enthalten Metadaten
;; über die Sprachebene dieser Datei in einer Form, die DrScheme verarbeiten kann.
#reader(lib "DMdA-vanilla-reader.ss" "deinprogramm")((modname PunkteListen) (read-case-sensitive #f) (teachpacks ((lib "image.ss" "teachpack" "deinprogramm") (lib "turtle.ss" "installed-teachpacks"))) (deinprogramm-settings #(#f write repeating-decimal #t #t none explicit #f ((lib "image.ss" "teachpack" "deinprogramm") (lib "turtle.ss" "installed-teachpacks")))))
(define nikolaus-haus (list (make-point 80   0 "blue")
                            (make-point 80  80 "blue")
                            (make-point  0  80 "blue")
                            (make-point  0   0 "blue")
                            (make-point 80  80 "blue")
                            (make-point 40 120 "red")
                            (make-point  0  80 "red")
                            (make-point 80   0 "blue")))

(define braun (make-color 139 69 19))

(define tree (list (make-point    0   0 (make-color 190 190 190)) ; 0
                   (make-point    7   0 braun) ; 1
                   (make-point    7  15 braun) ; 2
                   (make-point  105  15 "green") ; 3 
                   (make-point   28  75 "green") ; 4
                   (make-point   58  75 "green") ; 5
                   (make-point   16 117 "green") ; 6
                   (make-point   37 117 "green") ; 7
                   (make-point    0 155 "green") ; 8 
                   (make-point  -37 117 "green") ; 9
                   (make-point  -16 117 "green") ;10
                   (make-point  -58  75 "green") ;11 
                   (make-point  -28  75 "green") ;12
                   (make-point -105  15 "green") ;13
                   (make-point   -7  15 "green") ;14
                   (make-point   -7   0 braun) ;15
                   (make-point    0   0 braun) ;16
                   ))
