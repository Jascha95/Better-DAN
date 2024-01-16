;; Die ersten drei Zeilen dieser Datei wurden von DrScheme eingefügt. Sie enthalten Metadaten
;; über die Sprachebene dieser Datei in einer Form, die DrScheme verarbeiten kann.
#reader(lib "DMdA-vanilla-reader.ss" "deinprogramm")((modname 20091126-higher-order) (read-case-sensitive #f) (teachpacks ((lib "image.ss" "teachpack" "deinprogramm"))) (deinprogramm-settings #(#f write repeating-decimal #t #t none explicit #f ((lib "image.ss" "teachpack" "deinprogramm")))))
; Ein Gast ist ein Wert
; (make-guest name sex veggie)
; wobei name : string
; sex : boolean (#t f ̈ur m ̈annlich, #f f ̈ur weiblich)
; veggie : boolean
(define-record-procedures guest
  make-guest guest?
  (guest-name guest-male? guest-veggie?))
(: make-guest (string boolean boolean -> guest))

; Gästeliste
(: guest-list (list guest))
(define guest-list
  (list (make-guest "Maike" #t #t)
        (make-guest "Hans-Peter" #t #f)
        (make-guest "Marlon" #f #t)
        (make-guest "Schneewittchen" #f #f)))

; welche Gäste sindf Vegetarier
(: filter-veggies ((list guest) -> (list guest)))
(define filter-veggies
  (lambda (guests)
    (if (empty? guests)
        empty
        (let ((guest (first guests))
              (guests (rest guests)))
          (if (guest-veggie? guest)
              (make-pair guest (filter-veggies guests))
              (filter-veggies guests))))))

; filtere die männlichen Gäste
(: filter-male ((list guest) -> (list guest)))
(define filter-male
  (lambda (guests)
    (if (empty? guests)
        empty
        (let ((guest (first guests))
              (guests (rest guests)))
          (if (guest-male? guest)
              (make-pair guest (filter-male guests))
              (filter-male guests))))))

; filtere eine Gästeliste gemäß einem Prädikat
(: filter-guest ((guest -> boolean)(list guest) -> (list guest)))
(define filter-guest
  (lambda (p guests)
    (if (empty? guests)
        empty
        (let ((guest (first guests))
              (guests (rest guests)))
          (if (p guest)
              (make-pair guest (filter-guest p guests))
              (filter-guest p guests))))))

(check-expect (filter-male guest-list)
              (filter-guest guest-male? guest-list))

(check-expect (filter-veggies guest-list)
              (filter-guest guest-veggie? guest-list))



(: male-veggie (guest -> boolean))
(define male-veggie
  (lambda (guest)
    (and (guest-male? guest)
         (guest-veggie? guest))))

(define filter-veggie-male
  (lambda(guests)
    (filter-guest male-veggie guests)))

; filtere eine Liste gemäß einem Prädikat
(: filter ((%a -> boolean)(list %a) -> (list %a)))
(define filter
  (lambda (p xs)
    (if (empty? xs)
        empty
        (let ((x (first xs))
              (xs (rest xs)))
          (if (p x)
              (make-pair x (filter p xs))
              (filter p xs))))))

(check-expect (filter-male guest-list)
              (filter guest-male? guest-list))

(check-expect (filter-veggies guest-list)
              (filter guest-veggie? guest-list))

(check-expect (filter odd? (list 1 2 3 4 5 6 7))
              (list 1 3 5 7))

(check-expect (filter string? (list 1 3 "haha" "Maike" #f guest-list filter))
              (list "haha" "Maike"))

; Elemente einer Liste auffalten
(: list-fold (number (number number -> number) (list number) -> number))
(define list-fold
  (lambda (e f l)
    (cond
      ((empty? l)
       e)
      ((pair? l)
       (f (first l) (list-fold e f (rest l)))))))

; Elemente einer Liste aufsummieren
(: list-sum ((list number) -> number))
(define list-sum
  (lambda (xs)
    (list-fold 0 + xs)))

(check-expect (list-sum (list )) 0)
(check-expect (list-sum (list 1 2 3)) 6)

; Elemente einer Liste aufmultiplizieren
(: list-product ((list number) -> number))
(define list-product
  (lambda (xs)
    (list-fold 1 * xs)))

(check-expect (list-product (list )) 1)
(check-expect (list-product (list 1 2 4)) 8)

; Länge einer Liste
(: list-length ((list %a) -> natural))
(define list-length
  (lambda (xs)
    (list-fold 0 list-length-combine xs)))

(: list-length-combine (%a natural -> natural))
(define list-length-combine
  (lambda (x len-rest)
    (+ 1 len-rest)))

(check-expect (list-length (list 3 4 5)) 3)

; Länge einer Liste
(: list-length-b ((list %a) -> natural))
(define list-length-b
  (lambda (xs)
    (list-fold 0
               (lambda (x len-rest)
                 (+ 1 len-rest))
               xs)))

; zwei Prozeduren komponieren (zusammensetzen, hintereinander ausf ̈uhren)
(: compose ((%b -> %c) (%a -> %b) -> (%a -> %c)))
(define compose
  (lambda (f g)
    (lambda (x) (f (g x)))))

(define ff
  (compose (lambda (x) (+ x 1)) 
           (lambda (x) (* x 2))))

; zweites El einer Liste
(: second ((list %as) ->  %as))
(define second
  (compose first rest))

(check-expect (second (list 2 5 7)) 5)

; Prozedur mit sich selbst komponieren
(: repeat (natural (%a -> %a) -> (%a -> %a)))
(define repeat
  (lambda (n f)
    (if (= n 0)
        (lambda (x) x)
        (compose f (repeat (- n 1) f)))))

; (check-expect (repeat 0 ff) (lambda (x) x)) ; Vergleich von Funktionen nicht möglich
(check-property
 (for-all
     ((x number))
   (= ((repeat 0 ff) x)
      x)))

(check-property
 (for-all
     ((x number))
   (= ((repeat  1 ff) x)
      (ff x))))

(check-property
 (for-all
     ((x number))
   (= ((repeat 2 ff) x)
      (ff (ff x)))))

; Spezialversion von repeat:
(: twice ((%a -> %a) -> (%a -> %a)))
(define twice
  (lambda (f) (compose f f)))

; (((twice twice) twice) twice)
; Was ist das Ergebnis von 
; (repeat n twice) (lambda (n) (* 2 n))
