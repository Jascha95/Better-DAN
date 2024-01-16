;; Die ersten drei Zeilen dieser Datei wurden von DrScheme eingefügt. Sie enthalten Metadaten
;; über die Sprachebene dieser Datei in einer Form, die DrScheme verarbeiten kann.
#reader(lib "DMdA-vanilla-reader.ss" "deinprogramm")((modname 20081209-2) (read-case-sensitive #f) (teachpacks ((lib "world.ss" "teachpack" "deinprogramm"))) (deinprogramm-settings #(#f write repeating-decimal #t #t none explicit #f ((lib "world.ss" "teachpack" "deinprogramm")))))
; Elemente einer Liste auffalten
(: list-fold (%b (%a %b -> %b) (list %a) -> %b))
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
  (lambda (l)
    (list-fold 0 + l)))

; Produkt aller Listenelemente
(: list-product ((list number )-> number))
(define list-product
  (lambda (l)
    (list-fold 1 * l)))

(: combine (%a number -> number))
(define combine
  (lambda (first-element length-rest)
    (+ 1 length-rest)))

; Länge einer Liste
(: list-length ((list %a) -> number))
(define list-length
  (lambda (l)
    (list-fold 0 
               (lambda (first-element length-rest)
                 (+ 1 length-rest))
               l)))

; Verkettung von Listen
(: list-cat ((list %a) (list %a) -> (list %a)))
(define list-cat
  (lambda (xs ys)
    (list-fold ys make-pair xs)))



