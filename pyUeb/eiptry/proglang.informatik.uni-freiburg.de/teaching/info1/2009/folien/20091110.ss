;; Die ersten drei Zeilen dieser Datei wurden von DrScheme eingefügt. Sie enthalten Metadaten
;; über die Sprachebene dieser Datei in einer Form, die DrScheme verarbeiten kann.
#reader(lib "DMdA-vanilla-reader.ss" "deinprogramm")((modname |20091110|) (read-case-sensitive #f) (teachpacks ((lib "image.ss" "teachpack" "deinprogramm"))) (deinprogramm-settings #(#f write repeating-decimal #t #t none explicit #f ((lib "image.ss" "teachpack" "deinprogramm")))))
; Elemente einer Liste addieren
(: list-sum ((list number) -> number))
(define list-sum
  (lambda (xs)
    (cond
      ((empty? xs) 0)
      ((pair? xs)
       (+ (first xs) (list-sum (rest xs)))))))

(check-expect (list-sum empty) 0)
(check-expect (list-sum (make-pair 3 (make-pair 5 empty))) 8)
(check-expect (list-sum (list 0 1 2 3 4 5 6 7 8 9)) 45)

; Anzahl der Element in einer Liste
(: list-length ((list %a) -> number))
(define list-length
  (lambda (xs)
    (cond
      ((empty? xs)
       0)
      ((pair? xs)
       (+ 1 (list-length (rest xs)))))))

(check-expect (list-length empty) 0)
(check-expect (list-length (list 1 2 3 4 5 6)) 6)
(check-expect (list-length (list "one" "two" "three")) 3)
(check-expect (list-length (list #f #t)) 2)
(check-expect (list-length (list "ghhghg" 2+3i #f (list 1 2 3))) 4)


; Fakultätsfunktion
(: ! (natural -> natural))
(define !
  (lambda (n)
    (if (zero? n)
        1
        (* n (! (- n 1))))))

(check-expect (! 0) 1)
(check-expect (! 1) 1)
(check-expect (! 5) 120)

; nichts tun
(: waste-time (number -> number))
(define waste-time
  (lambda (x)
    (waste-time x)))

; (waste-time 4711)

; n! nicht berechnen
(: notfac (natural -> natural))
(define notfac
  (lambda (n)
    (if (zero? n)
        1
        (* n (notfac n)))))

; (notfac 40)

; Produkt der Zahlen 1 .. n berechnen
(: it-factorial (natural -> natural))
(define it-factorial
  (lambda (n)
    (it-factorial-1 n 1)))

(: it-factorial-1 (natural natural -> natural))
(define it-factorial-1
  (lambda (n r)
    (if (zero? n)
        r
        (it-factorial-1 (- n 1) (* n r)))))

(check-expect (it-factorial 0) 1)
(check-expect (it-factorial 1) 1)
(check-expect (it-factorial 5) 120)


; (it-factorial-1 18 r) =>
; (it-factorial-1 17 (* 18 r)) =>
; (it-factorial-1 16 (* 17 (* 18 r)))
; ...
; (it-factorial-1 0 (! 18))

; (it-factorial-1 5 1)

; Invertieren einer Liste
(: invert ((list %a) -> (list %a)))
(define invert
  (lambda (l)
    (invert-helper l empty)))
(: invert-helper ((list %a) (list %a) -> (list %a)))
(define invert-helper
  (lambda (l acc)
    (if (empty? l)
        acc
        (invert-helper (rest l) (make-pair (first l) acc)))))

(check-expect (invert empty) empty)
(check-expect (invert (list 1 2 3 4)) (list 4 3 2 1))