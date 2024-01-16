(+ 1 (+ 1 (+ 1 0)))

(- (+ 2 3) (* 4 (cos 0)))

((lambda (a) a) (+ ((lambda (a) (+ a 2)) 3) 2))

(define pi 3.14)
(* 2 pi)

(define pi-quadrat (* pi pi))
(/ pi-quadrat 2)

(define quadrat
  (lambda (n)
    (* n n)))
(quadrat (+ 4 2))

((lambda (x) x) (lambda (x) x))