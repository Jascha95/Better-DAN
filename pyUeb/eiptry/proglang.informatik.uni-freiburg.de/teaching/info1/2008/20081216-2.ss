;; Die ersten drei Zeilen dieser Datei wurden von DrScheme eingefügt. Sie enthalten Metadaten
;; über die Sprachebene dieser Datei in einer Form, die DrScheme verarbeiten kann.
#reader(lib "DMdA-vanilla-reader.ss" "deinprogramm")((modname 20081216-2) (read-case-sensitive #f) (teachpacks ((lib "world.ss" "teachpack" "deinprogramm"))) (deinprogramm-settings #(#f write repeating-decimal #t #t none explicit #f ((lib "world.ss" "teachpack" "deinprogramm")))))
; zwei Prozeduren komponieren (zusammensetzen, hintereinander ausf¨uhren)
(: compose ((%b -> %c) (%a -> %b) -> (%a -> %c)))
(define compose
  (lambda (f g)
    (lambda (x) (f (g x)))))

(: add (number -> (number -> number)))
(define add
  (lambda (x)
    (lambda (y)
      (+ x y))))

(define curry
  (lambda (op)
    (lambda (x)
      (lambda (y)
        (op x y)))))

(define add-1
  (lambda (x)
    (+ x 1)))

(define mult-2
  (lambda (x)
    (* x 2)))




; Prozedur mit sich selbst komponieren
(: repeat (natural (%a -> %a) -> (%a -> %a)))
(define repeat
  (lambda (n f)
    (if (= n 0)
        (lambda (x) x)
        (compose f (repeat (- n 1) f)))))

(check-expect ((repeat 0 mult-2) 1) 1)
(check-expect ((repeat 0 mult-2) 2) 2)

; Prozedur entstaffeln (uncurry)
(: uncurry ((%a -> (%b -> %c)) -> (%a %b -> %c)))
(define uncurry
  (lambda (f)
    (lambda (a b)
      ((f a) b))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-contract set-of-integer (integer -> boolean))
; leere Menge
(: make-empty-set ( -> set-of-integer))
(define make-empty-set
  (lambda ()
    (lambda (i)
      #f)))
; Elementtest
(: set-member? (set-of-integer integer -> boolean))
(define set-member?
  (lambda (set i)
    (set i)))

; Einfügen eines Elements
(: set-insert (set-of-integer integer ->  set-of-integer))
(define set-insert
  (lambda (set i)
    (lambda (j)
      (or (= i j) (set j)))))

; Löschen eines Elements
(: set-remove (set-of-integer integer -> set-of-integer))
(define set-remove
  (lambda (set i)
    (lambda (j)
      (if (= i j)
          #f
          (set j)))))

; Menge aller Zahlen größer als n
(: set-bigger-than (integer -> set-of-integer))
(define set-bigger-than
  (lambda (n)
    (lambda (i)
      (> i n))))

(define set-smaller-than
  (lambda (n)
    (lambda (i)
      (< i n))))

; Vereinigung von bMengen
(: set-union (set-of-integer set-of-integer -> set-of-integer))
(define set-union
  (lambda (s1 s2)
    (lambda (i)
      (or (s1 i) (s2 i)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;
(: stream-empty? (stream -> boolean))
(: stream-head (stream -> %X))
(: stream-tail (stream -> stream))
; Implementierung der elemente eines Stream
(define-record-procedures stream-cons
  make-stream-cons stream-cons?
  (stream-cons-real-head stream-cons-real-tail))
; wobei (: stream-cons-real-tail (stream-cons -> ( -> stream)))
(define-contract stream
  (mixed %empty stream-cons))

(define stream-empty?
  empty?)

(define stream-head 
  stream-cons-real-head)

(define stream-tail
  (lambda (st)
    ((stream-cons-real-tail st))))

; erzeuge Stream mit allen Zahlen größer gleich einem Startwert
(: stream-from (integer -> stream))
(define stream-from
  (lambda (n)
    (make-stream-cons
     n
     (lambda ()
       (stream-from (+ n 1))))))

; filtere einen Stream nach einem Prädikat
(: stream-filter ((%a -> boolean) stream -> stream))
(define stream-filter
  (lambda (p s)
    (cond
      ((stream-empty? s)
       empty)
      ((stream-cons? s)
       (let ((x (stream-head s)))
         (if (p x)
             (make-stream-cons x (lambda ()
                              (stream-filter p (stream-tail s))))
             (stream-filter p (stream-tail s))))))))

(check-expect (stream-filter odd? empty) empty)
(check-expect (stream-filter odd? (make-stream-cons 0 (lambda () empty))) empty)

;Sieb des Eratosthenes
(: sieve (stream -> stream))
(define sieve
  (lambda (s)
    (let* ((next-prime (stream-head s))
           (p (lambda (n)
                (not (= 0 (remainder n next-prime))))))
      (make-stream-cons
       next-prime
       (lambda ()
         (sieve
          (stream-filter p (stream-tail s))))))))

; Stream aller Primzahlen
(: primes stream)
(define primes
  (sieve (stream-from 2)))



(: stream-display (stream -> boolean))
(define stream-display
  (lambda (s)
    (cond
      ((stream-empty? s)
       #f)
      ((stream-cons? s)
       (let* ((n (stream-head s))
              (xx (write-string (number->string n)))
              (xx (write-string ", "))
              (ns (stream-tail s)))
         (stream-display ns))))))






