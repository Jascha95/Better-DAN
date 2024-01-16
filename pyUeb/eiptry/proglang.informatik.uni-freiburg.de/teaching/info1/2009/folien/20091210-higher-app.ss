;; Die ersten drei Zeilen dieser Datei wurden von DrScheme eingefügt. Sie enthalten Metadaten
;; über die Sprachebene dieser Datei in einer Form, die DrScheme verarbeiten kann.
#reader(lib "DMdA-vanilla-reader.ss" "deinprogramm")((modname 20091210-higher-app) (read-case-sensitive #f) (teachpacks ((lib "image.ss" "teachpack" "deinprogramm"))) (deinprogramm-settings #(#f write repeating-decimal #t #t none explicit #f ((lib "image.ss" "teachpack" "deinprogramm")))))
(define set-of-integer
  (contract (integer -> boolean)))

(: make-empty-set ( -> set-of-integer))
(define make-empty-set
  (lambda ()
    (lambda (x)
      #f)))

(: set-member? (set-of-integer integer -> boolean))
(define set-member?
  (lambda (s x)
    (s x)))

(: set-insert (set-of-integer integer -> set-of-integer))
(define set-insert
  (lambda (s x)
    (lambda (y)
      (or (= y x) (s y)))))

(: set-remove (set-of-integer integer -> set-of-integer))
(define set-remove
  (lambda (s x)
    (lambda (y)
      (and (not (= y x)) (s y)))))

(check-property
 (for-all ((s set-of-integer)
           (x integer))
   (set-member? (set-insert s x) x)))
(check-property
 (for-all ((s set-of-integer)
           (x integer))
   (not (set-member? (set-remove s x) x))))
(check-property
 (for-all ((x integer))
   (not (set-member? (make-empty-set) x))))

; Menge der ungeraden Zahlen
(define set-of-odds
  odd?)
; Menge der durch drei teilbaren Zahlen
(define set-of-divisible-3
  (lambda (y)
    (= (remainder y 3) 0)))








; berechne die Ableitung einer Funktion
(: derivative (real -> ((real -> real) -> (real -> real))))
(define derivative
  (lambda (h)
    (lambda (f)
      (lambda (x)
        (/ (- (f (+ x h)) (f x))
           h)))))

; Tests
(define x->2x ((derivative .00001) (lambda (x) (* x x))))
(check-property
 (for-all ((x real))
   (expect-within (x->2x x) (* 2 x) .01)))
(define x->3xx ((derivative .00001) (lambda (x) (* x x x))))
(check-property
 (for-all ((x real))
   (expect-within (x->3xx x) (* 3 x x) .01)))
;(define exp-1 ((derivative .00000000000000001) exp))
;(check-property
; (for-all ((x real))
;   (expect-within (exp-1 x) (exp x) .1)))




; berechne das Integral einer Funktion zwischen zwei Grenzen
(: integral ((real -> real) real real natural -> real))
(define integral
  (lambda (f a b n)
    (let ((dx (/ (- b a) n)))
      (letrec ((loop
                (lambda (i x sum)
                  (if (zero? i)
                      sum
                      (loop (- i 1) (+ x dx) (+ (f x) sum))))))
        (* dx (loop n a 0))))))

; Tests
(check-within
 (integral (lambda (x) (+ x 1)) 0 1 1000)
 1.5 .005)
(check-within
 (integral (lambda (x) (* x x)) 0 1 1000)
 (/ 1 3) .005)



; Implementierung der Elemente eines Stream
(define-record-procedures stream-cons
  make-stream-cons stream-cons?
  (stream-cons-real-head stream-cons-real-tail))
; Der Rest eines Stroms ist speziell:
(: make-stream-cons (%X ( -> stream) -> stream-cons))
(define stream
  (contract (mixed (one-of empty) stream-cons)))

; Test auf leeren Stream
(: stream-empty? (stream -> boolean))
(define stream-empty?
  empty?)

; Erstes Element eines nicht-leeren Streams
(: stream-head (stream-cons -> %X))
(define stream-head
  (lambda (s)
    (stream-cons-real-head s)))

; Rest eines nicht-leeren Streams
(: stream-tail (stream-cons -> stream))
(define stream-tail
  (lambda (s)
    ((stream-cons-real-tail s))))

; Stream der natürlichen Zahlen
(: naturals-from (natural -> stream))
(define naturals-from
  (lambda (n)
    (make-stream-cons n
                      (lambda ()
                        (naturals-from (+ n 1))))))

(: all-naturals stream)
(define all-naturals (naturals-from 0))

; Filtern eines Stream nach einem Prädikat
(: stream-filter ((integer -> boolean) stream -> stream))
(define stream-filter
  (lambda (p s)
    (if
     (stream-empty? s)
     empty
     (let ((x (stream-head s))
           (s (stream-tail s)))
       (if (p x)
           (make-stream-cons x (lambda () (stream-filter p s)))
           (stream-filter p s))))))

(: all-divisible-3 stream)
(define all-divisible-3 (stream-filter set-of-divisible-3 all-naturals))

; Sieb des Eratosthenes
(: sieve (stream -> stream))
(define sieve
  (lambda (s)
    (let ((p (stream-head s)))
      (make-stream-cons p
                        (lambda ()
                          (let* ((is-not-multiple-of-p? 
                                  (lambda (x) (not (zero? (remainder x p)))))
                                 (next-s (stream-filter is-not-multiple-of-p?
                                                        (stream-tail s))))
                            (sieve next-s)))))))

(: primes stream)
(define primes (sieve (naturals-from 2)))





(: stream-display (stream -> boolean))
(define stream-display
  (lambda (s)
    (cond
      ((stream-empty? s)
       #f)
      ((stream-cons? s)
       (let* ((n (stream-head s))
              (xx (write-string (number->string n)))
              (xx (write-string ", ")))
         (stream-display (stream-tail s)))))))



