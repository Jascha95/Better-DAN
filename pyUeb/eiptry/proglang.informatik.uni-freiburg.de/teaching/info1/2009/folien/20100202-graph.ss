;; Die ersten drei Zeilen dieser Datei wurden von DrScheme eingefügt. Sie enthalten Metadaten
;; über die Sprachebene dieser Datei in einer Form, die DrScheme verarbeiten kann.
#reader(lib "DMdA-advanced-reader.ss" "deinprogramm")((modname 20100202-graph) (read-case-sensitive #f) (teachpacks ((lib "world.ss" "teachpack" "deinprogramm") (lib "vector.ss" "installed-teachpacks"))) (deinprogramm-settings #(#f write repeating-decimal #f #t none datum #f ((lib "world.ss" "teachpack" "deinprogramm") (lib "vector.ss" "installed-teachpacks")))))
; element test in einer liste
(define member
  (lambda (xs x)
    (if (empty? xs)
        #f
        (if (equal? (first xs) x)
            #t
            (member (rest xs) x)))))

; filtern einer Liste nach einem Prädikat
(: filter ((%x -> boolean) (list %x) -> (list %x)))
(define filter
  (lambda (p xs)
    (if (empty? xs)
        empty
        (let ((x (first xs))
              (xs (rest xs)))
          (if (p x)
              (make-pair x (filter p xs))
              (filter p xs))))))

; sortieren einer Liste mit generativeer Rekursion (quicksort)
(: qsort ((%x %x -> boolean) (list %x) -> (list %x)))
(define qsort
  (lambda (<= l)
    (cond
      ((empty? l) l)
      ((empty? (rest l)) l)
      (else
       (let ((p (first l))
             (l (rest l)))
         (let ((smaller (filter (lambda (x) (not (<= p x))) l))
               (larger  (filter (lambda (x) (<= p x)) l)))
           (append (qsort <= smaller)
                   (make-pair p
                              (qsort <= larger)))))))))

(check-expect (qsort <= (list)) (list))
(check-expect (qsort <= (list 11 8 14 7)) (list 7 8 11 14))
(check-expect (qsort string<=? (list "d" "c" "b" "a"))
              (list "a" "b" "c" "d"))


(define posnat
  (contract (combined natural (predicate positive?))))
; größter gemeinsamer Teiler (strukturell)
(: ggt0 (posnat posnat -> posnat))
(define ggt0
  (lambda (m n)
    (letrec ((greatest-divisor-<= 
              (lambda (i)
                (if (= i 1)
                    1
                    (if (and (= 0 (remainder m i))
                             (= 0 (remainder n i)))
                        i
                        (greatest-divisor-<= (- i 1)))))))
      (greatest-divisor-<= (min m n)))))
(check-expect (ggt0 1 1) 1)
(check-expect (ggt0 81 18) 9)
(check-expect (ggt0 1001 21) 7)

; größter gemeinsamer Teiler (generativ, Euklid)
(: ggt (posnat posnat -> posnat))
(define ggt
  (lambda (m n)
    (letrec ((ggt-h
              (lambda (a b) ; a >= b > 0
                (let ((r (remainder a b)))
                  (if (zero? r)
                      b
                      (ggt-h b r))))))
      (if (>= m n)
          (ggt-h m n)
          (ggt-h n m)))))

(check-expect (ggt 1 1) 1)
(check-expect (ggt 81 18) 9)
(check-expect (ggt 1001 21) 7)

; Rechengenauigkeit
(: epsilon real)
(define epsilon 1e-10)

; Mittelwertbedingung
(: mean-value-condition (real real -> boolean))
(define mean-value-condition
  (lambda (x y)
    (or (<= x 0 y)
        (<= y 0 x))))

(: find-root ((real -> real) real real -> real))
;Erklarung: (find-root f a b) liefert eine Zahl x zwischen a und b, so dass f eine
; Nullstelle zwischen x und x + epsilon besitzt.
; Voraussetzung: (mean-value-condition (f a) (f b))
(define find-root
  (lambda (f a b)
    (if (< (abs (- a b)) epsilon)
        a
        (let* ((m (/ (+ a b) 2))
               (fa (f a))
               (fb (f b))
               (fm (f m)))
          (if (mean-value-condition fa fm)
              (find-root f a m)
              (find-root f m b))))))

(define poly
  (lambda (x) (* (- x 1) (- x 2) (- x 3))))
(check-within (find-root poly 0 1.5) 1 epsilon)
(check-within (find-root poly 1.5 2.5) 2 epsilon)
(check-within (find-root poly 2.5 1000) 3 epsilon)
(check-within (find-root sin -1 1) 0 epsilon)


(define node
  (contract symbol))
; erster Knoten jeder Liste ist Vorganger aller restlichen Knoten
(define graph
  (contract (list (list node))))

; Kleidungsstucke
(: clothes (list node))
(define clothes '(unterhose socken schuhe hose guertel uhr
                            unterhemd oberhemd krawatte sacko mantel))
; vorher anziehen
(: put-on-before graph)
(define put-on-before
  (list '(unterhose)
        '(socken)
        '(schuhe socken hose)
        '(hose unterhose unterhemd)
        '(guertel hose)
        '(unterhemd)
        '(oberhemd unterhemd)
        '(krawatte oberhemd)
        '(sacko krawatte)
        '(mantel sacko hose)
        '(uhr)))

; kanten: (schuhe socken) (schuhe hose) (hose unterhose) (hose unterhemd)
; (huertel hose) (oberhemd unterhemd)  (krawatte oberhemd) (sacko krawatte)
; (mantel sacko) (mantel hose)

; Nachbarschaftsliste zu einem Knoten
(: succ (graph node -> (list node)))
(define succ
  (lambda (g n)
    (if (empty? g)
        (violation "Knoten nicht gefunden")
        (let ((nodelist (first g)))
          (if (equal? (first nodelist) n)
              (rest nodelist)
              (succ (rest g) n))))))
(check-expect (succ put-on-before 'mantel) '(sacko hose))
(check-expect (succ put-on-before 'uhr) '())

; finde einen Pfad zwischen einem Knoten aus einer Liste zu 
; einem Zielknoten oder #f, falls von keinem der Knoten ein Pfad
; zum Ziel existiert
; Die Knoten in visited liegen bereits auf dem Pfad.
(: find-path* (graph (list node)(list node) node -> (mixed (one-of #f) (list node))))
(define find-path*
  (lambda (g visited nodes dest)
    (cond
      ((empty? nodes) #f)
      (else 
       (let ((orig (first nodes)))
         (if (member visited orig )
             (find-path* g visited (rest nodes) dest)
             (let ((result (find-path-helper g visited orig dest)))
               (if (pair? result)
                   result
                   (find-path* g visited (rest nodes) dest)))))))))

; finde einen Pfag zwischen zwei Knoten oder #f, falls keiner existiert
(: find-path (graph node node -> (mixed (one-of #f) (list node))))
(define find-path
  (lambda (g orig dest)
    (find-path-helper g (list) orig dest)))

(: find-path-helper (graph (list node) node node -> (mixed (one-of #f) (list node))))
(define find-path-helper
  (lambda (g visited orig dest)
    (if (equal? orig dest)
        (list orig)
        (let* ((neighbors (succ g orig))
               (result (find-path* g (make-pair orig visited) neighbors dest)))
          (if (pair? result)
              (make-pair orig result)
              #f)))))

(define graph-as-list
  '((A B E)
    (B E F)
    (C D)
    (D )
    (E C F)
    (F D G)
    (G )))

(check-expect (find-path graph-as-list 'a 'g) '(a b e f g))
(check-expect (find-path graph-as-list 'b 'a) #f)
(check-expect (find-path graph-as-list 'd 'd) '(d))


(define g2
  '((A B)
    (B A C)
    (C)))

(check-expect (find-path g2 'a 'a) (list 'a))
(check-expect (find-path g2 'a 'b) '(a b))
(check-expect (find-path g2 'a 'c) '(a b c))


(define g3
  '((A B C)
    (B D)
    (C B E)
    (E B F)
    (F B G)
    (G B H)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define vector3d
  (contract
   (combined (predicate vector?)
             (predicate (lambda (v) (= 3 (vector-length v)))))))

; addiere die Elemente eines 3d Vektors
(: vector-sum-3 (vector3d -> number))
(define vector-sum-3
  (lambda (v)
    (+ (vector-ref v 0)
       (vector-ref v 1)
       (vector-ref v 2))))
(check-expect (vector-sum-3 (vector 0 0 0)) 0)
(check-expect (vector-sum-3 (make-vector 3 4)) 12)
(check-expect (vector-sum-3 (vector -1 3/4 1/4)) 0)

; addiere die Elemente eines beliebigen Vektors
(: vector-sum ((predicate vector?) -> number))
(define vector-sum
  (lambda (v)
    (vector-sum-helper v (vector-length v))))

; summiere die Elemente eines Vektors im Bereich 0..n-1 (n <= Länge des Vektors)
(: vector-sum-helper ((predicate vector?) natural -> number))
(define vector-sum-helper
  (lambda (v n)
    (if (zero? n)
        0
        (+ (vector-ref v (- n 1))
           (vector-sum-helper v (- n 1))))))

(check-expect (vector-sum (vector 0 0 0)) 0)
(check-expect (vector-sum (make-vector 3 4)) 12)
(check-expect (vector-sum (vector -1 3/4 1/4)) 0)
(check-expect (vector-sum (make-vector 250 1/4)) 125/2)

; summierung eines Vektors mit generativer Rek
(: vector-sum-1 ((predicate vector? ) -> number))
(define vector-sum-1
  (lambda (v)
    (letrec ((loop
              (lambda (i)
                (if (= i (vector-length v))
                    0
                    (+ (vector-ref v i)
                       (loop (+ i 1)))))))
      (loop 0))))

(check-expect (vector-sum-1 (vector 0 0 0)) 0)
(check-expect (vector-sum-1 (make-vector 3 4)) 12)
(check-expect (vector-sum-1 (vector -1 3/4 1/4)) 0)
(check-expect (vector-sum-1 (make-vector 250 1/4)) 125/2)

; addition von vektoren
(: vector-add ((predicate vector?) (predicate vector?) -> (predicate vector?)))
(define vector-add
  (lambda (v w)
    (let* ((n (min (vector-length v) (vector-length w)))
           (r (make-vector n 0)))
      (letrec ((loop
                (lambda (i)
                  (if (= i n)
                      r
                      (begin
                        (vector-set! r i (+ (vector-ref v i) (vector-ref w i)))
                        (loop (+ i 1)))))))
        (loop 0)))))

(check-expect (vector-add (vector ) (vector )) (vector))
(check-expect (vector-add (make-vector 250 1) (make-vector 300 2)) (make-vector 250 3))
(check-expect (vector-add (vector 1 2 3) (vector 6 5 4)) (vector 7 7 7))









