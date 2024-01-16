;; Die ersten drei Zeilen dieser Datei wurden von DrScheme eingefügt. Sie enthalten Metadaten
;; über die Sprachebene dieser Datei in einer Form, die DrScheme verarbeiten kann.
#reader(lib "DMdA-advanced-reader.ss" "deinprogramm")((modname 20100128-generative) (read-case-sensitive #f) (teachpacks ((lib "world.ss" "teachpack" "deinprogramm"))) (deinprogramm-settings #(#f write repeating-decimal #f #t none datum #f ((lib "world.ss" "teachpack" "deinprogramm")))))
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


