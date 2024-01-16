;; Die ersten drei Zeilen dieser Datei wurden von DrScheme eingefügt. Sie enthalten Metadaten
;; über die Sprachebene dieser Datei in einer Form, die DrScheme verarbeiten kann.
#reader(lib "DMdA-vanilla-reader.ss" "deinprogramm")((modname 20091215-adt) (read-case-sensitive #f) (teachpacks ((lib "world.ss" "teachpack" "deinprogramm"))) (deinprogramm-settings #(#f write repeating-decimal #t #t none explicit #f ((lib "world.ss" "teachpack" "deinprogramm")))))
; Vertrag für Prädikate
(define pred
  (lambda (x)
    (contract (x x -> boolean))))

; Repräsentation für Mengen durch Listen mit Wdhl
(define-record-procedures list-set
  make-list-set list-set?
  (list-set-eq? list-set-rep))
; wobei (: eq? (X X -> boolean)) und (: rep (list X)) sind.
;Dabei ist eq? die Gleichheitsrelation auf X.

;Implementierung: Leere Menge
(: make-empty-list-set ((pred %x) (pred %x) -> list-set))
(define make-empty-list-set
  (lambda (= <)
    (make-list-set = empty)))

;Implementierung: Element einf¨ugen
(: list-set-insert (list-set %x -> list-set))
(define list-set-insert
  (lambda (s x)
    (make-list-set (list-set-eq? s)
                   (make-pair x (list-set-rep s)))))

;Element
(: list-set-member (list-set %x -> boolean))
(define list-set-member
  (lambda (s x)
    (let ((= (list-set-eq? s)))
      (letrec ((loop-member
                (lambda (l)
                  (cond
                    ((empty? l)
                     #f)
                    ((pair? l)
                     (or (= x (first l))
                         (loop-member (rest l))))))))
        (loop-member (list-set-rep s))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-record-procedures sorted-list-set
  make-sorted-list-set sorted-list-set?
  (sorted-list-set-eq? sorted-list-set-lt? sorted-list-set-rep))
;(: eq? (X X -> boolean)) die Gleichheitsrelation auf X,
;(: lt? (X X -> boolean)) die Kleiner-als-Relation auf X und
;(: rep (list X)) eine aufsteigend sortierte Liste ohne wiederholte

;Implementierung: Leere Menge
(: make-empty-sorted-list-set ((pred %x) (pred %x) -> sorted-list-set))
(define make-empty-sorted-list-set
  (lambda (= <)
    (make-sorted-list-set = < empty)))

;Implementierung: Element einf¨ugen
(: sorted-list-set-insert (sorted-list-set %x -> sorted-list-set))
(define sorted-list-set-insert
  (lambda (s x)
    (let ((= (sorted-list-set-eq? s))
          (< (sorted-list-set-lt? s)))
      (letrec ((loop-insert
                (lambda (l)
                  (cond
                    ((empty? l)
                     (list x))
                    ((pair? l)
                     (let ((y (first l)))
                       (cond
                         ((= x y)
                          (rest l))
                         ((< x y)
                          (make-pair x l))
                         ((> x y)
                          (make-pair y (loop-insert (rest l)))))))))))
        (make-sorted-list-set = < (loop-insert (sorted-list-set-rep s)))))))

; Implementierung: Elementtest
(: sorted-list-set-member (sorted-list-set %x -> boolean))
(define sorted-list-set-member
  (lambda (s x)
    (let ((= (sorted-list-set-eq? s))
          (< (sorted-list-set-lt? s)))
      (letrec ((loop-member
                (lambda (l)
                  (cond
                    ((empty? l)
                     #f)
                    ((pair? l)
                     (or (= x (first l))
                         (and (< (first l) x)
                              (loop-member (rest l)))))))))
        (loop-member (sorted-list-set-rep s))))))

(: make-generic-list-set ((pred %a) (pred %a) -> (string -> %op)))
(define make-generic-list-set
  (lambda (= <)
    (letrec ((wrap (lambda (rep)
                     (lambda (m)
                       (cond
                         ((string=? m "empty?")
                          (lambda ()
                            (empty? (list-set-rep rep))))
                         ((string=? m "member?")
                          (lambda (x)
                            (list-set-member rep x)))
                         ((string=? m "insert")
                          (lambda (x)
                            (wrap (list-set-insert rep x)))))))))
      (wrap (make-list-set = < empty)))))

(: make-generic-sorted-list-set ((pred %a) (pred %a) -> (string -> %op)))
(define make-generic-sorted-list-set
  (lambda (= <)
    (letrec ((wrap (lambda (rep)
                     (lambda (m)
                       (cond
                         ((string=? m "empty?")
                          (lambda ()
                            (empty? (sorted-list-set-rep rep))))
                         ((string=? m "member?")
                          (lambda (x)
                            (sorted-list-set-member rep x)))
                         ((string=? m "insert")
                          (lambda (x)
                            (wrap (sorted-list-set-insert rep x)))))))))
      (wrap (make-sorted-list-set = < empty)))))

; generic methods
(define set-empty?
  (lambda (s)
    ((s "empty?"))))
(define set-member?
  (lambda (s x)
    ((s "member?") x)))
(define set-insert
  (lambda (s x)
    ((s "insert") x)))