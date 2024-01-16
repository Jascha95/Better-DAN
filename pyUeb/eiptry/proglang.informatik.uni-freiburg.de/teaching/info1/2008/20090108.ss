;; Die ersten drei Zeilen dieser Datei wurden von DrScheme eingefügt. Sie enthalten Metadaten
;; über die Sprachebene dieser Datei in einer Form, die DrScheme verarbeiten kann.
#reader(lib "DMdA-vanilla-reader.ss" "deinprogramm")((modname |20090108|) (read-case-sensitive #f) (teachpacks ((lib "world.ss" "teachpack" "deinprogramm"))) (deinprogramm-settings #(#f write repeating-decimal #t #t none explicit #f ((lib "world.ss" "teachpack" "deinprogramm")))))
(define-record-procedures list-set
  make-list-set list-set?
  (list-set-eq? list-set-rep))
; wobei (: eq? (X X -> boolean)) und (: rep (list X)) sind.
;Dabei ist eq? die Gleichheitsrelation auf X.
;Implementierung: Leere Menge
(define make-empty-list-set
  (lambda (= <)
    (make-list-set = empty)))
;Implementierung: Element einf¨ugen
(define list-set-insert
  (lambda (x s)
    (make-list-set (list-set-eq? s)
                   (make-pair x (list-set-rep s)))))
;Element
(define list-set-member
  (lambda (x s)
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

(define-record-procedures sort-list-set
  make-sorted-list-set sorted-list-set?
  (sorted-list-set-eq? sorted-list-set-lt? sorted-list-set-rep))
;(: eq? (X X -> boolean)) die Gleichheitsrelation auf X,
;(: lt? (X X -> boolean)) die Kleiner-als-Relation auf X und
;(: rep (list X)) eine aufsteigend sortierte Liste ohne wiederholte
;Implementierung: Leere Menge
(define make-empty-sorted-list-set
  (lambda (= <)
    (make-sorted-list-set = < empty)))
;Implementierung: Element einf¨ugen
(define sorted-list-set-insert
  (lambda (x s)
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

(define sorted-list-set-member
  (lambda (x s)
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


(define-record-procedures ops
  make-ops ops?
  (ops-ins ops-mem))
(define-record-procedures set
  really-make-set set?
  (set-ops set-rep))

(define wrap-rep
  (lambda (this rep1)
    (really-make-set (set-ops this) rep1)))

(define set-insert
  (lambda (x s)
    (wrap-rep s
              ((ops-ins (set-ops s)) x (set-rep s)))))
; Generischer Aufruf des Elementtests
;(: set-member (X (set X) -> boolean))
(define set-member
  (lambda (x s)
    ((ops-mem (set-ops s)) x (set-rep s))))

(define make-generic-list-set
  (lambda (= <)
    (really-make-set
     (make-ops list-set-insert
               list-set-member)
     (make-empty-list-set = <))))

(define make-generic-sorted-list-set
  (lambda (= <)
    (really-make-set
     (make-ops sorted-list-set-insert
               sorted-list-set-member)
     (make-empty-sorted-list-set = <))))
