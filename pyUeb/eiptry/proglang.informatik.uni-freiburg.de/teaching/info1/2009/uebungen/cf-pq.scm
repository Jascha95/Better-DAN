;; Die ersten drei Zeilen dieser Datei wurden von DrScheme eingefügt. Sie enthalten Metadaten
;; über die Sprachebene dieser Datei in einer Form, die DrScheme verarbeiten kann.
#reader(lib "DMdA-vanilla-reader.ss" "deinprogramm")((modname complicated-forest-pq) (read-case-sensitive #f) (teachpacks ()) (deinprogramm-settings #(#f write repeating-decimal #t #t none explicit #f ())))
; ein Complicated Forest Priority Queue ist ein Wert
; (really-make-cf-pq my>= r)
; wobei my>= ein reflexiver Vergleichsoperator und 
; r entweder ein cf-heap-node oder #f (leer) ist.
(define-record-procedures cf-pq
  really-make-cf-pq cf-pq?
  (cf-pq-ge? cf-pq-root))

; ein Heapknoten ist ein Wert
; (make-cf-heap-node v children)
; der den Wert v enthaelt und die Kinderliste children hat.
(define-record-procedures cf-heap-node
  make-cf-heap-node cf-heap-node?
  (cf-heap-node-value cf-heap-node-children))

; erzeuge eine leere Complicated Forest PQ mit der gegebenen Vergleichsprozedur
(: make-empty-cf-pq ((%a %a -> boolean) -> cf-pq))
(define make-empty-cf-pq
  (lambda (my>=)
    (really-make-cf-pq my>= #f)))

; fuegt die kids den Kindern von root hinzu und liefert ein neues root
(: put-below (cf-heap-node (list cf-heap-node) -> cf-heap-node))
(define put-below 
  (lambda (root kids)
    (make-cf-heap-node (cf-heap-node-value root)
                       (append (cf-heap-node-children root) kids))))

; sucht das Maximum unter den heapnodes in hs und versammelt die nicht-Maxima
; unter dem Maximum. Ergibt neuen heap-node, falls hs nichtleer war, sonst #f.
(: shallow-merge ((%a %a -> boolean) (list cf-heap-node) -> (mixed false cf-heap-node)))
(define shallow-merge
  (lambda (my>= hs)
    (letrec
        ([merge (lambda (max-so-far nonmaxes hs)
                  (cond
                    [(empty? hs)
                     (put-below max-so-far nonmaxes)]
                    [(my>= (cf-heap-node-value (first hs)) (cf-heap-node-value max-so-far))
                     (let* ([new-max (first hs)]
                            [new-nonmaxes (make-pair max-so-far nonmaxes)])
                       (merge new-max new-nonmaxes (rest hs)))]
                    [else
                     (let* ([new-nonmaxes (make-pair (first hs) nonmaxes)])
                       (merge max-so-far new-nonmaxes (rest hs)))]))])
      (if (empty? hs)
          #f
          (merge (first hs) empty (rest hs))))))
                     
; Fuegt einen Wert in eine cf-pq ein
(: cf-pq-insert (cf-pq %a -> cf-pq))
(define cf-pq-insert
  (lambda (pq x)
    (let* ([my>= (cf-pq-ge? pq)]
           [root (cf-pq-root pq)])
      (if (false? root)
          (really-make-cf-pq my>= (make-cf-heap-node x empty))
          (let* ([forest+x (list (make-cf-heap-node x empty) root)])
            (really-make-cf-pq my>= (shallow-merge my>= forest+x)))))))

; Findet das Maximum in einer cf-pq. Falls leer, Violation "empty pq".
(: cf-pq-getmax (cf-pq -> %a))
(define cf-pq-getmax
  (lambda (pq)
    (let* ([my>= (cf-pq-ge? pq)]
           [root (cf-pq-root pq)])
      (if (false? root)
          (violation "empty pq")
          (cf-heap-node-value root)))))

; Entfernt das Maximum einer cf-pq. Falls leer, Violation "empty pq".
(: cf-pq-removemax (cf-pq -> cf-pq))
(define cf-pq-removemax
  (lambda (pq)
    (let* ([my>= (cf-pq-ge? pq)]
           [root (cf-pq-root pq)])
      (if (false? root)
          (violation "empty pq")
          (really-make-cf-pq my>= (shallow-merge my>= (cf-heap-node-children root)))))))

; Ist die cf-pq leer?
(: cf-pq-empty? (cf-pq -> boolean))
(define cf-pq-empty? 
  (lambda (pq)
    (false? (cf-pq-root pq))))
    
