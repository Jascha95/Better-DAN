;; Die ersten drei Zeilen dieser Datei wurden von DrScheme eingefügt. Sie enthalten Metadaten
;; über die Sprachebene dieser Datei in einer Form, die DrScheme verarbeiten kann.
#reader(lib "DMdA-vanilla-reader.ss" "deinprogramm")((modname 20071206-info1) (read-case-sensitive #f) (teachpacks ((lib "world.ss" "teachpack" "deinprogramm"))) (deinprogramm-settings #9(#f write repeating-decimal #t #t none explicit #f ((lib "world.ss" "teachpack" "deinprogramm")))))
; Ein leerer Baum ist ein Wert
; (make-empty-tree)
(define-record-procedures empty-tree
make-empty-tree empty-tree?
())
; Der leere Baum
; the-empty-tree : empty-tree
(define the-empty-tree (make-empty-tree))

; Ein Knoten ist ein Wert
; (make-node l b r)
; wobei b : value eine Markierung ist und l und r Baume.
(define-record-procedures node
make-node node?
(node-left node-label node-right))
; Ein Binaerbaum ist dann ein gemischter, rekursiver Datentyp:
; Ein Binaerbaum ist eins der folgenden
; - ein leerer Baum
; - ein Knoten
; Name: btree
(define make-leaf
(lambda (x) (make-node the-empty-tree x the-empty-tree)))
(define t0 the-empty-tree)
(define t1 (make-leaf 5))
(define t2 (make-node t1 6 (make-leaf 10)))
(define t3 (make-node t2 12 (make-leaf 20)))

; tiefe eines Binärbaums berechnen
; btree-depth : btree -> nat
(define btree-depth
  (lambda (bt)
    (cond
      ((empty-tree? bt)
       0)
      ((node? bt)
       (+ 1 (max (btree-depth (node-left bt))
                 (btree-depth (node-right bt))))))))

; Größe eines Binärbaums bestimmen
; btree-size : btree -> nat
(define btree-size 
  (lambda (bt)
    (cond
      ((empty-tree? bt)
       0)
      ((node? bt)
       (+ 1 (btree-size (node-left bt))
          (btree-size (node-right bt)))))))

; Ein Suchbaum ist ein Wert
; (make-search-tree eq le t)
; wobei
; eq : A A -> boolean
; eine Prozedur ist, die zwei Elemente auf Gleichheit testet
; le : A A -> boolean
; eine Prozedur ist, die zwei Elemente auf kleiner oder gleich testet
; t : btree(A)
; ein Binaerbaum ist, der die Suchbaumeigenschaft besitzt.
(define-record-procedures search-tree
  make-search-tree search-tree?
  (search-tree-label-equal-proc
   search-tree-label-leq-proc
   search-tree-tree))

; leeren Suchbaum konstruieren
; make-empty-search-tree : (A A -> boolean) (A A -> boolean)
; -> search-tree(A)
(define make-empty-search-tree
(lambda (eq le)
(make-search-tree eq le the-empty-tree)))


(define s0 (make-empty-search-tree = <=))
(define s1 (make-empty-search-tree string=? string<=?))

; testen ob ein Element im Suchbaum vorkommt
; search-tree-member : search-tree(A) A -> boolean
(define search-tree-member
  (lambda (st el)
    (let ((eq? (search-tree-label-equal-proc st))
          (le? (search-tree-label-leq-proc st))
          (bt  (search-tree-tree st)))
      (letrec ((member
                (lambda (bt)
                  (cond
                    ((empty-tree? bt)
                     #f)
                    ((node? bt)
                     (let ((node-elem (node-label bt)))
                       (cond
                         ((eq? node-elem el)
                          #t)
                         ((le? el node-elem)
                          (member (node-left bt)))
                         ((le? node-elem el)
                          (member (node-right bt))))))))))
        (member bt)))))

; ein Element in Suchbaum einfügen
; search-tree-insert : search-tree(A) A -> search-tree(A)
(define search-tree-insert
  (lambda (st el)
    (let ((eq? (search-tree-label-equal-proc st))
          (le? (search-tree-label-leq-proc st))
          (bt  (search-tree-tree st)))
      ; insert : btree(A) -> btree(A)
      (letrec ((insert
                (lambda (bt)
                  (cond
                    ((empty-tree? bt)
                     (make-leaf el))
                    ((node? bt)
                     (let ((node-elem (node-label bt)))
                       (cond
                         ((eq? node-elem el)
                          bt)
                         ((le? el node-elem)
                          (make-node (insert (node-left bt)) node-elem (node-right bt)))
                         ((le? node-elem el)
                          (make-node (node-left bt) node-elem (insert (node-right bt)))))))))))
        (make-search-tree eq? le? (insert bt))))))

; search-tree-insert-many : search-tree(A) list(A) -> search-tree(A)
(define search-tree-insert-many
  (lambda (st ls)
    (cond
      ((empty? ls)
       st)
      ((pair? ls)
       (search-tree-insert (search-tree-insert-many st (rest ls)) (first ls))))))
