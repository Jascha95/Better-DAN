;; Die ersten drei Zeilen dieser Datei wurden von DrScheme eingefügt. Sie enthalten Metadaten
;; über die Sprachebene dieser Datei in einer Form, die DrScheme verarbeiten kann.
#reader(lib "DMdA-vanilla-reader.ss" "deinprogramm")((modname 20091201-btree) (read-case-sensitive #f) (teachpacks ((lib "image.ss" "teachpack" "deinprogramm"))) (deinprogramm-settings #(#f write repeating-decimal #t #t none explicit #f ((lib "image.ss" "teachpack" "deinprogramm")))))
; Ein leerer Bin ̈arbaum ist ein Wert
; (make-empty-tree)
(define-record-procedures empty-tree
  make-empty-tree empty-tree?
  ())
; Der leere Bin¨arbaum
; the-empty-tree : empty-tree
(define the-empty-tree (make-empty-tree))

; Ein Knoten ist ein Wert
; (make-node l b r)
; wobei b : value eine Markierung ist und l und r Bin ̈arb ̈aume.
(define-record-procedures node
  make-node node?
  (node-left node-label node-right))
; Ein Bin¨arbaum ist ein gemischter, rekursiver Datentyp:
; Ein Bin¨arbaum ist eins der folgenden
; - ein leerer Baum
; - ein Knoten
(define btree
  (contract (mixed empty-tree node)))

; erzeuge ein Blatt
(: make-leaf (%a -> btree))
(define make-leaf
  (lambda (v)
    (make-node the-empty-tree v the-empty-tree)))

(define t0 the-empty-tree)
(define t1 (make-leaf 5))
(define t2 (make-node t1 6 (make-leaf 10)))
(define t3 (make-node t2 12 (make-leaf 20)))

; Berechnet die Tiefe eines Bin ̈arbaums
(: btree-depth (btree -> natural))
(define btree-depth
  (lambda (t)
    (cond
      ((empty-tree? t)
       0)
      ((node? t)
       (+ 1
          (max (btree-depth (node-left t)) 
               (btree-depth (node-right t))))))))

; Tests
(check-expect (btree-depth the-empty-tree)
              0)
(check-expect (btree-depth (make-node (make-leaf 1) 0 the-empty-tree))
              2)

; bestimmt die Anzahl der Knoten im Bin ̈arbaum
(: btree-size (btree -> natural))
(define btree-size
  (lambda (t)
    (cond
      ((empty-tree? t)
       0)
      ((node? t)
       (+ 1
          (btree-size (node-left t))
          (btree-size (node-right t)))))))

; Tests
(check-expect (btree-size the-empty-tree)
              0)
(check-expect (btree-size (make-node (make-leaf 1) 0 the-empty-tree))
              2)

; Visualiere einen Baum (mit Zahlen)
(: visualize (btree -> image))
(define visualize
  (lambda (t)
    (cond
      ((empty-tree? t)
       (ellipse 10 10 "solid" "black"))
      ((node? t)
       (let ((left-im (visualize (node-left t)))
             (right-im (visualize (node-right t))))
         (above
          (text (number->string (node-label t)) 10 "red")
          (beside left-im right-im "top")
          "center"))))))

; Visualiere einen Baum (mit Zahlen)
(: visualize-1 (btree -> image))
(define visualize-1
  (lambda (t)
    (cond
      ((empty-tree? t)
       (ellipse 10 10 "solid" "black"))
      ((node? t)
       (let ((left-im (visualize-1 (node-left t)))
             (right-im (visualize-1 (node-right t))))
         (above
          (text (number->string (node-label t)) 10 "red")
          (beside (beside left-im (rectangle 2 2 "solid" "white")"top") right-im "top")
          "center"))))))


; Suchen eines Elements in einem bin ̈aren Suchbaum
(: btree-member? (btree real -> boolean))
(define btree-member?
  (lambda (s x)
    (cond
      ((empty-tree? s)
       #f)
      ((node? s)
       (let ((y (node-label s)))
         (cond
           ((= x y)
            #t)
           ((< x y)
            (btree-member? (node-left s) x))
           ((> x y)
            (btree-member? (node-right s) x))))))))

; Tests
(check-expect (btree-member? the-empty-tree 42)
              #f)
(check-expect (btree-member? (make-leaf 99) 42)
              #f)
(check-expect (btree-member? (make-leaf 99) 99)
              #t)
(check-expect (btree-member? t3 6) #t)
(check-expect (btree-member? t3 20) #t)
(check-expect (btree-member? t3 11) #f)

; Einf ̈ugen eines Elements in einen bin ̈aren Suchbaum
(: btree-insert (btree real -> btree))
(define btree-insert
  (lambda (s x)
    (cond
      ((empty-tree? s)
       (make-leaf x))
      ((node? s)
       (let ((y (node-label s)))
         (cond
           ((= x y)
            s)
           ((< x y)
            (make-node (btree-insert (node-left s) x)
                       y
                       (node-right s)))
           ((> x y)
            (make-node (node-left s)
                       y
                       (btree-insert (node-right s) x)))))))))

; Tests
(check-expect (btree-insert the-empty-tree 42)
              (make-leaf 42))
(check-expect (btree-insert (make-leaf 99) 42)
              (make-node (make-leaf 42) 99 the-empty-tree))
(check-expect (btree-insert (make-leaf 99) 99)
              (make-leaf 99))

; L ̈oschen eines Elements in einem bin ̈aren Suchbaum
(: btree-delete (btree real -> btree))
(define btree-delete
  (lambda (s x)
    (cond
      ((empty-tree? s)
       s)
      ((node? s)
       (let ((y (node-label s)))
         (cond
           ((= x y)
            (btree-combine (node-left s) (node-right s)))
           ((< x y)
            (make-node (btree-delete (node-left s) x)
                       y
                       (node-right s)))
           ((> x y)
            (make-node (node-left s)
                       y
                       (btree-delete (node-right s) x)))))))))

; Kombiniere zwei SB zu einem (Vorbedingung: alle Marken im linken Arg < alle Marken im rechten Arg)
(: btree-combine (btree btree -> btree))
(define btree-combine
  (lambda (l r)
    (cond
      ((empty-tree? l)
       r)
      ((node? l)
       (cond
         ((empty-tree? r)
          l)
         ((node? r)
          (let ((min+tree (btree-delete-min r)))
            (make-node l (first min+tree) (first (rest min+tree))))))))))

; Entferne aus nichtleerem Binärem Suchbaum das minimale Element
; Ergebnis: das Element + verbleibende SB (als zwei-elementige Liste)
(: btree-delete-min (node -> (list %v)))
(define btree-delete-min
  (lambda (s)
    (let ((l (node-left s)))
      (cond
        ((empty-tree? l)
         (list (node-label s) (node-right s)))
        ((node? l)
         (let ((min+tree (btree-delete-min l)))
           (list (first min+tree)
                 (make-node (first (rest min+tree)) (node-label s) (node-right s)))))))))

; Tests
(check-expect (btree-delete (make-leaf 55) 55)
              (make-empty-tree))
(check-expect (btree-delete (make-leaf 66) 55)
              (make-leaf 66))
(check-expect (btree-delete (make-node (make-leaf 55) 66 (make-leaf 77)) 55)
              (make-node (make-empty-tree) 66 (make-leaf 77)))
(check-expect (btree-delete (make-node (make-leaf 55) 66 (make-leaf 77)) 66)
              (make-node (make-leaf 55) 77 (make-empty-tree)))

(define xxx
  (lambda () 
    (visualize-1 (btree-insert (btree-insert (btree-insert t3 11) 5.5) 42))))









