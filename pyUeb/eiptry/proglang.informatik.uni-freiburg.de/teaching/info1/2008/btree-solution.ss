;; Die ersten drei Zeilen dieser Datei wurden von DrScheme eingefügt. Sie enthalten Metadaten
;; über die Sprachebene dieser Datei in einer Form, die DrScheme verarbeiten kann.
#reader(lib "DMdA-vanilla-reader.ss" "deinprogramm")((modname btree-solution) (read-case-sensitive #f) (teachpacks ()) (deinprogramm-settings #(#f write repeating-decimal #t #t none explicit #f ())))
(define-record-procedures empty-tree
  make-empty-tree empty-tree? ())

(define-record-procedures node
  make-node node?
  (node-left node-label node-right))

(define-contract btree (mixed empty-tree node))

(: make-leaf (%value -> btree))
(define make-leaf
  (lambda (x)
    (make-node (make-empty-tree) x (make-empty-tree))))

(define e (make-empty-tree))
(define n make-node)
(define l make-leaf)

; Entferne das minimale Element aus nicht-leerem Suchbaum
; ergibt Liste mit minimalem Element und verkleinertem Suchbaum
(: btree-remove-minimum (btree -> (list %value)))
(define btree-remove-minimum
  (lambda (t)
    (cond
      ((empty-tree? (node-left t))
       (list (node-label t) (node-right t)))
      ((node? (node-left t))
       (let ((root-and-new-left (btree-remove-minimum (node-left t))))
         (list (first root-and-new-left)
               (make-node (first (rest root-and-new-left)) (node-label t) (node-right t))))))))

(check-expect (btree-remove-minimum (l 42)) 
              (list 42 e))
(check-expect (btree-remove-minimum (n e 42 (l 99)))
              (list 42 (l 99)))
(check-expect (btree-remove-minimum (n (n e 42 (l 99)) 100 (l 150)))
              (list 42 (n (l 99) 100 (l 150))))

; Zusammensetzen zweier binärer Suchbäume
; wobei alle Schlüssel des linken Baums kleiner sind als die des rechten
(: btree-combine (btree btree -> btree))
(define btree-combine
  (lambda (left right)
    (cond
      ((empty-tree? left)
       right)
      ((node? left)
       (cond 
         ((empty-tree? right)
          left)
         ((node? right)
          ; neue Wurzel gesucht
          (let ((root-and-new-right (btree-remove-minimum right)))
            (make-node left
                       (first root-and-new-right)
                       (first (rest root-and-new-right))))))))))

; Löschen aus einem binären Suchbaum
(: btree-delete (btree real -> btree))
(define btree-delete
  (lambda (t y)
    (cond
      ((empty-tree? t)
       t)
      ((node? t)
       (let ((x (node-label t)))
         (cond
           ((= y x)
            (btree-combine (node-left t) (node-right t)))
           ((< y x)
            (make-node (btree-delete (node-left t) y) x (node-right t)))
           ((> y x)
            (make-node (node-left t) x (btree-delete (node-right t) y)))))))))

(check-expect (btree-delete e 55)
              e)
(check-expect (btree-delete (l 55) 55)
              e)
(check-expect (btree-delete (n (l 55) 66 (l 77)) 55)
              (n e 66 (l 77)))
(check-expect (btree-delete (n (l 55) 66 (l 77)) 66)
              (n (l 55) 77 e))
(check-expect (btree-delete (n (l 55) 66 (l 77)) 77)
              (n (l 55) 66 e))