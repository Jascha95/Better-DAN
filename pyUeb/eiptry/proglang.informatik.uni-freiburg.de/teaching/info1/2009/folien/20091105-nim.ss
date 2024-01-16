;; Die ersten drei Zeilen dieser Datei wurden von DrScheme eingefügt. Sie enthalten Metadaten
;; über die Sprachebene dieser Datei in einer Form, die DrScheme verarbeiten kann.
#reader(lib "DMdA-beginner-reader.ss" "deinprogramm")((modname 20091105-nim) (read-case-sensitive #f) (teachpacks ((lib "image.ss" "teachpack" "deinprogramm"))) (deinprogramm-settings #(#f write repeating-decimal #t #t none explicit #f ((lib "image.ss" "teachpack" "deinprogramm")))))
; Ein Nim-Spielstand ist ein Wert
;  (make-nim-score left right)
; wobei left und right den Vertrag natural erfüllen.
;  (   left, right : natural  )
(define-record-procedures nim-score
  make-nim-score nim-score?
  (nim-score-left nim-score-right))
(: make-nim-score (natural natural -> nim-score))
(: nim-score-left (nim-score -> natural))
(: nim-score-right (nim-score -> natural))

; Ein Zug vom linken Stapel ist ein Wert
;   (make-left-move n)
; wobei n : natural
(define-record-procedures left-move
  make-left-move left-move?
  (left-move-number))
(: make-left-move (natural -> left-move))

; Ein Zug vom rechten Stapel ist ein Wert
;   (make-right-move n)
; wobei n : natural
(define-record-procedures right-move
  make-right-move right-move?
  (right-move-number))
(: make-right-move (natural -> right-move))

; Ein Zug ist entweder
; - ein Zug vom linken Stapel
; - ein Zug vom rechten Stapel
(define nim-move (contract (mixed left-move right-move)))

; einen Zug ausführen
(: nim-execute (nim-score nim-move -> nim-score))
(define nim-execute
  (lambda (score move)
    (cond
      ((left-move? move)
       (make-nim-score (- (nim-score-left score) (left-move-number move))
                       (nim-score-right score)))
      ((right-move? move)
       (make-nim-score (nim-score-left score)
                       (- (nim-score-right score) (right-move-number move)))))))

(check-expect (nim-execute (make-nim-score 10 10) (make-left-move 2))
              (make-nim-score 8 10))
(check-expect (nim-execute (make-nim-score 0 2) (make-right-move 2))
              (make-nim-score 0 0))

;(check-expect (nim-execute (make-nim-score 0 2) (make-right-move 4))
;              (make-nim-score 0 -2))

; alternative Ausführung eines Zugs
(: nim-alternative (nim-score nim-move -> nim-score))
(define nim-alternative
  (lambda (score move)
    (make-nim-score
     (cond 
       ((left-move? move) (- (nim-score-left score) (left-move-number move)))
       ((right-move? move) (nim-score-left score)))
     (cond 
       ((left-move? move) (nim-score-right score))
       ((right-move? move) (- (nim-score-right score) (right-move-number move))))
     )))

(check-expect (nim-alternative (make-nim-score 10 10) (make-left-move 2))
              (make-nim-score 8 10))
(check-expect (nim-alternative (make-nim-score 0 2) (make-right-move 2))
              (make-nim-score 0 0))





