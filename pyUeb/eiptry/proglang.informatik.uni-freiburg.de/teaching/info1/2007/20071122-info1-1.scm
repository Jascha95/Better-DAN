;; Die ersten drei Zeilen dieser Datei wurden von DrScheme eingefügt. Sie enthalten Metadaten
;; über die Sprachebene dieser Datei in einer Form, die DrScheme verarbeiten kann.
#reader(lib "DMdA-vanilla-reader.ss" "deinprogramm")((modname 20071122-info1-1) (read-case-sensitive #f) (teachpacks ()) (deinprogramm-settings #9(#f write repeating-decimal #t #t none explicit #f ())))
; türme von Hanoi lösen

; Spielzustand besteht aus drei Säulen 1, 2, 3
; und n Scheiben unterschiedlicher Größe.
; Auf jede Säule können Scheiben aufgestapelt sein.

; Ein Spielzug besteht darin die oberste Scheibe von einer Säule auf eine andere zu bewegen.
; Einschränkung: Es darf niemals eine Scheibe auf einer kleineren liegen.

; Zum Beginn sind alle n Scheiben auf Säule 1.
; Aufgabe: finde eine Zugfolge, die allen Scheiben auf Säule 3 transportiert.

; Datenanalyse
; Anzahl der Scheiben: n (nat)
; Säulen: säule = {1, 2, 3}
; Spielzug: besteht aus Start- und Zielsäule (unterschiedlich)
; Zugfolge: list(Spielzug)

; Ein Spielzug ist ein Wert
;  (make-move f t)
; wobei f, t unterschiedliche Säulen sind.
(define-record-procedures hanoi-move
  make-move hanoi-move?
  (move-from move-to))

; Löse Türme von Hanoi
; hanoi : nat -> list(hanoi-move)
(define hanoi
  (lambda (n)
    (if (= n 0)
        empty
        (let ((h-n-1 (hanoi (- n 1))))
          (append
           (rename-pillars 2 3 h-n-1)
           (make-pair (make-move 1 3)
                      (rename-pillars 1 2 h-n-1))))
        )))

; Alternative Lösung, die keine berechneten Werte wegwirft
; hanoi-1 : nat -> list(hanoi-move)
(define hanoi-1
  (lambda (n)
    (hanoi-2 n 1 2 3)))

; transportiere Stapel der Größe n von Säule "from" auf Säule "to" unter Verwendung von Säule "help"
; hanoi-2 : nat säule säule säule -> list(hanoi-move)
(define hanoi-2
  (lambda (n from help to)
    (if (= n 0)
        empty
        (append
         (hanoi-2 (- n 1) from to help)
         (make-pair (make-move from to) 
                    (hanoi-2 (- n 1) help from to))
         ))))

; vertausche in einer Zugfolge zwei Säulen miteinander
; rename-pillars : säule säule list(hanoi-move) -> list(hanoi-move)
(define rename-pillars
  (lambda (a b mvs)
    (cond
      ((empty? mvs)
       empty)
      ((pair? mvs)
       (make-pair 
        (rename-pillar a b (first mvs))
        (rename-pillars a b (rest mvs)))))))

; vertausche in einem Zug zwei Säulen miteinander
; rename-pillar : säule säule hanoi-move -> hanoi-move
(define rename-pillar
  (lambda (a b mv)
    (make-move (rename-pin a b (move-from mv))
               (rename-pin a b (move-to mv)))))

; vertausche zwei Säulen miteinander
; rename-pin : säule säule säule -> säule
(define rename-pin
  (lambda (a b s)
    (cond
      ((= a s) b)
      ((= b s) a)
      (else s))))

; berechne die Anzahl der notwendigen Züge für (hanoi n)
; hanoi-count : nat -> nat
(define hanoi-count
  (lambda (n)
    (if (= n 0)
        0
        ( + 1 (* 2 (hanoi-count (- n 1)))))))

; Vermutung: (hanoi-count n) = 2^n - 1
; Ind Basis
; n=0
; (hanoi-count 0) => 0 = 2^0 - 1
;
; Ind Schritt
; n -> n+1
; (hanoi-count n+1)
; => (+ 1 (* 2 (hanoi-count n)))
; Ind Vor
; =   (+ 1 (* 2 (2^n-1)))
; =   1 + 2^(n+1) -2
; = 2^(n+1) - 1

