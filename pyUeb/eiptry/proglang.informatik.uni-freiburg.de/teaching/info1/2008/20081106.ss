;; Die ersten drei Zeilen dieser Datei wurden von DrScheme eingefügt. Sie enthalten Metadaten
;; über die Sprachebene dieser Datei in einer Form, die DrScheme verarbeiten kann.
#reader(lib "DMdA-vanilla-reader.ss" "deinprogramm")((modname |20081106|) (read-case-sensitive #f) (teachpacks ()) (deinprogramm-settings #(#f write repeating-decimal #t #t none explicit #f ())))
; Summe der Zahlen 1 .. n
(: sum-to (natural -> natural))
(define sum-to
  (lambda (n)
    (if (zero? n)
        0
        (+ n (sum-to (- n 1))))))

(check-expect (sum-to 0) 0)
(check-expect (sum-to 4) 10)

; nichts tun
(: waste-time (%a -> %a))
(define waste-time
  (lambda(x)
    (waste-time x)))

; nicht die Fakultätsfunktion
(: notfac (natural -> natural))
(define notfac
  (lambda (n)
    (if (zero? n)
        1
        (* n (notfac n)))))

; auch nicht die Fakultätsfunktion
(define notfac2
  (lambda (n)
    (* n (notfac2 (- n 1)))))

;;; Die folgenden Definitionen schlagen fehl, 
;;; da das Ergebnis größer wird als der verfügbare Speicher.

; rekursive Definition der Fakultätsfunktion
(: factorial (natural -> natural))
(define factorial
  (lambda (n)
    (if (zero? n)
        1
        (* n (factorial (- n 1))))))

; Fakultät berechnen
(: it-factorial (natural -> natural))
(define it-factorial
  (lambda (n)
    (it-factorial-helper 1 n)))

; Iterative Definition
(: it-factorial-helper (natural natural -> natural))
(define it-factorial-helper
  (lambda (r n)
    (if (zero? n)
        r
        (it-factorial-helper (* r n) (- n 1)))))

;;; Nun verwenden wir modulo-Arithmetik um die Zahlen klein genug 
;;; zu halten. Ziel: Berechnung von (150000000!) modulo 150000001

;;; Dadurch bleiben die Zahlen klein genug, aber die iterative
;;; Definition bricht immer noch mangels Speicher ab.
;;; Der Grund hierfür ist der Vertrag der iterativen Funktion
;;; it-mod-factorial-helper, markiert mit ***.
;;; Sobald dieser Vertrag auskommentiert ist, kann mit
;;; it-mod-factorial die modulare Fakultätsfunktion für beliebig
;;; große Eingaben berechnet werden.
;;; (Es handelt sich hier um ein Implementierungsproblem des DrScheme Systems.)

; modulare Fakultät, rekursiv 
(: mod-factorial (natural -> natural))
(define mod-factorial
  (lambda (n)
    (if (zero? n)
        1
        (remainder (* n (mod-factorial (- n 1))) 150000001))))

; modulare Fakultät berechnen, iterativ
(: it-mod-factorial (natural -> natural))
(define it-mod-factorial
  (lambda (n)
    (it-mod-factorial-helper 1 n)))

; Der folgende Vertrag muss auskommentiert werden um
; z.B. (it-mod-factorial 150000000) zu berechnen.
(: it-mod-factorial-helper (natural natural -> natural))
(define it-mod-factorial-helper
  (lambda (r n)
    (if (zero? n)
        r
        (it-mod-factorial-helper (remainder (* r n) 150000001) (- n 1)))))

