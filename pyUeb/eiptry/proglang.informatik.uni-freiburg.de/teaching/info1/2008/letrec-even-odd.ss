;; Die ersten drei Zeilen dieser Datei wurden von DrScheme eingefügt. Sie enthalten Metadaten
;; über die Sprachebene dieser Datei in einer Form, die DrScheme verarbeiten kann.
#reader(lib "DMdA-vanilla-reader.ss" "deinprogramm")((modname letrec-even-odd) (read-case-sensitive #f) (teachpacks ((lib "world.ss" "teachpack" "deinprogramm"))) (deinprogramm-settings #(#f write repeating-decimal #t #t none explicit #f ((lib "world.ss" "teachpack" "deinprogramm")))))
; bestimme zu natürlicher Zahl, ob gerade oder ungerade
(: even (natural -> boolean))
(: odd (natural -> boolean))

(define even
  (lambda (n)
    (if (zero? n)
        #t
        (not (even (- n 1))))))
(check-expect (even 0) #t)

(define odd 
  (lambda (n)
    (if (zero? n)
        #f
        (not (odd (- n 1))))))

(letrec 
    ((even 
      (lambda (n)
        (if (zero? n)
            #t
            (odd (- n 1)))))
     (odd 
      (lambda (n)
        (if (zero? n)
            #f
            (even (- n 1))))))
  (odd 15))
