;; Die ersten drei Zeilen dieser Datei wurden von DrScheme eingefügt. Sie enthalten Metadaten
;; über die Sprachebene dieser Datei in einer Form, die DrScheme verarbeiten kann.
#reader(lib "DMdA-vanilla-reader.ss" "deinprogramm")((modname 20091124-properties) (read-case-sensitive #f) (teachpacks ((lib "image.ss" "teachpack" "deinprogramm"))) (deinprogramm-settings #(#f write repeating-decimal #t #t none explicit #f ((lib "image.ss" "teachpack" "deinprogramm")))))
; Addition ist kommutativ
(check-property
 (for-all ((x number)
           (y number))
   (= (+ x y) (+ y x))))

; Addition ist assoziativ
(check-property
 (for-all ((x rational)
           (y rational)
           (z rational))
   (= (+ x (+ y z))
      (+ (+ x y) z))))

; Eigenschaften von Relationen
; Reflexivität
(define refl
  (lambda (p)
    (for-all ((x integer))
      (p x x))))

; Reflexivität der Gleichheit
(check-property (refl =))
; Reflexivität von < ???
; (check-property (refl <))

; Symmetrie: falls x R y dann auch y R x
(define symm
  (lambda(r)
    (for-all ((x integer)
              (y integer))
      (==> (r x y)
           (r y x)))))

; Symmetrie von =
(check-property (symm =))
; Symmetrie von < ???
; (check-property (symm <))


; empty ist Linksidentität für append
(check-property
 (for-all ((xs (list boolean)))
   (expect (append empty xs) 
          xs)))
; empty ist Rechtsidentität für append
(check-property
 (for-all ((xs (list boolean)))
   (expect (append  xs empty) 
          xs)))
; Assoziativität von append
(check-property
 (for-all ((xs (list boolean))
           (ys (list boolean))
           (zs (list boolean)))
   (expect (append xs (append ys zs)) 
           (append (append xs ys) zs))))

; Listen umdrehen
(: invert ((list %a) -> (list %a)))
;...

; (invert (invert xs))    ===  xs
; (invert (append xs ys)) === (append (invert ys) (invert xs))

