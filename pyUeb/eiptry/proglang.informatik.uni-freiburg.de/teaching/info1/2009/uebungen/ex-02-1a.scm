;; Die ersten drei Zeilen dieser Datei wurden von DrScheme eingefügt. Sie enthalten Metadaten
;; über die Sprachebene dieser Datei in einer Form, die DrScheme verarbeiten kann.
#reader(lib "DMdA-vanilla-reader.ss" "deinprogramm")((modname code-1a) (read-case-sensitive #f) (teachpacks ()) (deinprogramm-settings #(#f write repeating-decimal #t #t none explicit #f ())))
(define a 3)          ;  --> drei
(define b -10)        ;  --> minuszehn
(define c 42)         ;  --> antwort
(define f 
  (lambda (a c)
    (+ a b c)))
(f c a)

