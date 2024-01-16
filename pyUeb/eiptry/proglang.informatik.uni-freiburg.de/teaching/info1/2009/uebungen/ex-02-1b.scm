;; Die ersten drei Zeilen dieser Datei wurden von DrScheme eingefügt. Sie enthalten Metadaten
;; über die Sprachebene dieser Datei in einer Form, die DrScheme verarbeiten kann.
#reader(lib "DMdA-vanilla-reader.ss" "deinprogramm")((modname code-1b) (read-case-sensitive #f) (teachpacks ()) (deinprogramm-settings #(#f write repeating-decimal #t #t none explicit #f ())))
(define a 1)
(define b 3.5)
(define c 5)
(define g
  (lambda (a)   
     ((lambda (b)
        ((lambda (c)
           (+ a b c))
         (+ a b c)))
      (+ a b c))))
(g c)