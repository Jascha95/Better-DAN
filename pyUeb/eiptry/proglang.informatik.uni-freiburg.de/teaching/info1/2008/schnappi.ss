;; Die ersten drei Zeilen dieser Datei wurden von DrScheme eingefügt. Sie enthalten Metadaten
;; über die Sprachebene dieser Datei in einer Form, die DrScheme verarbeiten kann.
#reader(lib "DMdA-vanilla-reader.ss" "deinprogramm")((modname schnappi) (read-case-sensitive #f) (teachpacks ()) (deinprogramm-settings #(#f write repeating-decimal #t #t none explicit #f ())))
(define subject
  "Ich bin Schnappi, das kleine Krokodil
Hab scharfe Zähne, und davon ganz schön viel
Ich schnapp mir was ich schnappen kann
Ja ich schnapp zu, weil ich das so gut kann")

; Ein Eintrag in einem Histogramm ist ein Wert
;  (make-entry s c)
; wobei s : string der Datenwert ist und
;       c : natural die Anzahl der Vorkommen des Datenwerts
(define-record-procedures entry
  make-entry entry?
  (entry-item entry-count))

; Ein Histogramm ist eine Liste von Histogrammeinträgen
(define-contract histogram (list entry))

; Einen neuen Datenwert ins Histogramm aufnehmen
(: histogram-insert-item (string histogram -> histogram))
(define histogram-insert-item
  (lambda (item histo)
    (cond
      ((empty? histo)
       (list (make-entry item 1)))
      ((pair? histo)
       (let ((entry (first histo)))
         (if (string=? (entry-item entry) item)
             (make-pair (make-entry item (+ 1 (entry-count entry)))
                        (rest histo))
             (make-pair entry
                        (histogram-insert-item item (rest histo)))))))))
(check-expect (histogram-insert-item "xx" empty)
              (list (make-entry "xx" 1)))
(check-expect (histogram-insert-item "xx" (histogram-insert-item "xx" empty))
              (list (make-entry "xx" 2)))
(check-expect (histogram-insert-item "zz" (histogram-insert-item "xx" (histogram-insert-item "xx" empty)))
              (list (make-entry "xx" 2) (make-entry "zz" 1)))

; Eine Liste neuer Datenwerte ins Histogramm aufnehmen
(: histogram-insert-items ((list string) histogram -> histogram))
(define histogram-insert-items
  (lambda (items histo)
    (cond
      ((empty? items)
       histo)
      ((pair? items)
       (histogram-insert-items (rest items) (histogram-insert-item (first items) histo))))))
(check-expect (histogram-insert-items (list) empty)
              empty)
(check-expect (histogram-insert-items (list "xx" "xx") empty)
              (list (make-entry "xx" 2)))
(check-expect (histogram-insert-items (list "xx" "yy" "xx") empty)
              (list (make-entry "xx" 2) (make-entry "yy" 1)))

; Aus einem String das Histogramm der Buchstaben ermitteln
(: make-histogram (string -> histogram))
(define make-histogram
  (lambda (str)
    (histogram-insert-items (string->strings-list str) empty)))
(check-expect (make-histogram "")
              (list))
(check-expect (make-histogram "xyz")
              (list (make-entry "x" 1) (make-entry "y" 1) (make-entry "z" 1)))
(check-expect (make-histogram "xyx")
              (list (make-entry "x" 2) (make-entry "y" 1)))
