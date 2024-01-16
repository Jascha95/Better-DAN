;; Die ersten drei Zeilen dieser Datei wurden von DrScheme eingefügt. Sie enthalten Metadaten
;; über die Sprachebene dieser Datei in einer Form, die DrScheme verarbeiten kann.
#reader(lib "DMdA-vanilla-reader.ss" "deinprogramm")((modname |20081202|) (read-case-sensitive #f) (teachpacks ((lib "world.ss" "teachpack" "deinprogramm"))) (deinprogramm-settings #(#f write repeating-decimal #t #t none explicit #f ((lib "world.ss" "teachpack" "deinprogramm")))))
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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


; Ein Huffman-Blatt ist ein Wert
; (make-huffman-leaf s w)
; wobei s : string
; und w : number (das Gewicht bzw die Haufigkeit von s)
(define-record-procedures huffman-leaf
  make-huffman-leaf huffman-leaf?
  (huffman-leaf-name huffman-leaf-weight))

; Ein Huffman-Knoten ist ein Wert
; (make-huffman-node sl w l r)
; wobei sl : list(string)
; w : number (kumulative Haufigkeit von sl)
; l, r : huffman-tree
(define-record-procedures huffman-node
  really-make-huffman-node huffman-node?
  (huffman-node-names huffman-node-weight
                      huffman-node-left huffman-node-right))

(define-contract huffman-tree
  (mixed huffman-leaf huffman-node))

; extrahiere aus Huffmanbaum die Symbole, die er repräsentiert
(: huffman-tree-names (huffman-tree -> (list string)))
(define huffman-tree-names
  (lambda (ht)
    (cond
      ((huffman-leaf? ht)
       (list (huffman-leaf-name ht)))
      ((huffman-node? ht)
       (huffman-node-names ht)))))

(: huffman-tree-weight (huffman-tree -> real))
(define huffman-tree-weight
  (lambda (ht)
    (cond
      ((huffman-leaf? ht)
       (huffman-leaf-weight ht))
      ((huffman-node? ht)
       (huffman-node-weight ht)))))

(define-contract bit (one-of 0 1))

; teste ob string in einer Liste von Strings vorkommt
(: string-member? (string (list string) -> boolean))
(define string-member?
  (lambda (s ss)
    (cond
      ((empty? ss)
       #f)
      ((pair? ss)
       (if (string=? s (first ss))
           #t
           (string-member? s (rest ss)))))))

; codiere ein Zeichen
(: huffman-encode-name (string huffman-tree -> (list bit)))
(define huffman-encode-name
  (lambda (name ht)
    (cond
      ((huffman-leaf? ht)
       empty)
      ((huffman-node? ht)
       (let ((left (huffman-node-left ht))
             (right (huffman-node-right ht)))
         (if (string-member? name (huffman-tree-names left))
             (make-pair 0 (huffman-encode-name name left))
             (make-pair 1 (huffman-encode-name name right))))))))

; codiere eine Zeichenfolge 
(: huffman-encode ((list string) huffman-tree -> (list bit)))
(define huffman-encode
  (lambda (names ht)
    (cond
      ((empty? names)
       empty)
      ((pair? names)
       (append (huffman-encode-name (first names) ht)
               (huffman-encode (rest names) ht))))))

; decodiere eine Folge von Bits in eine Folge von Symbolen
(: huffman-decode (huffman-tree (list bit) -> (list string)))
(define huffman-decode
  (lambda (ht-root bits)
    (letrec
        ((decode
          (lambda (ht bits)
            (cond
              ((huffman-leaf? ht)
               (make-pair (huffman-leaf-name ht)
                          (decode ht-root bits)))
              ((huffman-node? ht)
               (cond
                 ((empty? bits)
                  empty)
                 ((pair? bits)
                  (let ((bit (first bits)))
                    (if (= 0 bit)
                        (decode (huffman-node-left ht) (rest bits))
                        (decode (huffman-node-right ht) (rest bits)))))))))))
      (decode ht-root bits))))

; decodiere eine Folge von Bits zum ersten Symbol
(: decode (huffman-tree (list bit) -> (list string)))
(define decode
  (lambda (ht bits)
    (cond
      ((huffman-leaf? ht)
       (list (huffman-leaf-name ht)))
      ((huffman-node? ht)
       (cond
         ((empty? bits)
          empty)
         ((pair? bits)
          (let ((bit (first bits)))
            (if (= 0 bit)
                (decode (huffman-node-left ht) (rest bits))
                (decode (huffman-node-right ht) (rest bits)))))))
      )))

; Knoten im Huffmanbaum erstellen
(: make-huffman-tree (huffman-tree huffman-tree -> huffman-tree))
(define make-huffman-tree
  (lambda (left right)
    (let ((weight (+ (huffman-tree-weight left)
                     (huffman-tree-weight right)))
          (names (append (huffman-tree-names left)
                         (huffman-tree-names right))))
    (really-make-huffman-node names weight left right))))

; finde Huffmanbaum mit minimalem Gewicht aus nicht-leerer Liste
(: huffman-minimum ((list huffman-tree) -> huffman-tree))
(define huffman-minimum
  (lambda (hts)
    (let ((ht-min (first hts))
          (hts1 (rest hts)))
      (letrec ((mini 
                (lambda (ht-min hts)
                  (cond
                    ((empty? hts)
                     ht-min)
                    ((pair? hts)
                     (let ((candidate (first hts)))
                       (if (< (huffman-tree-weight candidate)
                              (huffman-tree-weight ht-min))
                           (mini candidate (rest hts))
                           (mini ht-min (rest hts)))))))))
        (mini ht-min hts1)))))

; teste zwei Huffmanbäume auf Gleichheit
(: huffman-equal? (huffman-tree huffman-tree -> boolean))
(define huffman-equal?
  (lambda (ht1 ht2)
    (let ((names1 (huffman-tree-names ht1))
          (names2 (huffman-tree-names ht2)))
      (string=? (strings-list->string names1)
                (strings-list->string names2)))))

; lösche einen Baum aus einer Liste von Huffmanbäumen
(: huffman-delete ((list huffman-tree) huffman-tree -> (list huffman-tree)))
(define huffman-delete
  (lambda (hts ht)
    (cond
      ((empty? hts)
       empty)
      ((pair? hts)
       (let ((ht0 (first hts)))
         (if (huffman-equal? ht ht0)
             (rest hts)
             (make-pair ht0 (huffman-delete (rest hts) ht))))))))

; erstelle einen Huffmanbaum aus einer nicht-leeren Liste von Huffmanbäumen
(: huffman-construct ((list huffman-tree) -> huffman-tree))
(define huffman-construct
  (lambda (hts)
    (if (< (length hts) 2)
        (first hts)
        (let* ((ht1 (huffman-minimum hts))
               (hts1 (huffman-delete hts ht1))
               (ht2 (huffman-minimum hts1))
               (hts2 (huffman-delete hts1 ht2))
               (ht3 (make-huffman-tree ht1 ht2))
               (hts4 (make-pair ht3 hts2)))
          (huffman-construct hts4)))))

(define trees (list (make-huffman-leaf "x" 5) 
                    (make-huffman-leaf "y" 10)
                    (make-huffman-leaf "z" 1)))

; konvertiere ein Histogramm in eine Liste von Huffmanblättern
(: convert (histogram -> (list huffman-tree)))
(define convert
  (lambda (histo)
    (cond
      ((empty? histo)
       empty)
      ((pair? histo)
       (let ((entry (first histo)))
       (make-pair (make-huffman-leaf (entry-item entry) (entry-count entry))
                  (convert (rest histo))))))))

(define schnappi-tree (huffman-construct (convert (make-histogram subject))))
(define schnappi-text (string->strings-list subject))
(define schnappi-encoded (huffman-encode schnappi-text schnappi-tree))


