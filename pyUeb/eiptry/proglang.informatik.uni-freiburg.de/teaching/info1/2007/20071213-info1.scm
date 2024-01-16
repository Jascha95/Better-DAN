;; Die ersten drei Zeilen dieser Datei wurden von DrScheme eingefügt. Sie enthalten Metadaten
;; über die Sprachebene dieser Datei in einer Form, die DrScheme verarbeiten kann.
#reader(lib "DMdA-vanilla-reader.ss" "deinprogramm")((modname 20071213-info1) (read-case-sensitive #f) (teachpacks ((lib "world.ss" "teachpack" "deinprogramm"))) (deinprogramm-settings #9(#f write repeating-decimal #t #t none explicit #f ((lib "world.ss" "teachpack" "deinprogramm")))))
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
; Ein Huffman-Baum ist eins der folgenden
; - ein Huffman-Blatt
; - ein Huffman-Knoten
; Name: huffman-tree

; Huffman-Knoten aus zwei Huffman-Baumen konstruieren
; make-huffman-node : huffman-tree huffman-tree -> huffman-node
(define make-huffman-node
  (lambda (l r)
    (really-make-huffman-node
     (append (huffman-tree-names l)
             (huffman-tree-names r))
     (+ (huffman-tree-weight l)
        (huffman-tree-weight r))
     l r)))

; huffman-tree-names : huffman-tree -> list(string)
(define huffman-tree-names
  (lambda (ht)
    (cond
      ((huffman-leaf? ht)
       (list (huffman-leaf-name ht)))
      ((huffman-node? ht)
       (huffman-node-names ht)))))

; huffman-tree-weight : huffman-tree -> number
(define huffman-tree-weight
  (lambda (ht)
    (cond
      ((huffman-leaf? ht)
       (huffman-leaf-weight ht))
      ((huffman-node? ht)
       (huffman-node-weight ht)))))

; aufbauen eines Huffman-Baums aus einer nicht-leeren Liste von Blättern
; huffman-construct : list(huffman-tree) -> huffman-tree
(define huffman-construct
  (lambda (huffies)
    (let* ((min-huffies (minimum-huffies huffies))
           (min-huffy (first min-huffies))
           (rest-huffies (rest min-huffies)))
      (cond
        ((empty? rest-huffies)
         min-huffy)
        ((pair? rest-huffies)
         (let* ((min-huffies-2 (minimum-huffies rest-huffies))
                (min-huffy-2 (first min-huffies-2))
                (rest-rest-huffies (rest min-huffies-2))
                (new-huffy (make-huffman-node min-huffy min-huffy-2))
                (new-huffies (make-pair new-huffy rest-rest-huffies)))
           (huffman-construct new-huffies)))))))

; liefert den H-Baum minimaleen Gewichts aus einer nicht-leeren Liste, Ergebnis: umsortierte Liste mit minimalen Element am Anfang
; minimum-huffies : list(huffman-tree) -> list(huffman-tree)
(define minimum-huffies
  (lambda (huffies)
    (let ((huffy (first huffies))
          (rest-huffies (rest huffies)))
      (letrec ((find-minimum
                (lambda (huffies mini remaining-trees)
                  (cond
                    ((empty? huffies)
                     (make-pair mini remaining-trees))
                    ((pair? huffies)
                     (if (< (huffman-tree-weight (first huffies))
                            (huffman-tree-weight mini))
                         (find-minimum (rest huffies) (first huffies) 
                                       (make-pair mini remaining-trees))
                         (find-minimum (rest huffies) mini
                                       (make-pair (first huffies) remaining-trees))))))))
        (find-minimum rest-huffies huffy empty)))))

; liefert den H-Baum minimaleen Gewichts aus einer nicht-leeren Liste
; minimum-huffy : list(huffman-tree) -> huffman-tree
(define minimum-huffy
  (lambda (huffies)
    (let ((huffy (first huffies))
          (rest-huffies (rest huffies)))
      (letrec ((find-minimum
                (lambda (huffies mini)
                  (cond
                    ((empty? huffies)
                     mini)
                    ((pair? huffies)
                     (if (< (huffman-tree-weight (first huffies))
                            (huffman-tree-weight mini))
                         (find-minimum (rest huffies) (first huffies))
                         (find-minimum (rest huffies) mini)))))))
        (find-minimum rest-huffies huffy)))))




; Ein Bit ist entweder 0 oder 1 (Name: bit)
; Huffman-codierte Bitfolge decodieren
; huffman-decode : list(bit) huffman-tree -> list(string)
(define huffman-decode
  (lambda (bits top-ht)
    (letrec ((decode-1
              (lambda (bits ht)
                (cond
                  ((huffman-leaf? ht)
                   (make-pair (huffman-leaf-name ht)
                              (decode-1 bits top-ht)))
                  ((huffman-node? ht)
                   (cond
                     ((empty? bits)
                      empty)
                     ((pair? bits)
                      (let ((bit (first bits))
                            (rest-bits (rest bits)))
                        (cond
                          ((= bit 0)
                           (decode-1 rest-bits (huffman-node-left ht)))
                          ((= bit 1)
                           (decode-1 rest-bits (huffman-node-right ht))))))))))))
      (decode-1 bits top-ht))))


; Decodiere erstes Zeichen eines Huffmann-Kode
; huffman-decode-1 : list(bit) huffman-tree -> list(string)
(define huffman-decode-1
  (lambda (bits ht)
    (cond
      ((huffman-leaf? ht)
       (list (huffman-leaf-name ht)))
      ((huffman-node? ht)
       (cond
         ((empty? bits)
          empty)
         ((pair? bits)
          (let ((bit (first bits))
                (rest-bits (rest bits)))
            (cond
              ((= bit 0)
               (huffman-decode-1 rest-bits (huffman-node-left ht)))
              ((= bit 1)
               (huffman-decode-1 rest-bits (huffman-node-right ht)))))))))))


; CDU/CSU  	1483  	76  	224
; SPD 	145 	77 	222
; FDP 	- 	61 	61
; DIE LINKE. 	3 	50 	53
; BÜNDNIS 90/DIE GRÜNEN 	1 	50 	51
; Fraktionslose 	(1)1 	(1)2 	2
; letter-list : list(huffman-tree)
(define letter-list
  (list (make-huffman-leaf "CDU" 224)
        (make-huffman-leaf "SPD" 222)
        (make-huffman-leaf "FDP" 61)
        (make-huffman-leaf "DIE LINKE" 53)
        (make-huffman-leaf "Fraktionslose" 2)
        (make-huffman-leaf "DIE GRÜNEN" 51)))



; CDU : 0