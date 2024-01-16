;; Die ersten drei Zeilen dieser Datei wurden von DrScheme eingefügt. Sie enthalten Metadaten
;; über die Sprachebene dieser Datei in einer Form, die DrScheme verarbeiten kann.
#reader(lib "DMdA-vanilla-reader.ss" "deinprogramm")((modname |20081202|) (read-case-sensitive #f) (teachpacks ((lib "world.ss" "teachpack" "deinprogramm"))) (deinprogramm-settings #(#f write repeating-decimal #t #t none explicit #f ((lib "world.ss" "teachpack" "deinprogramm")))))
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

(define-contract bit (one-of 0 1))

; codiere eine Zeichenfolge 
(: huffman-encode (huffman-tree (list string) -> (list bit)))

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

