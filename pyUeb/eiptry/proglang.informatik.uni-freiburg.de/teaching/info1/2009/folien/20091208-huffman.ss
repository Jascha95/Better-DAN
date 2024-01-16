;; Die ersten drei Zeilen dieser Datei wurden von DrScheme eingefügt. Sie enthalten Metadaten
;; über die Sprachebene dieser Datei in einer Form, die DrScheme verarbeiten kann.
#reader(lib "DMdA-vanilla-reader.ss" "deinprogramm")((modname 20091208-huffman) (read-case-sensitive #f) (teachpacks ((lib "image.ss" "teachpack" "deinprogramm"))) (deinprogramm-settings #(#f write repeating-decimal #t #t none explicit #f ((lib "image.ss" "teachpack" "deinprogramm")))))
; Ein Huffman-Blatt ist ein Wert
; (make-huffman-leaf s w)
; wobei s : string
; und w : real (das Gewicht bzw die Haufigkeit von s)
(define-record-procedures huffman-leaf
  make-huffman-leaf huffman-leaf?
  (huffman-leaf-name huffman-leaf-weight))
(: make-huffman-leaf (string real -> huffman-leaf))

; Ein Huffman-Knoten ist ein Wert
; (make-huffman-node sl w l r)
; wobei sl : list(string)
; w : real (kumulative Haufigkeit von sl)
; l, r : huffman-tree
(define-record-procedures huffman-node
  really-make-huffman-node huffman-node?
  (huffman-node-names huffman-node-weight
                      huffman-node-left huffman-node-right))
(: really-make-huffman-node ((list string) real huffman-tree huffman-tree -> huffman-tree))
; Ein Huffman-Baum ist einer der folgenden
; - ein Huffman-Blatt
; - ein Huffman-Knoten
(define huffman-tree
  (contract (mixed huffman-leaf huffman-node)))

; Huffman-Knoten aus zwei Huffman-Baumen konstruieren
(: make-huffman-node (huffman-tree huffman-tree -> huffman-node))
(define make-huffman-node
  (lambda (l r)
    (really-make-huffman-node
     (append (huffman-tree-names l)
             (huffman-tree-names r))
     (+ (huffman-tree-weight l)
        (huffman-tree-weight r))
     l r)))
(check-expect (make-huffman-node (make-huffman-leaf "xx" 1)
                                 (make-huffman-leaf "yy" 2))
              (really-make-huffman-node (list "xx" "yy")
                                        3
                                        (make-huffman-leaf "xx" 1)
                                        (make-huffman-leaf "yy" 2)))

; Nach Topdown-Design verbleibt zu denieren
(: huffman-tree-names (huffman-tree -> (list string)))
(define huffman-tree-names
  (lambda (ht)
    (cond
      ((huffman-leaf? ht)
       (list (huffman-leaf-name ht)))
      ((huffman-node? ht)
       (huffman-node-names ht)))))
(check-expect (huffman-tree-names (make-huffman-leaf "xx" 1))
              (list "xx"))
(check-expect (huffman-tree-names (really-make-huffman-node (list "xx" "yy")
                                                            3
                                                            (make-huffman-leaf "xx" 1)
                                                            (make-huffman-leaf "yy" 2)))
              (list "xx" "yy"))

(define ht-xx-yy
  (really-make-huffman-node (list "xx" "yy")
                            3
                            (make-huffman-leaf "xx" 1)
                            (make-huffman-leaf "yy" 2)))

; Gesamtgewicht eines Huffman Baums
(: huffman-tree-weight (huffman-tree -> real))
(define huffman-tree-weight
  (lambda (ht)
    (cond
      ((huffman-leaf? ht)
       (huffman-leaf-weight ht))
      ((huffman-node? ht)
       (huffman-node-weight ht)))))

; Ein Bit ist entweder 0 oder 1
(define bit
  (contract (one-of 0 1)))

; Erstes Zeichen dekodieren
(: decode-1 ((list bit) huffman-tree -> string))
(define decode-1
  (lambda (bits ht)
    (cond
      ((huffman-leaf? ht)
       (huffman-leaf-name ht))
      ((huffman-node? ht)
       (cond
         ((empty? bits)
          (violation "Illegaler Codestring"))
         ((pair? bits)
          (let ((bit (first bits))
                (bits (rest bits)))
            (if (zero? bit)
                (decode-1 bits (huffman-node-left ht))
                (decode-1 bits (huffman-node-right ht))))))))))

(check-expect (decode-1 (list 0) ht-xx-yy)
              "xx")
(check-expect (decode-1 (list 1) ht-xx-yy)
              "yy")

; Huffman-codierte Bitfolge decodieren
(: huffman-decode ((list bit) huffman-tree -> (list string)))
(define huffman-decode
  (lambda (bits ht-root)
    (letrec ((decode-1
              (lambda (bits ht)
                (cond
                  ((huffman-leaf? ht)
                   (make-pair (huffman-leaf-name ht)
                              (decode-2 bits ht-root)))
                  ((huffman-node? ht)
                   (cond
                     ((empty? bits)
                      (violation "Illegaler Codestring"))
                     ((pair? bits)
                      (let ((bit (first bits))
                            (bits (rest bits)))
                        (if (zero? bit)
                            (decode-1 bits (huffman-node-left ht))
                            (decode-1 bits (huffman-node-right ht))))))))))
             (decode-2
              (lambda (bits ht)
                (if (empty? bits)
                    empty
                    (decode-1 bits ht)))))
      (decode-2 bits ht-root))))

; Codieren einer Liste von Symbolen
(: huffman-encode ((list string) huffman-tree -> (list bit)))
(define huffman-encode
  (lambda (symbols ht)
    (cond
      ((empty? symbols)
       empty)
      ((pair? symbols)
       (let((sym (first symbols))
            (symbols (rest symbols)))
         (append (huffman-encode-name sym ht)
                 (huffman-encode symbols ht)))))))

(: list-member (string  (list string) -> boolean))
(define list-member
  (lambda (x xs)
    (if (empty? xs)
        #f
        (let((y (first xs))
             (xs (rest xs)))
          (if (string=? x y)
              #t
              (list-member x xs))))))

; Codieren eines Symbols (Annahme dabei: das Symbol kommt im HB vor)
(: huffman-encode-name (string huffman-tree -> (list bit)))
(define huffman-encode-name
  (lambda (sym ht)
    (cond
      ((huffman-leaf? ht)
       (if (string=? sym (huffman-leaf-name ht))
           empty
           (violation "Symbol nicht im HB vorhanden")))
      ((huffman-node? ht)
       (let ((left (huffman-node-left ht))
             (right (huffman-node-right ht)))
         (if (list-member sym (huffman-tree-names left))
             (make-pair 0 (huffman-encode-name sym left))
             (make-pair 1 (huffman-encode-name sym right))))))))
(check-expect (huffman-encode-name "xx" ht-xx-yy)
              (list 0))
(check-expect (huffman-encode-name "yy" ht-xx-yy)
              (list 1))

(check-property
 (for-all ((inp (list (one-of "xx" "yy"))))
   (expect (huffman-decode (huffman-encode inp ht-xx-yy) ht-xx-yy)
           inp)))

























