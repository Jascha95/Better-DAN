;; Die ersten drei Zeilen dieser Datei wurden von DrScheme eingefügt. Sie enthalten Metadaten
;; über die Sprachebene dieser Datei in einer Form, die DrScheme verarbeiten kann.
#reader(lib "DMdA-vanilla-reader.ss" "deinprogramm")((modname 20071127-info1-1) (read-case-sensitive #f) (teachpacks ()) (deinprogramm-settings #9(#f write repeating-decimal #t #t none explicit #f ())))
; Ein Gast ist ein Wert
; (make-guest name sex veggie)
; wobei name : string
; sex : boolean (#t fur mannlich, #f fur weiblich)
; veggie : boolean
(define-record-procedures guest
make-guest guest?
(guest-name guest-male? guest-vegetarian?))

; aus einer Gasteliste die Liste der Gaste extrahieren,
; die ein Pradikat erfullen
; filter-guests : (guest -> boolean) list(guest) -> list(guest)
(define filter-guests
  (lambda (pred? guests)
    (cond
      ((empty? guests)
       empty)
      ((pair? guests)
       (let* ((guest (first guests))
              (result (filter-guests pred? (rest guests))))
         (if (pred? guest)
             (make-pair guest result)
             result))))))

(define my-guests
  (list (make-guest "Klaus" #t #f)
        (make-guest "Eva" #f #t)
        (make-guest "Beowulf" #t #f)
        (make-guest "Bruce" #t #t)
        (make-guest "Britney" #f #t)))

; Liste der männlichen Gäste
; male-guests : list(guest) -> list(guest)
(define male-guest
  (lambda (guests)
    (filter-guests guest-male? guests)))

; liste der Vegetarier
; vegetarian-guests : list(guest) -> list(guest)
(define vegetarian-guest
  (lambda (guests)
    (filter-guests guest-vegetarian? guests)))

; Liste der weiblichen Gäste
; female-guests : list(guest) -> list(guest)
(define female-guests
  (lambda (guests)
    (filter-guests female-guest? guests)))

; Testen ob ein Gast weiblich ist
; female-guest? : guest -> boolean
(define female-guest?
  (lambda (guest)
    (not (guest-male? guest))))

; female-guests-2 : list(guest) -> list(guest)
(define female-guests-2
  (lambda (guests)
    (filter-guests (lambda (guest) (not (guest-male? guest)))
                   guests)))

; Elemente einer Liste auffalten
; list-fold : number (number number -> number) list(number) -> number
(define list-fold
  (lambda (e f l)
    (cond
      ((empty? l)
       e)
      ((pair? l)
       (f (first l) (list-fold e f (rest l)))))))

; Liste aufsummieren
; list-sum : list(number) -> number
(define list-sum
  (lambda (ls)
    (list-fold 0 + ls)))

; Liste aufmultiplizieren
; list-mult : list(number) -> nunmber
(define list-mult
  (lambda (ls)
    (list-fold 1 * ls)))

; Hilfsfunktion zur Definition von length mit Hilfe von list-fold
; add-1-for-length : A nat -> nat
(define add-1-for-length
  (lambda (ignore n)
    (+ 1 n)))

; Lange einer Liste
; my-length : list(A) -> nat
(define my-length
  (lambda (xs)
    (list-fold 0 add-1-for-length xs)))

; filter : (A -> boolean) list(A) -> list(A)
(define filter
  (lambda (pred? xs)
    (list-fold empty
               (lambda (elem result)
                 (if (pred? elem)
                     (make-pair elem result)
                     result))
               xs)))

; zwei Prozeduren komponieren (zusammensetzen, hintereinander ausfuhren)
; compose : (Y -> Z) (X -> Y) -> (X -> Z)
(define compose
  (lambda (f g)
    (lambda (x) (f (g x)))))
