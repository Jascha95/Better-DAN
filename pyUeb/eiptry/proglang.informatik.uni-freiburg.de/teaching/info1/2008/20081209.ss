;; Die ersten drei Zeilen dieser Datei wurden von DrScheme eingefügt. Sie enthalten Metadaten
;; über die Sprachebene dieser Datei in einer Form, die DrScheme verarbeiten kann.
#reader(lib "DMdA-vanilla-reader.ss" "deinprogramm")((modname |20081209|) (read-case-sensitive #f) (teachpacks ((lib "world.ss" "teachpack" "deinprogramm"))) (deinprogramm-settings #(#f write repeating-decimal #t #t none explicit #f ((lib "world.ss" "teachpack" "deinprogramm")))))
; Ein Gast ist ein Wert
; (make-guest name sex veggie)
; wobei name : string
; sex : boolean (#t f¨ur m¨annlich, #f f¨ur weiblich)
; veggie : boolean
(define-record-procedures guest
  make-guest guest?
  (guest-name guest-male? guest-vegetarian?))

; aus einer G¨asteliste die Liste der G¨aste extrahieren,
; die ein Pr¨adikat erf¨ullen
(: guests-filter ((guest -> boolean) (list guest) -> (list guest)))
(define guests-filter
  (lambda (pred? guests)
    (cond
      ((empty? guests)
       empty)
      ((pair? guests)
       (let* ((guest (first guests))
              (result (guests-filter pred? (rest guests))))
         (if (pred? guest)
             (make-pair guest result)
             result))))))

; Liste der Vegetarian extrahieren
(: guests-vegetarian ((list guest) -> (list guest)))
(define guests-vegetarian
  (lambda (guests)
    (guests-filter guest-vegetarian? guests)))

; Liste der Männer extrahieren
(: guests-male ((list guest) -> (list guest)))
(define guests-male
  (lambda (guests)
    (guests-filter guest-male? guests)))

; ist männlicher Vegetarier
(: male-veggie? (guest -> boolean))
(define male-veggie?
  (lambda(g)
    (and (guest-male? g)
         (guest-vegetarian? g))))


(define guests (list (make-guest "Lilli" #f #t) (make-guest "Rosalie" #f #f)
                       (make-guest "Bello" #t #f) (make-guest "Egon" #t #f)))
(define male-guests
  (guests-male guests))
(define veggie-guests
  (guests-vegetarian guests))
(define male-veggie-guests
  (guests-filter male-veggie? guests))
(define female-guests
  (lambda (guests)
    (guests-filter (lambda (g)
                     (not (guest-male? g)))
                   guests)))
; Elemente einer Liste auffalten
(: list-fold (%b (%a %b -> %b) (list %a) -> %b))
(define list-fold
  (lambda (e f l)
    (cond
      ((empty? l)
       e)
      ((pair? l)
       (f (first l) (list-fold e f (rest l)))))))

; filter mit list-fold und anonymer Prozedur
(define filter
  (lambda (pred? xs)
    (list-fold empty
               (lambda (elem result)
                 (if (pred? elem)
                     (make-pair elem result)
                     result))
               xs)))
