(require (planet schematics/schemeunit:3:4))
  
; nth-element: List * Int -> SchemeVal
; usage: (nth-element lst n) = the nth element of lst
(define nth-element
  (lambda (lst n)
    (if (null? lst)
        (error "list to short by ~s elements.~%" n)
        (if (zero? n)
            (car lst)
            (nth-element (cdr lst) (- n 1))))))

(check-equal? (nth-element '(a b c d) 2) 'c)
(check-equal? (nth-element (list 1 2 3) 0) 1)

; occurs-free?: Sym * LcExp -> Bool
; usage: (occurs-free? sym exp) returns true if symbol sym
;        occurs free in lambda expression exp, otherwise
;        false is returned
(define occurs-free?
  (lambda (sym exp)
    (cond
      ((symbol? exp) (equal? sym exp))
      ((equal? (car exp) 'lambda)
       (and (not (equal? sym (car (cadr exp))))
            (occurs-free? sym (caddr exp))))
      (else
       (or (occurs-free? sym (car exp))
           (occurs-free? sym (cadr exp)))))))

(check-equal? (occurs-free? 'x '(lambda (x) (x y))) #f)
(check-equal? (occurs-free? 'y '(lambda (x) (x y))) #t)

; duple: Nat * SchemeVal -> List
; usage: (duple n x) returns a list with n copies of x
(define duple
  (lambda (n x)
    (if (zero? n)
        '()
        (cons x (duple (- n 1) x)))))

(check-equal? (duple 3 'a) '(a a a))
(check-equal? (duple 0 1) '())
    
; down: List -> List
; usage: (down lst) wraps parantheses around each top-level element of lst.
(define down
  (lambda (lst)
    (if (null? lst)
        '()
        (cons (list (car lst)) (down (cdr lst))))))

(check-equal? (down '(1 2 3)) '((1) (2) (3)))
(check-equal? (down (list 1 2 3)) '((1) (2) (3)))

; count-occurrences: Sym * List -> Nat
; usage: (count-occurences s lst) returns the number of occurrences of x in lst (recursively).
(define count-occurrences
  (lambda (x lst)
    (cond
      ((null? lst) 0)
      ((list? (car lst))
       (+ (count-occurrences x (car lst)) 
          (count-occurrences x (cdr lst))))
      (else
       (+ (if (equal? x (car lst)) 1 0)
          (count-occurrences x (cdr lst)))))))

(check-equal? (count-occurrences 'x '(x (f x) (g (g y x)))) 3)

; product: List(Sym) * List(Sym) -> List
; usage: (produce sos1 sos2) returns a list of 2-lists that represents the
;        cartesian product of sos1 and sos2
(define product
  (lambda (sos1 sos2)
    (if (or (null? sos1)
            (null? sos2))
        '()
        (let 
            ((s1 (car sos1)))
          (append (map (lambda (s2) (list s1 s2)) sos2)
                  (product (cdr sos1) sos2))))))

(check-equal? (product '(a b c) '(x y)) '((a x) (a y) (b x) (b y) (c x) (c y)))

; flatten: List(Sym) -> List(Sym)
; usage: (flatten slist) returns a list of all symbols contained in slist.
(define flatten
  (lambda (lst)
    (cond 
      ((null? lst) '())
      ((symbol? (car lst)) (cons (car lst) (flatten (cdr lst))))
      ((list? (car lst)) (append (flatten (car lst)) (flatten (cdr lst))))
      (else (flatten (cdr lst))))))

(check-equal? (flatten '(a b c)) '(a b c))
(check-equal? (flatten '((a) () (b ()) () (c))) '(a b c))
(check-equal? (flatten '((a b) c (((d)) e))) '(a b c d e))

; exists?: (SchemeVal -> Bool) * List -> Bool
; usage: (exists? pred lst) returns true if, and only if, lst contains
;        an element satisfying pred.
(define exists?
  (lambda (pred lst)
    (if (null? lst)
        #f
        (or (pred (car lst))
            (exists? pred (cdr lst))))))

(check-equal? (exists? number? '(a b 1 c)) #t)
(check-equal? (exists? number? '(a b c)) #f)