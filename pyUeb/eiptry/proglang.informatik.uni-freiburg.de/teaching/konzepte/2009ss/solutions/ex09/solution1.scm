(define id (lambda (x) x))

(define list-length/k
  (lambda (lst k)
    (if (null? lst)
        (k 0)
        (list-length/k (cdr lst) (lambda (n) (k (+ 1 n)))))))

(list-length/k '() id)
(list-length/k (list 1 2 3) id)

(define remove-first/k
  (lambda (s los k)
    (if (null? los)
        (k '())
        (if (eqv? (car los) s)
            (k (cdr los))
            (remove-first/k s (cdr los) (lambda (l) (k (cons (car los) l))))))))

(remove-first/k 'b '(a b c) id)

(define occurs-free?/k
  (lambda (var exp k)
    (cond
      ((symbol? exp) (k (eqv? var exp)))
      ((eqv? (car exp) 'lambda)
       (occurs-free?/k var (caddr exp)
                       (lambda (b) (k (and b (not (eqv? var (car (cadr exp)))))))))
      (else
        (occurs-free?/k var (car exp) 
                        (lambda (b1)
                          (occurs-free?/k var (cadr exp)
                                          (lambda (b2) (k (or b1 b2))))))))))

(occurs-free?/k 'a '(lambda (a) a) id)
(occurs-free?/k 'a '(lambda (b) a) id)

(define subst/k
  (lambda (new old slist k)
    (if (null? slist)
        (k '())
        (subst-in-s-exp/k new old (car slist)
                          (lambda (exp)
                            (subst/k new old (cdr slist)
                                     (lambda (exp-list) (k (cons exp exp-list)))))))))
        
(define subst-in-s-exp/k
  (lambda (new old sexp k)
    (if (symbol? sexp)
        (k (if (eqv? sexp old) new sexp))
        (subst/k new old sexp k))))

(subst/k 'x 'a '(a (b (c a)) (c a) ()) id)
