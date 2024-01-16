(require (planet schematics/schemeunit:3:4))

;; Exercise 1

;; empty-env: () -> Env
(define empty-env
  (lambda () '()))

;; extend-env: Var * SchemeVal * Env -> Env
(define extend-env
  (lambda (var val env)
    (cons (cons var val) env)))

;; apply-env: Env * Var -> SchemeVal
(define apply-env
  (lambda (env var)
    (cond 
      ((null? env)
       (eopl:error 'apply-env "No binding for ~s" var))
      ((eqv? var (caar env))
       (cdar env))
      (else (apply-env (cdr env) var)))))

(define sample-env
  (extend-env 'x 1 (extend-env 'y 2 (empty-env))))

(check-equal? (apply-env sample-env 'x) 1)
(check-equal? (apply-env sample-env 'y) 2)

;; Exercise 2

;; empty-env? : Env -> Bool
(define empty-env? null?)

;; has-binding? : Env * Var -> Bool
(define has-binding?
  (lambda (env var)
    (cond 
      ((null? env) #f)
      ((eqv? var (caar env)) #t)
      (else (has-binding? (cdr env) var)))))  

(check-equal? (has-binding? sample-env 'x) #t)
(check-equal? (has-binding? sample-env 'y) #t)
(check-equal? (has-binding? sample-env 'z) #f)


;; Exercise 3

(define-datatype bintree bintree?
  (leaf-node
   (num integer?))
  (interior-node
   (left bintree?)
   (right bintree?)))

;; bintree->list : Bintree -> Listof(Num)
(define bintree->list
  (lambda (t)
    (cases bintree t
      (leaf-node (x) (list x))
      (interior-node (left right) (append (bintree->list left) (bintree->list right))))))

(define sample-btree (interior-node (leaf-node 0) 
                                    (interior-node (leaf-node 1) (leaf-node 2))))

(check-equal? (bintree->list sample-btree) '(0 1 2))
(check-equal? (bintree->list (leaf-node 42)) '(42))


;; Exercise 4
(define-datatype prefix-exp prefix-exp?
  (const-exp
   (num integer?))
  (diff-exp
   (operand1 prefix-exp?)
   (operand2 prefix-exp?)))

;; parse-prefix-list : List -> Prefix-exp
(define parse-prefix-list
  (lambda (datum)
    (let* ((x (parse-prefix-list-aux datum))
           (result (car x))
           (rest (cdr x)))
      (if (null? rest) 
          result
          (eopl:error 'parse-prefix-list "invalid prefix list: ~s" datum)))))

;; parse-prefix-list-aux : List -> Prefix-exp * List
(define parse-prefix-list-aux
  (lambda (datum)
    (cond
      ((integer? (car datum)) (cons (const-exp (car datum)) (cdr datum)))
      ((eqv? '- (car datum))
       (let* ((left (parse-prefix-list-aux (cdr datum)))
              (right (parse-prefix-list-aux (cdr left))))
         (cons (diff-exp (car left) (car right))
               (cdr right))))
      (else (eopl:error 'parse-prefix-list-aux "invalid prefix list: ~s" datum)))))

(define sample-prefix-list '(- - 3 2 - 4 - 12 7))
(check-equal? (parse-prefix-list sample-prefix-list) (diff-exp
                                                      (diff-exp
                                                       (const-exp 3)
                                                       (const-exp 2))
                                                      (diff-exp
                                                       (const-exp 4)
                                                       (diff-exp
                                                        (const-exp 12)
                                                        (const-exp 7)))))