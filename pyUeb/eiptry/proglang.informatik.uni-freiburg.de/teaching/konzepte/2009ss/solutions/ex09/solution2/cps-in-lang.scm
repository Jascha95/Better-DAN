(module cps-in-lang (lib "eopl.ss" "eopl")                
  
  ;; input language for the CPS converter.

  (require "drscheme-init.scm")
  
  (provide (all-defined))

  ;;;;;;;;;;;;;;;; grammatical specification ;;;;;;;;;;;;;;;;
  
  (define the-lexical-spec
    '((whitespace (whitespace) skip)
      (comment ("%" (arbno (not #\newline))) skip)
      (identifier
       (letter (arbno (or letter digit "_" "-" "?")))
       symbol)
      (number (digit (arbno digit)) number)
      (number ("-" digit (arbno digit)) number)
      ))
  
  (define the-grammar
    '((program (expression) a-program)

      (expression (number) const-exp)

      (expression
        ("-" "(" expression "," expression ")")
        diff-exp)
      
      (expression
        ("+" "(" (separated-list expression ",") ")")
        sum-exp)

      (expression
       ("zero?" "(" expression ")")
       zero?-exp)

      (expression
       ("if" expression "then" expression "else" expression)
       if-exp)

      (expression
        ("letrec" 
          (arbno identifier "(" (arbno identifier) ")"
            "=" expression)
          "in"
          expression)
        letrec-exp)

      (expression (identifier) var-exp)

      (expression
       ("let" identifier "=" expression "in" expression)
       let-exp)   

      (expression
       ("proc" "(" (arbno identifier) ")" expression)
       proc-exp)

      (expression
       ("(" expression (arbno expression) ")")
       call-exp)
      
      ))

  ;;;;;;;;;;;;;;;; sllgen boilerplate ;;;;;;;;;;;;;;;;
  
  (sllgen:make-define-datatypes the-lexical-spec the-grammar)
  
  (define show-the-datatypes
    (lambda () (sllgen:list-define-datatypes the-lexical-spec the-grammar)))
  
  (define scan&parse
    (sllgen:make-string-parser the-lexical-spec the-grammar))
  
  (define just-scan
    (sllgen:make-string-scanner the-lexical-spec the-grammar))
  
  ;; Solution
  (define tail-form?
    (lambda (exp)
      (cases expression exp
        (if-exp (e1 e2 e3) (and (simple? e1) (tail-form? e2) (tail-form? e3)))
        (letrec-exp (fs xss es body)
                    (and (forall tail-form? es) (tail-form? body)))
        (let-exp (x e1 e2)
                 (and (simple? e1) (tail-form? e2)))
        (call-exp (e es)
                  (and (simple? e) (forall simple? es)))
        (else simple? exp))))
  
  (define simple?
    (lambda (exp)
      (cases expression exp  
        (const-exp (n) #t)
        (diff-exp (e1 e2) (and (simple? e1) (simple? e2)))
        (sum-exp (es) (forall simple? es))
        (zero?-exp (e) (simple? e))
        (var-exp (x) #t)
        (proc-exp (xs body) (tail-form? body))
        (else #f))))
  
  (define forall
    (lambda (f l)
      (if (null? l)
          #t
          (and (f (car l)) (forall f (cdr l))))))
 
  )
