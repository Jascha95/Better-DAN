(module store-sw (lib "eopl.ss" "eopl")
  
  (require "drscheme-init.scm")
   
  (provide empty-store reference? newref deref setref ;; SW remove initialize-store!, added empty-store
    instrument-newref get-store-as-list
    make-answer get-value get-store ;; SW: added
    )
  
  (define instrument-newref (make-parameter #f))
  
  ;; SW: the type AnswerOf(X) represents some value of type X paired together with a store
  
  ;; make-answer: X * Store -> AnswerOf(X)
  (define make-answer cons)
  
  ;; get-value: AnswerOf(X) -> X
  (define get-value car)
  
  ;; get-store: AnswerOf(X) -> Sto
  (define get-store cdr)
  
  ;;;;;;;;;;;;;;;; references and the store ;;;;;;;;;;;;;;;;
  
  ;;; world's dumbest model of the store:  the store is a list and a
  ;;; reference is number which denotes a position in the list.

  ;; the-store: a Scheme variable containing the current state of the
  ;; store.  Initially set to a dummy variable.
  ;; SW: no longer needed (define the-store 'uninitialized)

  ;; empty-store : () -> Sto
  ;; Page: 111
  (define empty-store
    (lambda () '()))
  
  ;; initialize-store! : () -> Sto
  ;; usage: (initialize-store!) sets the-store to the empty-store
  ;; Page 111
  ;; SW: deleted

  ;; get-store : () -> Sto
  ;; Page: 111
  ;; This is obsolete.  Replaced by get-store-as-list below
  ;; SW: deleted

  ;; reference? : SchemeVal -> Bool
  ;; Page: 111
  (define reference?
    (lambda (v)
      (integer? v)))

  ;; newref : Sto * ExpVal -> AnswerOf(Ref)
  ;; Page: 111
  ;; SW: added parameter the-store, added return value
  (define newref
    (lambda (the-store val)
      (let ((next-ref (length the-store))
            (new-store (append the-store (list val))))
        (if (instrument-newref)
            (eopl:printf 
             "newref: allocating location ~s with initial contents ~s~%"
             next-ref val))                     
        (make-answer next-ref new-store)))) 

  ;; deref : Sto * Ref -> ExpVal
  ;; Page 111
  ;; SW: added parameter the-store
  (define deref 
    (lambda (the-store ref)
      (list-ref the-store ref)))

  ;; setref : Sto * Ref * ExpVal -> Sto
  ;; Page: 112
  ;; SW: added parameter the-store and changed result value
  (define setref
    (lambda (the-store ref val)
        (letrec
          ((setref-inner
             ;; returns a list like store1, except that position ref1
             ;; contains val. 
             (lambda (store1 ref1)
               (cond
                 ((null? store1)
                  (report-invalid-reference ref the-store))
                 ((zero? ref1)
                  (cons val (cdr store1)))
                 (else
                   (cons
                     (car store1)
                     (setref-inner
                       (cdr store1) (- ref1 1))))))))
          (setref-inner the-store ref))))

  (define report-invalid-reference
    (lambda (ref the-store)
      (eopl:error 'setref
        "illegal reference ~s in store ~s"
        ref the-store)))

  ;; get-store-as-list : () -> Listof(List(Ref,Expval))
  ;; Exports the current state of the store as a scheme list.
  ;; (get-store-as-list '(foo bar baz)) = ((0 foo)(1 bar) (2 baz))
  ;;   where foo, bar, and baz are expvals.
  ;; If the store were represented in a different way, this would be
  ;; replaced by something cleverer.
  ;; Replaces get-store (p. 111)
    ;; SW: added the-store parameter
   (define get-store-as-list
     (lambda (the-store)
       (letrec
         ((inner-loop
            ;; convert sto to list as if its car was location n
            (lambda (sto n)
              (if (null? sto)
                '()
                (cons
                  (list n (car sto))
                  (inner-loop (cdr sto) (+ n 1)))))))
         (inner-loop the-store 0))))

  )