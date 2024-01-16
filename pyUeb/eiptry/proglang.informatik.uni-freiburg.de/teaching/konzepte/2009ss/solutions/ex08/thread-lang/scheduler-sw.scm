(module scheduler-sw (lib "eopl.ss" "eopl")
  
  (require "drscheme-init.scm")
  (require "queues.scm")
  (require "data-structures-sw.scm")       ; for continuation?
  (require "lang.scm")                  ; for expval?
  
  (provide
    initialize-scheduler!
    set-final-answer! 
    
    time-expired?
    decrement-timer!

    place-on-ready-queue!
    run-next-thread

    ;; SW
    next-thread-id!
    current-thread-id
    make-thread
    run-thread
    )
  
  ;;;;;;;;;;;;;;;; the state ;;;;;;;;;;;;;;;;
  
  ;; components of the scheduler state:
  
  (define the-ready-queue   'uninitialized)         
  (define the-final-answer  'uninitialized)
  
  (define the-max-time-slice    'uninitialized)
  (define the-time-remaining    'uninitialized)

  ;; SW:
  (define next-thread-id 0)
  (define cur-thread-id 'undefined)
  
  ;; initialize-scheduler! : Int -> Unspecified
  (define initialize-scheduler!
    (lambda (ticks)
      (set! the-ready-queue (empty-queue))
      (set! the-final-answer 'uninitialized)
      (set! the-max-time-slice ticks)
      (set! the-time-remaining the-max-time-slice)
      ;; SW
      (set! next-thread-id 0)
      (set! cur-thread-id (next-thread-id!))
      ))
  
  ;;;;;;;;;;;;;;;; the final answer ;;;;;;;;;;;;;;;;

  ;; place-on-ready-queue! : Thread -> Unspecified
  ;; Page: 184  
  (define place-on-ready-queue!
    (lambda (th)
      (set! the-ready-queue
        (enqueue the-ready-queue th))))

  ;; run-next-thread : () -> FinalAnswer
  ;; Page: 184    
  (define run-next-thread
    (lambda ()
      (if (empty? the-ready-queue)
        the-final-answer
        (dequeue the-ready-queue
          (lambda (first-ready-thread other-ready-threads)
            (set! the-ready-queue other-ready-threads)            
            (set! the-time-remaining the-max-time-slice)
            ;; SW
            (run-thread first-ready-thread))))))

  ;; set-final-answer! : ExpVal -> Unspecified
  ;; Page: 184    
  (define set-final-answer!
    (lambda (val)
      (set! the-final-answer val)))

  ;; time-expired? : () -> Bool
  ;; Page: 184    
  (define time-expired?
    (lambda ()
      (zero? the-time-remaining)))

  ;; decrement-timer! : () -> Unspecified
  ;; Page: 184    
  (define decrement-timer!
    (lambda ()
      (set! the-time-remaining (- the-time-remaining 1))))

  
  ;; SW
    
  (define next-thread-id!
    (lambda ()
      (let ((res next-thread-id))
        (begin
          (set! next-thread-id (+ 1 res))
          res))))

  
  (define current-thread-id
    (lambda () cur-thread-id))
  
  (define make-thread
    (lambda (proc)
      (a-thread (current-thread-id) proc)))
  
  (define run-thread
    (lambda (th)
      (cases thread th
        (a-thread (tid proc)
          (begin
            (set! cur-thread-id tid)
            (proc))))))
                  

  )
