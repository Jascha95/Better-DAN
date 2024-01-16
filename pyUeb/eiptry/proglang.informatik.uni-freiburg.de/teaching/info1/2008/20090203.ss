;; Die ersten drei Zeilen dieser Datei wurden von DrScheme eingefügt. Sie enthalten Metadaten
;; über die Sprachebene dieser Datei in einer Form, die DrScheme verarbeiten kann.
#reader(lib "DMdA-assignments-reader.ss" "deinprogramm")((modname |20090203|) (read-case-sensitive #f) (teachpacks ((lib "world.ss" "teachpack" "deinprogramm"))) (deinprogramm-settings #(#f write repeating-decimal #t #t none explicit #f ((lib "world.ss" "teachpack" "deinprogramm")))))
(define-record-procedures-parametric-2 (ref X)
  make-ref ref?
  ((get-ref set-ref!)))


; ausdrucken einer Liste von Strings
(: write-list-newline ((list string) -> unspecific))
(define write-list-newline
  (lambda (text)
    (begin
    (for-each write-string text)
    (write-newline))))


(define make-person
  (lambda (name)
    (let ((slaps (make-ref 0)))
      (lambda (message)
        (cond
          ((string=? message "get-name")
           ;; person -> string
           (lambda (self)
             name))
          ((string=? message "say")
           ;; person list(string) -> unspecified
           (lambda (self text)
             (write-list-newline text)))
          ((string=? message "slap")
           ;; person -> unspecified
           (lambda (self)
             (begin
               (set-ref! slaps (+ (get-ref slaps) 1))
               (if (< (get-ref slaps) 3)
                   (send self "say" (list "huh?"))
                   (begin
                     (send self "say" (list "ouch!"))
                     (set-ref! slaps 0))))))
          (else #f))))))

(define send
  (lambda (obj meth . args)
    (apply (obj meth) obj args)))


(define make-singer
  (lambda (name)
    (let ((person (make-person name)))
      (lambda (message)
        (cond
          ((string=? message "sing")
           ;; Text singen
           ;; singer list(string) -> unspecified
           (lambda (self text)
             (send self "say" (make-pair "tra-la-la " text))))
          (else (person message)))))))


(define make-rockstar
  (lambda (name)
    (let ((singer (make-singer name)))
      (lambda (message)
        (cond
          ((string=? message "say")
           ;; Text sagen
           ;; rockstar list(string) -> unspecified
           (lambda (self text)
             (send singer "say" (append text (list ", dude")))))
          ((string=? message "slap")
           ;; Schlag einstecken
           ;; rockstar -> unspecified
           (lambda (self)
             (send self "say" (list "pain just makes me stronger"))))
          (else (singer message)))))))


; Dichter konstruieren
; make-poet : string -> (message -> method)
(define make-poet
  (lambda (name)
    (lambda (message)
      (cond
        ((string=? message "say")
         ;; poet list(string) -> unspecified
         (lambda (self text)
           (write-list-newline (append text (list " and the sky is blue")))))
        ((string=? message "recite")
         ;; poet -> unspecified
         (lambda (self)
           (write-list-newline (list "the sky is blue"))))
        (else
         #f)))))

; james, der rockstar-poet
; james : message -> method
(define james
  (let* ((name "James")
         (rockstar (make-rockstar name))
         (poet (make-poet name)))
    (lambda (message)
      (let ((method (rockstar message)))
        (if (equal? method #f)
            (poet message)
            method)))))

; james, der poeten-rockstar
; james : message -> method
(define james-2
  (let* ((name "James")
         (rockstar (make-rockstar name))
         (poet (make-poet name)))
    (lambda (message)
      (let ((method (poet message)))
        (if (equal? method #f)
            (rockstar message)
            method)))))


(define make-make-cool-someone
  (lambda (make-super)
    (lambda (name)
      (let ((super (make-super name)))
        (lambda (message)
          (cond
            ((string=? message "say")
             ;; Text sagen
             ;; rockstar list(string) -> unspecified
             (lambda (self text)
               (send super "say" (append text (list ", dude")))))
            ((string=? message "slap")
             ;; Schlag einstecken
             ;; rockstar -> unspecified
             (lambda (self)
               (send self "say" (list "pain just makes me stronger"))))
            (else (super message))))))))


