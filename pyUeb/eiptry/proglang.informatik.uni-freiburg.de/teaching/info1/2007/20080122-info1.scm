;; Die ersten drei Zeilen dieser Datei wurden von DrScheme eingefügt. Sie enthalten Metadaten
;; über die Sprachebene dieser Datei in einer Form, die DrScheme verarbeiten kann.
#reader(lib "DMdA-assignments-reader.ss" "deinprogramm")((modname 20080122-info1) (read-case-sensitive #f) (teachpacks ()) (deinprogramm-settings #9(#f write repeating-decimal #t #t none explicit #f ())))
; Konto aus Geldbetrag erzeugen
; make-account : number -> account
; make-account : number -> (number -> (number or #f))
(define make-account-1
  (lambda (balance)
    ;; das account Objekt ist die Abhebeprozedur!
    (lambda (amount)
      (if (<= amount balance)
          (begin
            (set! balance (- balance amount))
            balance)
          #f))))

; Konto aus Geldbetrag erzeugen
; make-account : number -> (message -> method)
(define make-account
  (lambda (balance)
    (lambda (message)
      (cond
        ((string=? message "balance")
         ;; Kontostand
         ;; -> number
         (lambda ()
           balance))
        ((string=? message "withdraw")
         ;; Abheben
         ;; number -> (number or #f)
         (lambda (amount)
           (if (<= amount balance)
               (begin
                 (set! balance (- balance amount))
                 balance)
               #f)))))))

; Sende eie BNachricht an ein Objekt
; send : object message value* -> value
(define send
  (lambda (obj message . args)
    (apply (obj message) obj args)))


; Drucke Liste von Strings
; write-list-newline : list(string) -> unspecified
(define write-list-newline
  (lambda (text)
    (begin
      (for-each write-string text)
      (write-newline))))

; Person konstruieren
; make-person : string -> (message -> method)
(define make-person-1
  (lambda (name)
    (lambda (message)
      (cond
        ((string=? message "get-name")
         ;; Namen liefern
         ;; -> string
         (lambda ()
           name))
        ((string=? message "say")
         ;; Text ausdrucken
         ;; list(string) -> unspecified
         (lambda (text)
           (write-list-newline text)))
        ))))

(define make-person-2
  (lambda (name)
    (let ((slaps 0)) ; Anzahl der Schlage
      (letrec ((self
                (lambda (message)
                  (cond
                    ((string=? message "get-name")
                     (lambda () name))
                    ((string=? message "say") 
                     (lambda(text)
                       (write-list-newline text)))
                    ((string=? message "slap")
                     (lambda ()
                       (begin
                         (set! slaps (+ slaps 1))
                         (if (< slaps 3)
                             (send self "say" (list "huh?"))
                             (begin
                               (set! slaps 0)
                               (send self "say" (list "ouch!")))
                             ))))))))
        self))))

(define make-person
  (lambda (name)
    (let ((slaps 0)) ; Anzahl der Schlage
      (lambda (message)
        (cond
          ((string=? message "get-name")
           (lambda (self) name))
          ((string=? message "say") 
           (lambda(self text)
             (write-list-newline text)))
          ((string=? message "slap")
           (lambda (self)
             (begin
               (set! slaps (+ slaps 1))
               (if (< slaps 3)
                   (send self "say" (list "huh?"))
                   (begin
                     (set! slaps 0)
                     (send self "say" (list "ouch!")))
                   ))))
          (else #f))))))

; Sänger
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

; Rockstar
(define make-rockstar
  (lambda (name)
    (let ((singer (make-singer name)))
      (lambda (message)
        (begin;(write-string "rockstar: ")(write-string message)
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
          (else (singer message))))))))


; Dichter konstruieren
; make-poet : string -> (message -> method)
(define make-poet
  (lambda (name)
    (lambda (message)
      (begin;(write-string"poet")(write-string message)
      (cond
       ((string=? message "say")
        ;; poet list(string) -> unspecified
        (lambda (self text)
          (write-list-newline (append text (list " and the sky is blue")))))
       ((string=? message "recite")
        ;; poet -> unspecified
        (lambda (self)
          (write-list-newline (list "the sky is blue"))))
       (else #f))))))

; message -> method
(define james 
  (let* ((name "James")
         (rockstar (make-rockstar name))
         (poet (make-poet name)))
    (lambda (message)
      (begin;(write-string "James:")(write-string message)
      (let ((rockstar-method (rockstar message)))
        (if (boolean? rockstar-method)
            (poet message)
          rockstar-method))))))

; henry, der poetische rockstar
; henry : message -> method
(define henry
  (let* ((name "Henry")
         (rockstar (make-rockstar name))
         (poet (make-poet name)))
    (lambda (message)
      (let ((poet-method (poet message)))
        (if (equal? poet-method #f)
            (rockstar message)
            poet-method)))))

; make someone cool
; make-make-cool-someone : 
;  (string -> (message -> method)) -> (string -> (message -> method))
(define make-make-cool-someone
  (lambda (make-someone)
    (lambda (name)
      (let ((super (make-someone name)))
        (lambda (message)
          (begin
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
          (else (super message)))))))))


; make-cool-poet : string -> (message -> method)
(define make-cool-poet 
  (make-make-cool-someone make-poet))
