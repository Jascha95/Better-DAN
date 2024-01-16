;; Die ersten drei Zeilen dieser Datei wurden von DrScheme eingefügt. Sie enthalten Metadaten
;; über die Sprachebene dieser Datei in einer Form, die DrScheme verarbeiten kann.
#reader(lib "DMdA-assignments-reader.ss" "deinprogramm")((modname 20080117-info1) (read-case-sensitive #f) (teachpacks ()) (deinprogramm-settings #9(#f write repeating-decimal #t #t none explicit #f ())))
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
    (apply (obj message) args)))


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
           (write-list-newline text)))))))

(define make-person
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
