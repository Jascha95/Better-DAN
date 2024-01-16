;; Die ersten drei Zeilen dieser Datei wurden von DrScheme eingefügt. Sie enthalten Metadaten
;; über die Sprachebene dieser Datei in einer Form, die DrScheme verarbeiten kann.
#reader(lib "DMdA-assignments-reader.ss" "deinprogramm")((modname |20090129|) (read-case-sensitive #f) (teachpacks ((lib "world.ss" "teachpack" "deinprogramm"))) (deinprogramm-settings #(#f write repeating-decimal #t #t none explicit #f ((lib "world.ss" "teachpack" "deinprogramm")))))
(define-record-procedures-parametric-2 (ref X)
  make-ref ref?
  ((get-ref set-ref!)))

(define-contract account (real -> (mixed real (one-of #f))))

; Konto aus einem Geldbetrag erzteugen
;(: make-account (real -> account))
(define make-account
  (lambda (initial-balance)
    (let ((balance (make-ref initial-balance)))
      (lambda (message)
        (cond
          ((string=? message "balance")
           (lambda ()
             (get-ref balance)))
          ((string=? message "withdraw")
           (lambda (amount)
             (if (>= (get-ref balance) amount)
                 (begin
                   (set-ref! balance (- (get-ref balance) amount))
                   (get-ref balance))
                 #f))))))))

; verschicken einer Nachricht an ein Objekt
; send : object string value* -> value
(define send
  (lambda (obj message . args)
    (apply (obj message) args)))

; ausdrucken einer Liste von Strings
(: write-list-newline ((list string) -> unspecific))
(define write-list-newline
  (lambda (text)
    (begin
    (for-each write-string text)
    (write-newline))))


; Person konstruieren
; make-person : string -> (message -> method)
(define make-person
  (lambda (name) ;; nicht veranderliche Eigenschaft
    (let ((count (make-ref 0)))
      (letrec ((self 
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
                    ((string=? message "slap")
                     (lambda ()
                       (if (= (get-ref count) 2)
                           (begin
                             (send self "say" (list "Ouch!"))
                             (set-ref! count 0))
                           (begin 
                             (send self "say" (list "huh?"))
                             (set-ref! count (+ (get-ref count) 1))))))))))
        self))))


; Sanger konstruieren
; make-singer : string -> (message -> method)
(define make-singer
  (lambda (name)
    (let ((person (make-person name)))
      (letrec ((self
                (lambda (message)
                  (cond
                    ((string=? message "sing")
                     ;; Text singen
                     ;; list(string) -> unspecified
                     (lambda (song)
                       (send self "say" (make-pair "tra-la-la " song))))
                    (else (person message))))))
    self))))

; Rockstar erzeugen
; make-rockstar : string -> (message -> method)
(define make-rockstar
  (lambda (name)
    (let ((singer (make-singer name)))
      (letrec ((self
                (lambda (message)
                  (cond
                    ((string=? message "say")
                     ;; Text sprechen
                     ;; list(string) -> unspecified
                     (lambda (text)
                       (send singer "say" (append text (list ", dude")))))
                    ((string=? message "slap")
                     ;; Schlag einstecken
                     ;; -> unspecified
                     (lambda ()
                       (send self "say" (list "pain just makes me stronger"))))
                    (else (singer message))))))
        self))))