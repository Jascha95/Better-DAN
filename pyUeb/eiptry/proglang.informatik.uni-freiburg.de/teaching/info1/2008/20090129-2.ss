;; Die ersten drei Zeilen dieser Datei wurden von DrScheme eingefügt. Sie enthalten Metadaten
;; über die Sprachebene dieser Datei in einer Form, die DrScheme verarbeiten kann.
#reader(lib "DMdA-assignments-reader.ss" "deinprogramm")((modname 20090129-2) (read-case-sensitive #f) (teachpacks ((lib "world.ss" "teachpack" "deinprogramm"))) (deinprogramm-settings #(#f write repeating-decimal #t #t none explicit #f ((lib "world.ss" "teachpack" "deinprogramm")))))
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
                     (set-ref! slaps 0)))))))))))

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