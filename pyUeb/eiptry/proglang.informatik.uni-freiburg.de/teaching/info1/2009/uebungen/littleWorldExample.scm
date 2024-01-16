;; Die ersten drei Zeilen dieser Datei wurden von DrScheme eingefügt. Sie enthalten Metadaten
;; über die Sprachebene dieser Datei in einer Form, die DrScheme verarbeiten kann.
#reader(lib "DMdA-advanced-reader.ss" "deinprogramm")((modname littleWorldExample) (read-case-sensitive #f) (teachpacks ((lib "world.ss" "teachpack" "deinprogramm"))) (deinprogramm-settings #(#f write repeating-decimal #f #t none datum #f ((lib "world.ss" "teachpack" "deinprogramm")))))
;; Beispiel zur Behandlung eines Mouse und Key-Events im Teach-Pack world.ss
;; Fabian Wenzelmann, 30.11.2009

;; Zeichnet ein Quadrat und eine Nachricht die sich abhängig von Events verändern können.

;; Record-Definition für ein Quadrat
;; ein Quadrat ist ein zusammengesetzer Datentyp bestehend aus 
;; natural: square-size Die Seitenlänge des Quadrats
;; image-color: square-color Der Farbe für das innere des Rechtecks

(define-record-procedures square make-square square?
  (square-size square-color))

(: make-square (natural image-color -> square))

;; Zeichnet ein square
(: square->image (square -> image))

(define square->image
  (lambda (s)
    (let*((size (square-size s))
          (color (square-color s)))
      (rectangle size size "solid" color))))

;; Record-Definition für eine Message
;; message ist ein zusammengesetzer Datentyp bestehend aus
;; string: message-text Der Inhalt der Nachricht
;; natural: message-font-size Der Größe der Nachricht
;; image-color: message-font-color Der Farbe in der die Nachricht dargestellt werden soll

(define-record-procedures message make-message message?
  (message-text message-font-size message-font-color))

(: make-message (string natural image-color -> message))

;; stellt eine Message als Bild dar

(: message->image (message -> image))
(define message->image
  (lambda (m)
    (let*((content (message-text m))
          (font-size (message-font-size m))
          (font-color (message-font-color m)))
      (text content font-size font-color))))


;; Record-Definition für die world
;; world ist ein zusammengesetzer Datentyp bestehend aus
;; square: world-square Das Quadrat welches dargestellt werden soll
;; message: world-message eine Nachricht die unter dem Quadrat angezeigt werden soll
;; int: world-count Einem Zähler der um 1 erhöht wird wenn die linke Maustaste gedrückt wird und um ein verringert wird, wenn die rechte Maustaste gedrückt wird

(define-record-procedures world make-world world?
  (world-square world-message world-count))

(: make-world (square message integer -> world))

;; stellt eine World als Bild dar

(: world->image (world -> image))

(define world->image
  (lambda (w)
    (let*((current-square (world-square w))
          (current-message (world-message w))
          (current-count (world-count w))
          (square-image (square->image current-square))
          (message-image (message->image current-message)))
      (above square-image message-image "left"))))

;; Breite des Fensters
(: window-width natural)
(define window-width 750)

;; Höhe des Fensters
(: window-height natural)
(define window-height 500)

;; behandelt Maus-Interaktionen
;; Verändert abhängig des Maus-Events den Zustand der World
;; Funktionen die Maus-Events bearbeiten haben immer den Vertrag (world natural natural mouse-event-kind -> world)
;; wobei die beiden natural Argumente die Position des Mauszeigers zum Zeitpunkt des Events repräsentieren (x- dann y-Komponente)
;; (wird hier nicht weiter behandelt einfach mal selbst ausprobieren)

;; Mögliche Interaktionen sind:
;; - Mauszeiger aus dem Fenster hinaus bewegen => Anzeige des Textes 'Maus nicht im Fenster aktiv'
;; - Mauszeiger in das Fenster hinein bewegen => Anzeige des Textes 'wb'
;; - Mauszeige links klicken => Count um 1 erhöhen und Anzeige des Textes 'Aktueller Zählerstand: #count'
;; - Mauszeiger rechts klicken => Count um 1 verringern "-----------------------------------------------"
;; weitere Typen von 'mouse-event-kind' gibt es in der Dokumentation (Hilfe -> Hilfezentrum -> Suche nach world.ss -> Eintrag der world.ss von 'deinprogramm' auswählen)

(: mouse-handler (world natural natural mouse-event-kind -> world))

(define mouse-handler
  (lambda (w x y kind)
    (cond
      ((string=? "leave" kind)
       (let* ((current-message (world-message w)))
         (make-world (world-square w)
                     (make-message "Maus nicht im Fenster aktiv"
                                   (message-font-size current-message)
                                   (message-font-color current-message))
                     (world-count w))))
      
      ((string=? "enter" kind)
       (let*((current-message (world-message w)))
         (make-world (world-square w) 
                     (make-message "wb"
                                   (message-font-size current-message)
                                   (message-font-color current-message))
                     (world-count w))))
    
    
    ((string=? "left-down" kind)
     (let*((current-message (world-message w))
           (new-count (+ 1 (world-count w))))
       (make-world (world-square w)
                   (make-message (string-append "Aktueller Zählerstand: " (number->string new-count))
                                 (message-font-size current-message)
                                 (message-font-color current-message))
                   new-count)))
    
    ((string=? "right-down" kind)
     (let*((current-message (world-message w))
           (new-count ( - (world-count w) 1)))
       (make-world (world-square w)
                   (make-message (string-append "Aktueller Zählerstand: " (number->string new-count))
                                 (message-font-size current-message)
                                 (message-font-color current-message))
                   new-count)))
    
    (else w))))

;; behandelt Tastatur-Interaktionen
;; Verändert abhängig des Maus-Events den Zustand der World

;; mögliche Interaktionen sind:
;; - Tastendruck auf "c" ändert die Farbe des Vierecks mit der ausgewählten Schriftfarbe
;; - Tastendruck auf "r" ändert die Farbe des Vierecks auf rot
;; - Tastendruck auf "p" ändert die Farbe des Vierecks auf violett

(: key-handler (world string -> world))

(define key-handler
  (lambda (w key)
    (cond
      ((string=? key "c")
       (let*((current-square (world-square w))
             (current-message (world-message w))
             (square-color-before-change (square-color current-square)))
         (make-world (make-square (square-size current-square)
                                  (message-font-color current-message))
                     (make-message (message-text current-message)
                                   (message-font-size current-message)
                                   square-color-before-change)
                     (world-count w))))
      
      ((string=? key "r")
       (let*((current-square (world-square w)))
         (make-world (make-square (square-size current-square)
                                  "red")
                     (world-message w)
                     (world-count w))))
      
      
      ((string=? key "p")
       (let*((current-square (world-square w)))
         (make-world (make-square (square-size current-square)
                                  "purple")
                     (world-message w)
                     (world-count w))))
      
      (else w))))

;; Quadrat zum Anfang der Animation
(: initial-square square)
(define initial-square (make-square 100 "pink"))

;; Message zu Beginn der Animation
(: initial-message message)
(define initial-message (make-message "dd"
                                      14
                                      "orange"))

;; Die initiale Welt
(: initial-world world)
(define initial-world (make-world initial-square
                                  initial-message
                                  0))
                                      
;; Starten der Animation
(big-bang window-width
          window-height
          0.01
          initial-world)

;; Zeichnen der Welt
(on-redraw world->image)

;; Registrieren des Maus-Events
(on-mouse-event mouse-handler)

;; Registrieren des Key-Events
(on-key-event key-handler)