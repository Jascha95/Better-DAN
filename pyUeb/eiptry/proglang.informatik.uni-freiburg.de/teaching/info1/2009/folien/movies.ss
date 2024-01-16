;; Die ersten drei Zeilen dieser Datei wurden von DrScheme eingefügt. Sie enthalten Metadaten
;; über die Sprachebene dieser Datei in einer Form, die DrScheme verarbeiten kann.
#reader(lib "DMdA-vanilla-reader.ss" "deinprogramm")((modname movies) (read-case-sensitive #f) (teachpacks ()) (deinprogramm-settings #(#f write repeating-decimal #t #t none explicit #f ())))
; Ein movie ist ein Wert
;  (make-movie title length)
; wobei title : string der Titel des Films ist und
;       length : natural die Länge des Films in Minuten ist
(define-record-procedures movie
  make-movie movie?
  (movie-title movie-length))

; Filme, die zur Wahl stehen
(: movies (list movie))
(define movies
  (list
   (make-movie "Hercules in New York" 91)
   (make-movie "The Long Goodby" 112)
   (make-movie "Stay Hungry" 102)
   (make-movie "The Villain" 89)
   (make-movie "Scavenger Hunt" 116)
   (make-movie "Conan the Barbarian" 129)
   (make-movie "Conan the Destroyer" 103)
   (make-movie "The Terminator" 108)
   (make-movie "Red Sonja" 89)
   (make-movie "Commando" 90)
   (make-movie "Raw Deal" 97)
   (make-movie "Predator" 107)
   (make-movie "The Running Man" 101)
   (make-movie "Red Heat" 104)
   (make-movie "Twins" 105)
   (make-movie "Total Recall" 113)
   (make-movie "Kindergarten Cop" 111)
   (make-movie "Terminator 2: Judgment Day" 137)
   (make-movie "Last Action Hero" 130)
   (make-movie "True Lies" 141)))

