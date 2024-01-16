; Aufgabe: Anneliese und ihre Freunde wollen möglichst viele Filme in einer Videonacht
; anschauen, ohne dass dabei Zeit verschenkt wird

; Gesamtdauer einer Liste von Filmen
(: movies-duration ((list movie) -> natural))
(define movies-duration
  (lambda (movies)
    (cond
      ((empty? movies)
       0)
      ((pair? movies)
       (+ (movie-length (first movies))
          (movies-duration (rest movies)))))))

; Vergleiche zwei Filmpläne, teste welcher Filmplan mehr Zeit in Anspruch nimmt
(: schedule-better ((list movie) (list movie) -> boolean))
(define schedule-better 
  (lambda (movies1 movies2)
    (>= (movies-duration movies1) (movies-duration movies2))))

; wähle Filme aus einer Liste so aus, dass ein vorgegebenes Zeitlimit
; möglichst gut ausgeschöpft, aber nicht überschritten wird
(: movie-schedule ((list movie) natural -> (list movie)))
(define movie-schedule
  (lambda (movies time-available)
    (cond
      ((empty? movies)
       empty)
      ((pair? movies)
       (let* ((movie (first movies))
              (duration (movie-length movie)))
         (if (<= duration time-available)
             ;movie-may-be-selected
             (let ((schedule-with-first 
                    (make-pair movie
                               (movie-schedule (rest movies) (- time-available duration))))
                   (schedule-without-first
                    (movie-schedule (rest movies) time-available)))
               (if (schedule-better schedule-with-first schedule-without-first)
                   schedule-with-first
                   schedule-without-first))
             (movie-schedule (rest movies) time-available)))))))