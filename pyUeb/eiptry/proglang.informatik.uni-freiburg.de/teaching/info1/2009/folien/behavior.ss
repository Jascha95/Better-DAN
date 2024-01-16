;; Die ersten drei Zeilen dieser Datei wurden von DrScheme eingefügt. Sie enthalten Metadaten
;; über die Sprachebene dieser Datei in einer Form, die DrScheme verarbeiten kann.
#reader(lib "DMdA-advanced-reader.ss" "deinprogramm")((modname behavior) (read-case-sensitive #f) (teachpacks ((lib "world.ss" "teachpack" "deinprogramm"))) (deinprogramm-settings #(#f write repeating-decimal #f #t none datum #f ((lib "world.ss" "teachpack" "deinprogramm")))))
; contract for time
(define time real)

; optional values
(define maybe
  (lambda (x)
    (contract (mixed (one-of 'nothing) x))))
(define just (lambda (x) x))
(define just-value (lambda (x) x))
(define nothing (lambda () 'nothing))
(define nothing? (lambda (mx) (eq? 'nothing mx)))

; a result is a pair of a behavior and a value
(define-record-procedures-parametric result result-of
  make-result result?
  (result-behavior result-value))
; events
; a keyboard event consists of the key pressed
(define-record-procedures keyboard-event
  make-keyboard-event keyboard-event?
  (keyboard-event-key))
; a mouse event --- needs further specification
(define-record-procedures mouse-event
  make-mouse-event mouse-event?
  ())
; an event is either a keyboard event or a mouse event
(define event
  (contract (mixed keyboard-event mouse-event)))

; behaviors
(define behavior
  (lambda (x)
    (contract (time (maybe event) -> (result-of (maybe (behavior x)) x)))))

;;; implementation of different behaviors
; a time dependent behavior
(: timed ((time -> %x) -> (behavior %x)))
(define timed
  (lambda (f)
    (letrec
        ((beh (lambda (t me)
                (make-result (just beh) (f t)))))
      beh)))

; time itself
(: now (behavior time))
(define now
  (timed (lambda (t) t)))

; time independent behavior
(: const (%x -> (behavior %x)))
(define const
  (lambda (x)
    (timed (lambda (t) x))))

; transform a behavior (problem: ignores changes in (behavior %y))
(: then ((behavior %x) (%x -> (behavior %y)) -> (behavior %y)))
(define then
  (lambda (bx x->by)
    (lambda (t me)
      (let* ((rx (bx t me))
             (vx (result-value rx))
             (ry ((x->by vx) t me)))
        (make-result (then (result-behavior rx)
                           x->by)
                     (result-value ry))))))

; transform the time by a function
(: time-transform ((time -> time) (behavior %x)  -> (behavior %x)))
(define time-transform
  (lambda (trans beh-in)
    (lambda (t me)
      (let ((r (beh-in (trans t) me)))
        (make-result (time-transform trans (result-behavior r))
                     (result-value r))))))

; transform time by time behavior
(: time-transform-b ((behavior (time -> time)) (behavior %x) -> (behavior %x)))
(define time-transform-b
  (lambda (btt bx)
    (lambda (t me)
      (let* ((rtt (btt t me))
             (tt (result-value rtt))
             (rx (bx (tt t) me)))
        (make-result (time-transform-b (result-behavior rtt)
                                       (result-behavior rx))
                     (result-value rx))))))

; sample extracts the first value of a behavior and "freezes" it
(: sample ((behavior %x) -> (behavior %x)))
(define sample
  (lambda (b)
    (lambda (t me)
      (let ((r (b t me)))
        (make-result (const (result-value r))
                     (result-value r))))))

; timeout performs one behavior for a given time and then switches to another
(: timeout (time (behavior %x) (behavior %x) -> (behavior %x)))
(define timeout
  (lambda (t-out b1 b2)
    (if-b ((lift2 (lambda (t0 t) (< (- t t0) t-out))) (sample now) now)
          b1
          b2)))

; conditional behavior
(: if-b ((behavior boolean) (behavior %x) (behavior %x) -> (behavior %x)))
(define if-b
  (lambda (bb btrue bfalse)
    (lambda (t me)
      (let ((rb (bb t me)))
        (if (result-value rb)
            (let ((rbtrue (btrue t me))
                  (rbfalse (bfalse t (nothing))))
              (make-result (if-b (result-behavior rb)
                                 (result-behavior rbtrue)
                                 (result-behavior rbfalse))
                           (result-value rbtrue)))
            (let ((rbfalse (bfalse t me))
                  (rbtrue (btrue t (nothing))))
              (make-result (if-b (result-behavior rb)
                                 (result-behavior rbtrue)
                                 (result-behavior rbfalse))
                           (result-value rbfalse))))))))

; change behaviors on keyboard event
(: until-key (string (behavior %a) (%a -> (behavior %a)) -> (behavior %a)))
(define until-key
  (lambda (key beh f-beh)
    (lambda (t me)
      (let ((res (beh t me)))
        (if (and (keyboard-event? me)
                 (string=? key (keyboard-event-key me)))
            (make-result (f-beh (result-value res))
                         (result-value res))
            (make-result (until-key key (result-behavior res) f-beh)
                         (result-value res)))))))

; dispatch different behaviors on keyboard event
(: until-keyboard-event ((behavior %a) (keyboard-event -> (behavior %a)) -> (behavior %a)))
(define until-keyboard-event
  (lambda (beh ev->beh)
    (lambda (t me)
      (let ((res (beh t me)))
        (if (keyboard-event? me)
            ((ev->beh me) t (nothing))
            (make-result (until-keyboard-event (result-behavior res) ev->beh)
                         (result-value res)))))))

; lifting behaviors
(: lift1 ((%a -> %e) -> ((behavior %a) -> (behavior %e))))
(define lift1
  (lambda (op)
    (letrec ((wrap
              (lambda (b1)
                (lambda (t me)
                  (let ((r1 (b1 t me)))
                    (make-result (wrap (result-behavior r1))
                                 (op (result-value r1))))))))
      wrap)))

; lifting behaviors
(: lift2 ((%a %b -> %e) 
          -> ((behavior %a) (behavior %b) 
                            -> (behavior %e))))
(define lift2
  (lambda (op)
    (letrec ((wrap
              (lambda (b1 b2)
                (lambda (t me)
                  (let ((r1 (b1 t me))
                        (r2 (b2 t me)))
                    (make-result (wrap (result-behavior r1)
                                       (result-behavior r2))
                                 (op (result-value r1)
                                     (result-value r2))))))))
      wrap)))

; lifting behaviors
(: lift3 ((%a %b %c -> %e) 
          -> ((behavior %a) (behavior %b) (behavior %c) 
                            -> (behavior %e))))
(define lift3
  (lambda (op)
    (letrec ((wrap
              (lambda (b1 b2 b3)
                (lambda (t me)
                  (let ((r1 (b1 t me))
                        (r2 (b2 t me))
                        (r3 (b3 t me)))
                    (make-result (wrap (result-behavior r1)
                                       (result-behavior r2)
                                       (result-behavior r3))
                                 (op (result-value r1)
                                     (result-value r2)
                                     (result-value r3))))))))
      wrap)))

; lifting behaviors
(: lift4 ((%a %b %c %d -> %e) 
          -> ((behavior %a) (behavior %b) (behavior %c) (behavior %d) 
                            -> (behavior %e))))
(define lift4
  (lambda (op)
    (letrec ((wrap
              (lambda (b1 b2 b3 b4)
                (lambda (t me)
                  (let ((r1 (b1 t me))
                        (r2 (b2 t me))
                        (r3 (b3 t me))
                        (r4 (b4 t me)))
                    (make-result (wrap (result-behavior r1)
                                       (result-behavior r2)
                                       (result-behavior r3)
                                       (result-behavior r4))
                                 (op (result-value r1)
                                     (result-value r2)
                                     (result-value r3)
                                     (result-value r4))))))))
      wrap)))

; overlay behaviors
(: overlay-b ((behavior image) (behavior image) (behavior real) (behavior real) -> (behavior image)))
(define overlay-b 
  (lift4 overlay))

;;;; application

; Breite der Welt
(: world-width natural)
(define world-width 640)
; Höhe der Welt
(: world-height natural)
(define world-height 480)

(define random-color
  (lambda ()
    (let ((r (* 8 (random))))
      (cond
        ((< r 1) "white")
        ((< r 2) "red")
        ((< r 3) "green")
        ((< r 4) "blue")
        ((< r 5) "yellow")
        ((< r 6) "orange")
        ((< r 7) "cyan")
        ((< r 8) "magenta")))))

(define fw-startx (/ world-width 2))
(define fw-starty world-height)
(define fw-background
  (const (rectangle world-width world-height "solid" "black")))
(define fw 
  (lambda (background)
    (until-key "f"
               background
               (lambda (bg)
                 (let ((vx0 (- 15 (* 30 (random))))
                       (vy0 (+ 20 (* 80 (random))))
                       (rcl (+ 2 (* 3 (random))))
                       (clr (random-color))
                       (g 10))
                   (time-transform-b
                    ((lift1 (lambda (t0) (lambda (t) (- t t0)))) (sample now))
                    (overlay-b (fw background)
                               (const (circle rcl "solid" clr))
                               (timed (lambda (t) (max 0 (+ fw-startx (* vx0 t)))))
                               (timed (lambda (t) (max 0 (- fw-starty (- (* vy0 t) (* 1/2 g t t)))))))))))))

; the world's behavior
(define the-behavior-2
  (fw fw-background))

(define middle-x (/ (- world-width 20) 2))
(define middle-y (/ (- world-height 20) 2))

(define up-down
  (lambda(v0)
    (until-keyboard-event 
     (const v0)
     (lambda (kev)
       (cond
         ((string=? (keyboard-event-key kev) "up")
          (up-down (+ v0 1)))
         ((string=? (keyboard-event-key kev) "down")
          (up-down (- v0 1)))
         (else 
          (up-down v0)))))))

(define up-down-1
  (lambda (v0)
    (until-key "up" 
               (until-key "down" 
                          (const v0)
                          (lambda (v0) (up-down-1 (- v0 1))))
               (lambda (v0) (up-down-1 (+ v0 1))))))

(define circ-or-rect
  (lambda (r)
    (until-key "r" (const (circle r "solid" "blue"))
               (lambda (im)
                 (until-key "c" (const (rectangle r r "solid" "blue"))
                            (lambda (im)
                              (circ-or-rect r)))))))

(define sub-behavior
  (lambda (w h r)
    (let ((mx (- (/ w 2) r)) (my (- (/ h 2) r)))
      (overlay-b (const (ellipse w h "solid" "orange"))
                 (circ-or-rect r)
                 (timed (lambda (t) (+ mx (* mx (cos t)))))
                 (timed (lambda (t) (+ my (* my (sin t)))))))))

; the world's behavior
(define the-behavior-1
  (time-transform (lambda (t) (* 1 t))
                  (let ((mx (- (/ world-width 2) 50)) (my (- (/ world-height 2) 50)))
                    (overlay-b (const (rectangle world-width world-height "solid" "green"))
                               (time-transform (lambda (t) (* -3 t)) (sub-behavior 100 100 20))
                               (timed (lambda (t) (+ mx (* mx (cos t)))))
                               (timed (lambda (t) (+ my (* my (sin t)))))))))

; the world's behavior
(define the-behavior-3
  (let ((mx (/ world-width 2)))
    ((lift4 rectangle)
     (timed (lambda (t) (+ mx (* mx (cos t)))))
     (const world-height)
     (const "solid")
     (const "green"))))

; the world's behavior
(define the-flyer
  (timed 
   (lambda (t)
     (let ((tx (- t (* 4 (floor (/ t 4))))))
       (cond
         ((< tx 1)
          (rectangle 100 20 "solid" "gold"))
         ((< tx 2)
          (rectangle 100 20 "solid" "silver"))
         ((< tx 3)
          (rectangle 100 20 "solid" "green"))
         ((< tx 4)
          (ellipse 100 20 "solid" "yellow")))))))

(define from-to
  (lambda (v0 v1)
    ((lift2 (lambda (t0 t) 
              (let* ((dt (- t t0))
                     (tx (- dt (floor dt)))
                     (v (+ v0 (* (- v1 v0) tx)))) v)))
     (sample now) now)))

(define the-behavior
  (overlay-b (const (rectangle world-width world-height "solid" "black"))
             (time-transform (lambda (t) (* 5 t)) 
                             the-flyer)
             (time-transform (lambda (t) (* 0.33 t))
                             (from-to 0 (- world-width 100)))
             (const middle-y)))

; the generic world
(define-record-procedures world
  make-world world?
  (world-time world-image world-behavior))
(: make-world (real image (behavior image) -> world))

; drawing the world
(: world-draw (world -> image))
(define world-draw 
  (lambda (w)
    (world-image w)))

; stepping the clock
(define world-tick
  (lambda (w)
    (let* ((t (world-time w))
           (r ((world-behavior w) t (nothing))))
      (make-world (+ t world-increment)
                  (result-value r)
                  (result-behavior r)))))

; key event
(define world-key
  (lambda (w k)
    (let* ((t (world-time w))
           (r ((world-behavior w) t (just (make-keyboard-event k)))))
      (make-world t
                  (result-value r)
                  (result-behavior r)))))

; Increment der Welt
(: world-increment real)
(define world-increment 0.1)

; Anfangsmodell
(: initial-world world)
(define initial-world
  (make-world 0 
              (rectangle 1 1 "solid" "white")
              the-behavior))

; Urknall...
(big-bang world-width world-height world-increment initial-world)

; Anmelden der Callbacks
(on-redraw world-draw)
(on-tick-event world-tick)
(on-key-event world-key)

