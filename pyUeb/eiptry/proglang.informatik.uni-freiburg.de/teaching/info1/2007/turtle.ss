(module turtle mzscheme
  
  ; we need the image functions from the image.ss teachpack
  ; math.ss is used, because we need sin and cos
  (require (only (lib "image" "deinprogramm") rectangle line overlay)
           (lib "math.ss") 
           )

  ; used to convert angles 
  (define pi/180 (/ pi 180))

  ; convert angle value
  ; grad->rad: number -> number
  (define grad->rad
    (lambda (grad)
      (* pi/180 grad)))

  ; This function is only for internal use.
  ; (new-turtle-priv h w x y angle img color state)
  ; creates a new turtle with hight h, width w.
  ; The cursor is at position (x,y) and the view direction
  ; is defined by an angle value relative to the vector (1,0) .
  ; The two next componets represents the image and the
  ; color of the pen. The last component represents an abritary
  ; value, that allows to transport state with the turtle.
  ; new-turtle-priv: number number number number number image color A -> turtle
  (define new-turtle-priv 
    (lambda (h w x y angle img color state)
      (vector h w x y angle img color state)))

  
  ; (new-turtle h w color)
  ; creates a new turtle with the pen color color and sets the
  ; width of the image to w and the hight to h.
  ; The background of the image is gray and the position of the
  ; cursor is (0,0) and the view direction is (1,0).
  ; new-turtle : number number color -> turtle
  (define new-turtle 
    (lambda (h w color)
      (let ((x (floor (/ w 2)))
            (y (floor (/ h 2))))
        (new-turtle-priv h w x y 0 (rectangle w h "solid" "gray") color #f))))

  ; (new-turtle-complex h w color bgcolor x y angle)
  ; creates a new turtle with the pen color color and sets the
  ; width of the image to w and the hight to h.
  ; The background of the image is bgcolor and the position of the
  ; cursor is (x,y) and the view direction is (1,0) * e^(- i angle).
  ; new-turtle : number number color color number number number -> turtle
  (define new-turtle-complex 
    (lambda (h w color bgcolor x y angle)
      (new-turtle-priv h w x y angle (rectangle w h "solid" bgcolor) color #f)))

  
  ; For internal use only
  ; get-h: turtle -> number
  (define get-h (lambda (t) (vector-ref t 0)))
  ; get-w: turtle -> number
  (define get-w (lambda (t) (vector-ref t 1)))
  ; get-x: turtle -> number
  (define get-x (lambda (t) (vector-ref t 2)))
  ; get-y: turtle -> number
  (define get-y (lambda (t) (vector-ref t 3)))
  ; get-angle: turtle -> number
  (define get-angle (lambda (t) (vector-ref t 4)))
  ; get-iamge: turtle -> image
  (define get-image (lambda (t) (vector-ref t 5)))
  ; get-color: turtle -> color
  (define get-color (lambda (t) (vector-ref t 6)))
  ; get-state: turtle -> A
  (define get-state (lambda (t) (vector-ref t 7)))

  ; (set-color color)
  ; returns a function of type turtle -> turtle.
  ; Use the result to change the color of the pen.
  ; set-color: color -> (turtle -> turtle)
  (define set-color
    (lambda (color)
      (lambda (t)
        (let* ((h (get-h t))
               (w (get-w t))
               (x (get-x t))
               (y (get-y t))
               (angle (get-angle t))
               (image (get-image t)))
          (new-turtle-priv h w x y angle image color #f)))))
  
  ; (turn angle) 
  ; returns a function of type turtle -> turtle.
  ; Use the result to turn the view of the turtle (counter-clockwise).
  ; turn: number -> (turtle -> turtle)
  (define turn
    (lambda (grad)
      (lambda (t)
              (let* ((h (get-h t))
                     (w (get-w t))
                     (x (get-x t))
                     (y (get-y t))
                     (angle (get-angle t))
                     (image (get-image t))
                     (color (get-color t))
                     (state (get-state t)))
                (new-turtle-priv h w x y (- angle grad) image color state)))))
      
  ; For internal use only
  ; (move-cursor turtle length)
  ; returns a new turtle where the cursor 
  ; is moved length steps along the view vector.
  ; move-cursor: turtle number -> turtle
  (define move-cursor
    (lambda (t length)
      (let* ((h (get-h t))
             (w (get-w t))
             (x (get-x t))
             (y (get-y t))
             (angle (get-angle t))
             (image (get-image t))
             (color (get-color t))
             (state (get-state t))
             (newx (+ x (* length (cos (grad->rad angle)))))
             (newy (+ y (* length (sin (grad->rad angle))))))
        (new-turtle-priv h w newx newy angle image color state))))
      
  ; (draw length)
  ; returns a function of type turtle -> turtle.
  ; The result can be used to move the turtle and draw a line.
  ; draw: number -> (turtle -> turtle)
  (define draw
    (lambda (length)
      (lambda (t)
        (let* ((h (get-h t))
              (w (get-w t))
              (x (get-x t))
              (y (get-y t))
              (angle (get-angle t))
              (image (get-image t))
              (color (get-color t))
              (state (get-state t))
              ; Compute new coordinats
              (newx (+ x (* length (cos (grad->rad angle)))))
              (newy (+ y (* length (sin (grad->rad angle))))))
          (new-turtle-priv
           h w 
           newx newy angle 
           ; Compute new image
           (overlay image
                    (line w h x y newx newy color) 0 0)
           color state)))))

  ; (move length)
  ; returns a function of type turtle -> turtle.
  ; The result can be used to move the turtle without drawing a line.
  ; move : number -> (turtle -> turtle)
  (define move
    (lambda (length)
      (lambda (t)
        (move-cursor t length)))) 
  
  ; runs a turtle function 
  ; run: (turtle -> turtle) number number color -> image
  (define run 
    (lambda (t->t h w color) 
      (get-image (t->t (new-turtle h w color)))))

;;   ; runs a turtle function 
;;   ; run*: (turtle -> turtle) -> turtle -> image
;;   (define run*
;;     (lambda (t->t h w color bgcolor x y angle) 
;;       (get-image (t->t (new-turtle h w color bgcolor x y angle)))))

  ; This function is only for internal use.
  (define comp_priv_2 
    (lambda (f1 f2)
      (lambda (t)
        (f2 (f1 t)))))
  
  ; This function is only for internal use.
  (define comp_priv
    (lambda (l)
      (cond 
        ((null? l) (error "do erwartet mind. ein Argument"))
        ((list? l) 
         (let ((head (car l))
               (tail (cdr l)))
           (if (null? tail)
               head 
               (comp_priv_2 head (comp_priv tail))))))))

  ; This function allows to do a list of
  ; turtle -> turtle 
  ; functions into one new function, that do
  ; one action of the turtle, then later the rest.
  ; Define the type alias tip = turtle -> turtle.
  ; do: tip ... tip -> tip 
  (define sequence (lambda l (comp_priv l)))


  ; export this functions 
  (provide set-color)
  (provide turn)
  (provide draw)
  (provide move)
  (provide run)
;;  (provide run*)
  (provide sequence)
  
  )
