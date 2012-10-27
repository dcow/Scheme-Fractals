;;; Procedure:
;;;   image-compute-line!
;;; Parameters:
;;;   image; an image
;;;   fun*; a procedure that takes an rgb value as an input and returns an rgb value 
;;;        as output
;;;   x1; int, starting column of line
;;;   y1; int, starting row of line
;;;   x2; int, ending col of line
;;;   y2; int, ending row of line
;;; Purpose:
;;;   image-compute-line! functions much like image-draw-line!, except, instead of 
;;;   drawing a line with the current fg color and brush setting,
;;;   image-compute-line! applies an rgb transforming function (fun*) to the rgb 
;;;   color of each of the pixels on the line between points (x1, y1) and (x2, y2)
;;; Produces:
;;;   absolutely nothing
;;; Preconditions:
;;;   all ints must be greater than or equal to 0
;;;   x1 and x2 must be less than (image-width image)
;;;   y1 and y2 must be less than (image-height image)
;;; Postconditions:
;;;   image-compute-line! applies fun* to all the pixels petween the points (x1, y1) 
;;;   and (x2, y2) that satisfy the equality: 
;;;         row + y1 = ((y2-y1) / (x2-x1)) * (col-x1)
;;;
;;;   Which is commonly recognized as:
;;;          y-y1 = m*(x-x1)
;;;                 *where m = (y2-y1)/(x2-x1)
;;;
;;;   One way to go about drawing a line would be to perform the above test on all
;;;   the pixels in the rectangular region bounded by (x1, y1) and (x2, y2).
;;;
;;;   But, we can simplify the equation even more to get:
;;;          y = m*x + b
;;;                 *where b = y1 - (m*x1)  
;;;
;;;   In the implementatoin, it is much easier (not to mention faster) to select a 
;;;   region (x2 - x1), and then, for each pixel xn, alter the corresponding pixel at 
;;;   the point (xn, yn) where yn = m*xn + b
;;;   
(define image-compute-line!
  (lambda (image fun* x1 y1 x2 y2)
    (let ((run (- x2 x1))
          (rise (- y2 y1)))
      (let ((m (if (= run 0) #f (/ rise run))))
        (letrec ((row! (lambda (x)
                         (when (<= x (max x1 x2))
                           (image-set-pixel! image x y1 (fun* (image-get-pixel image x y1)))
                           (row! (increment x)))))
                 (col! (lambda (y)
                         (when (<= y (max y1 y2))
                           (image-set-pixel! image x1 y (fun* (image-get-pixel image x1 y)))
                           (col! (increment y)))))
                 (slp! (lambda (x y)
                         (when (<= x (max x1 x2))
                           (image-set-pixel! image x (round y) (fun* (image-get-pixel image x (round y))))
                           (slp! (increment x) (+ y m))))))
          (if m 
              (if (= m 0)
                  (row! (min x1 x2))
                  (let ((b (- y1 (* m x1))))
                    (slp! (min x1 x2) (+ (* m (min x1 x2)) b))))
              (col! (min y1 y2)))
          )))
    ))


;;;========== safe image-compute-line! ==========;;;
;;;**almsot-working**
;;;+++SKIP TO image-compute-line***! FOR THE 100% WORKING VERSION
;;; Procedure:
;;;   image-compute-line*!
;;; Parameters:
;;;   same as image-compute-line! except you can put whatever you want in for the points
;;;   so long as they are integers 
;;;---DOES NOT WORK FOR LINES WITH A NEGAITVE SLOPE THAT START OFF THE CANVAS

(define image-compute-line*!
  (lambda (image fun* x1 y1 x2 y2)
    (let ((run (- x2 x1))
          (rise (- y2 y1))
          (w (image-width image))
          (h (image-height image)))
      (let ((m (if (= run 0) #f (/ rise run)))
            (x* (min (max x1 x2) (decrement w)))
            (y* (min (max y1 y2) (decrement h))))
        (letrec ((row! (lambda (x)
                         (when (<= x x*)
                           (image-set-pixel! image x y1 (fun* (image-get-pixel image x y1)))
                           (row! (increment x)))))
                 (col! (lambda (y)
                         (when (<= y y*)
                           (image-set-pixel! image x1 y (fun* (image-get-pixel image x1 y)))
                           (col! (increment y)))))
                 (slp! (lambda (x y x*)
                         (let it! ((x x)
                                   (y y))
                           (when (and (<= x x*) (<= y y*))
                             (image-set-pixel! image x y (fun* (image-get-pixel image x y)))
                             (display x) (display " ") (display y) (newline) (display x*) (newline)
                             (it! (increment x) (round (+ y m))))))))
          (if m 
              (if (= m 0)
                  (when (<= 0 y1 (decrement h)) (row! (max (min x1 x2) 0)))
                  
                  (let* ((b (- y1 (* m x1))))
                    (cond ((and (> m 0) (>= b 0))
                           (let* ((x_ (round (max (min x1 x2) 0)))
                                  (y_ (round (+ (* m x_) b))))
                             (display "case 1");debug stuff
                             (slp! x_ y_ x*)))
                          ((and (> m 0) (<= b 0))
                           (let* ((x_ (round (max (- (/ b m)) (min x1 x2))))
                                  (y_ (round (+ (* m x_) b))))
                             (display "case 2");debug stuff
                             (slp! x_ y_ x*)))
                          ((and (< m 0) (>= b 0))
                           (let* ((x* (round (min (max x1 x2) (- (/ b m)))))
                                  (x_ (round (max (min x1 x2) 0)))
                                  (y_ (round (+ (* m x_) b))))
                             (display "case 3:") (display " ");debug stuff --- case three still fails when going from 
                             (display x_) (display " ");debug stuff ---------- below the image to a point on or off 
                             (display y_);debug stuff ------------------------ the image such that the slope is 
                             (newline);debug stuff --------------------------- negative so x_ should to not be 0 (and thuss y_ should not be b)
                             (display x*) (display " ") (display b) (display " ") (display y*) (newline);debug stuff
                             (slp! x_ y_ x*))))))
              
              (when (<= 0 x1 (decrement w)) (col! (max (min y1 y2) 0)))
              ))
        ))
    ))

;;;=====+ Other safe image-compute-line +=====:::
;;;**99% working but a little more inefficient**
;;;---WORKS IN MOST CASES BUT NOT WHEN YOU SART A LINE OFF THE CANVAS
;;;---TO THE RIGHT AND BELOW

(define image-compute-line**!
  (lambda (image fun* x1 y1 x2 y2)
    (let ((run (- x2 x1))
          (rise (- y2 y1))
          (w (image-width image))
          (h (image-height image)))
      (let ((m (if (= run 0) #f (/ rise run)))
            (x* (min (max x1 x2) (decrement w)))
            (y* (min (max y1 y2) (decrement h))))
        (letrec ((row! (lambda (x)
                         (when (<= x x*) 
                           (image-set-pixel! image x y1 (fun* (image-get-pixel image x y1)))
                           (row! (increment x)))))
                 (col! (lambda (y)
                         (when (<= y y*)
                           (image-set-pixel! image x1 y (fun* (image-get-pixel image x1 y)))
                           (col! (increment y)))))
                 (slp! (lambda (x y)
                         (when (<= x x*)
                           (when (and (<= 0 x x*) (<= 0 y y*)) (image-set-pixel! image x y (fun* (image-get-pixel image x y))))
                             (slp! (increment x) (round (+ y m)))))))
          (if m 
              (if (= m 0)
                  (when (<= 0 y1 (decrement h)) (row! (max (min x1 x2) 0)))
                  
                  (let* ((b (- y1 (* m x1))))
                    (cond ((and (> m 0) (>= b 0))
                           (let* ((x_ (round (max (min x1 x2) 0)))
                                  (y_ (round (+ (* m x_) b))))
                             (slp! x_ y_)))
                          ((and (> m 0) (<= b 0))
                           (let* ((x_ (round (max (- (/ b m)) (min x1 x2))))
                                  (y_ (round (+ (* m x_) b))))
                             (slp! x_ y_)))
                          ((and (< m 0) (>= b 0))
                           (let* ((x_ (round (max (min x1 x2) 0)))
                                  (y_ (round (+ (* m x_) b))))
                             (slp! x_ y_))))))
              
              (when (<= 0 x1 (decrement w)) (col! (max (min y1 y2) 0)))
              ))
        ))
    ))

;;;+++===------===+++;;;
;;;Safe, 100% working, least efficient, ie. procedure will sill run
;;;   for lines that are completely off the canvas as well as run
;;;   for the parts of lines that are off the canvas.  Idealy, 
;;;   the procedure should not 'calculate' and parts of the line that
;;;   are off the canvas.  The extra time is not noticable in a single 
;;;   use of image-compute-line***! as apposed to the earlier versions
;;;   BUT, it probalby would show if you were to use the procedure over
;;;   and over again.. (perhaps in a fractal...)
(define image-compute-line***!
  (lambda (image fun* x1 y1 x2 y2)
    (let ((run (- x2 x1))
          (rise (- y2 y1))
          (w (image-width image))
          (h (image-height image)))
      (let ((m (if (= run 0) #f (/ rise run)))
            (x* (min (max x1 x2) (decrement w)))
            (y* (min (max y1 y2) (decrement h))))
        (letrec ((row! (lambda (x)
                         (when (<= x x*) 
                           (image-set-pixel! image x y1 (fun* (image-get-pixel image x y1)))
                           (row! (increment x)))))
                 (col! (lambda (y)
                         (when (<= y y*)
                           (image-set-pixel! image x1 y (fun* (image-get-pixel image x1 y)))
                           (col! (increment y)))))
                 (slp! (lambda (x y)
                         (when (<= x x*)
                           (when (and (<= 0 x x*) (<= 0 y y*)) (image-set-pixel! image x y (fun* (image-get-pixel image x y))))
                           (slp! (increment x) (round (+ y m)))))))
          (if m 
              (if (= m 0)
                  (when (<= 0 y1 (decrement h)) (row! (max (min x1 x2) 0)))
                  
                  (let* ((b (- y1 (* m x1)))
                         (x_ (min x1 x2))
                         (y_ (+ (* m x_) b)))
                    (slp! x_ y_)))
              
              (when (<= 0 x1 (decrement w)) (col! (max (min y1 y2) 0)))
              ))
        ))
    ))


;;;==Old image-compute-line! ===;;;  ***sometimes this doesn't work and we have no idea why..
;;;                          which is why we aren't using it.  It is also the least efficient.
;;;   *only difference (documentation wise) from above is what is listed below*
;;; Procedure
;;;   image-compute-line_old!
;;; Postconditions:
;;;   image-compute-line! applies fun! to all the pixels in the rectangular region
;;;   bounded by (x1, y1) and (x2, y2) that satisfy the equality: 
;;;         row + y1 = ((y2-y1) / (x2-x1)) * (col-x1)  
;;;   ...math...
;;;   This equation is commonly seen in the simplified form/s:
;;;
;;;      y-y1 = m*(x-x1)
;;;                 *where m = (y2-y1)/(x2-x1)
;;;   or
;;;      y = m*x + b
;;;                 *where b = y1 - (m*x1)         
(define image-compute-line_old!
  (lambda (image fun* x1 y1 x2 y2)
    (let* ((m (/ (- y2 y1) (- x2 x1)))
           (line-fun! (lambda (col row rgb)
                        (if (= (- row y1) (round (* m (- col x1))))
                            (fun* rgb)
                            rgb))))
      (region-scan image (min x1 x2) (min y1 y2) (abs (- x2 x1)) (abs (- y2 y1)) line-fun!)
      )
    )
  )