;;; Procedure:
;;;   mesh-grid
;;; Parameters:
;;;   image; an image
;;;   left; the left coordinate of the start point of the grid
;;;   top; the top coordinate of the start point of the grid
;;;   width; width of the grid
;;;   height; height of the grid
;;;   g-width; width of one cell of the grid
;;;   g-height; height of one cell of the grid
;;;   [s-type]; type of selection created (ie. ADD, SUBTRACT, REPLACE)
;;; Purpose:
;;;   Selects the holes in a mesh grid (like a screen) on image.  The
;;;   grid will start at (left, top) and span width x height.  If g-width
;;;   and g-height are set to 5, the cells will be (g-width - 1) x (g-height - 1)
;;;   with 1 px of spacing inbetween.
;;; Produces:
;;;   nothing (a selection)
;;; Preconditions:
;;;   width, height, left, top, g-width, and g-height should all be integers
;;;   (as should image..)
;;;   ***NOTE***
;;;   *Things work best when width/g-width and height/g-height are whole numbers*
;;;   *This way the grid will fit evenly in the dimentions*
;;; Postconditions:
;;;   The cells will be selected, not the space inbetween (assuming s-type is not
;;;   altered).  s-type defaults to ADD

(define mesh-grid
  (lambda (image left top width height g-width g-height . s-type)
    (let ((w (- (min (- (image-width image) left) width) g-width))
          (h (- (min (- (image-height image) top) height) g-height))
          (w-offset (- (round (* g-width 0.8)) 1))
          (h-offset (- (round (* g-height 0.8)) 1))
          (mesh-w (round (* g-width 0.4)))
          (mesh-h (round (* g-height 0.4)))
          (s-type (if (null? s-type) 0 (car s-type))))
      (let row ((i top))
        (if (<= i (+ h top))
            (let col ((j left))
              (let ((circle (lambda () 
                              (image-select-rectangle! image s-type
                                                       j i
                                                       mesh-w mesh-h)
                              (image-select-rectangle! image s-type
                                                       (+ j w-offset) i
                                                       mesh-w mesh-h)
                              (image-select-rectangle! image s-type
                                                       j (+ i h-offset)
                                                       mesh-w mesh-h)
                              (image-select-rectangle! image s-type
                                                       (+ j w-offset)
                                                       (+ i h-offset)
                                                       mesh-w mesh-h)
                              (col (+ j g-width)))))                                                   
                (if (<= j (+ w left))
                    (circle)
                    (row (+ i g-height))))))))))

;;; Procedure:
;;;   circle-grid
;;; Parameters:
;;;   image; an image
;;;   left; the left coordinate of the start point of the grid
;;;   top; the top coordinate of the start point of the grid
;;;   width; width of the grid
;;;   height; height of the grid
;;;   g-width; width of one cell of the grid
;;;   g-height; height of one cell of the grid
;;;   [s-type]; type of selection created (ie. ADD, SUBTRACT, REPLACE)
;;; Purpose:
;;;   selects a grid of circles on image that starts at (left, top)
;;;   and has dimentions width x height.  The cells are g-width x 
;;;   g-height.
;;; Produces:
;;;   nothing
;;; Preconditions:
;;;   width, height, left, top, g-width, and g-height should all be integers
;;;   (as should image..)
;;;   ***NOTE***
;;;   *Things work best when width/g-width and height/g-height are whole numbers*
;;;   *This way the grid will fit evenly in the dimentions*
;;; Postconditions:
;;;   The circles of the circle grid (not the inverse) are selected on image.
;;;   s-type defaults to add

(define circle-grid
  (lambda (image left top width height g-width g-height . s-type)
    (let ((w (- (min (- (image-width image) left) width) g-width))
          (h (- (min (- (image-height image) top) height) g-height))
          (w-offset (round (* g-width 0.2)))
          (h-offset (round (* g-height 0.2)))
          (ellipse-w (round (* g-width 0.6)))
          (ellipse-h (round (* g-height 0.6)))
          (s-type (if (null? s-type) 0 (car s-type))))
      (let row ((i top))
        (if (<= i (+ h top))
            (let col ((j left))
              (let ((circle (lambda () 
                              (image-select-ellipse! image s-type
                                                     (round (+ j w-offset))
                                                     (round (+ i h-offset))
                                                     ellipse-w ellipse-h)
                              (col (+ j g-width))))) 
                (if (<= j (+ w left))
                    (circle)
                    (row (+ i g-height))))))))))
