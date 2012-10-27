;;; Procedure:
;;;   circles
;;; Parameters:
;;;   n, int
;;;   image, an image
;;;   w, int, width
;;;   h, int, height
;;; Purpose:
;;;   given an n 0-9, circles uses circle-grid to draw a varying 
;;;   number of circles.  n=0 results in a grid covering the 
;;;   whole image
;;; Produces:
;;;   nothing
;;; Preconditions:
;;;   image must exist
;;; Postconditions:
;;;   image will be altered

(define circles
  (lambda (n image w h)
    (let ((g-width (max 1 (round (/ w 50))))
          (g-height (max 1 (round (/ h 50)))))
      (when (>= n 0)
        (circle-grid image 0 0 w h g-width g-height))
      (when (>= n 1)
        (circle-grid image (* 40 g-width) 0 (* 10 g-width) h g-width g-height SUBTRACT))
      (when (>= n 2)
        (circle-grid image 0 (* 40 g-height) w (* 10 g-height) g-width g-height SUBTRACT))
      (when (>= n 3)
        (circle-grid image (* 20 g-width) (* 10 g-height) (* 20 g-width) (* 30 g-height) g-width g-height SUBTRACT))
      (when (>= n 4)
        (circle-grid image (* 20 g-width) (* 40 g-height) w (* 10 g-width) g-width g-height SUBTRACT))
      (when (>= n 5)
        (circle-grid image 0 (* 42 g-height) w (* 5 g-height) g-width g-height SUBTRACT))
      (when (>= n 6)
        (circle-grid image (* 15 g-width) (* 30 g-height) (* 20 g-width) (* 15 g-height) g-width g-height SUBTRACT))
      (when (>= n 7)
        (circle-grid image (* 17 g-width) (* 21 g-height) (* 3 g-width) (* 3 g-height) (* 3 g-width) (* 3 g-height) ADD))
      (when (>= n 8)
        (circle-grid image (* 12 g-width) (* 24 g-height) (* 10 g-width) (* 10 g-height) (* 10 g-width) (* 10 g-height) ADD))
      (when (= n 9)
        (circle-grid image 0 (* 47 g-height) (* 5 g-width) (* 3 g-height) g-width g-height ADD))
      )
    ))

