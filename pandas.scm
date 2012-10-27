;;; Procedure:
;;;   panda-new
;;; Parameters:
;;;   image; an image
;;; Purpose:
;;;   creates a new panda on the image given
;;; Produces:
;;;   panda
;;; Preconditions:
;;;   image must exist
;;; Postconditions:
;;;   a panda will roam it's new jungle eating leaves as you instruct it

(define panda-new
  (lambda (image)
    (let ((env image)
          (col 0)
          (row 0)
          (angle 0)
          (diet (lambda (rgb) (rgb-new 0 0 0))))
      (lambda (message . leaves)
        (cond
          ((eq? message ':type)
           'panda)
          ((eq? message ':jungle)
           env)
          ((eq? message ':col)
           col)
          ((eq? message ':row)
           row)
          ((eq? message ':angle)
           angle)
          ((eq? message ':diet)
           diet)
          ((eq? message ':set-jungle)
           (set! world (car leaves)))
          ((eq? message ':set-col!)
           (set! col (car leaves)))
          ((eq? message ':set-row!)
           (set! row (car leaves)))
          ((eq? message ':set-angle!)
           (set! angle (car leaves)))
          ((eq? message ':set-diet!)
           (set! diet (car leaves)))
          )))))

;;; Procedure:
;;;   panda-climb!
;;; Parameters:
;;;   panda; a panda
;;;   distance, int, distance 
;;; Purpose:
;;;   panda will climb a distance distance in the direction it is looking
;;;   eating all the leaves on the way
;;; Produces:
;;;   panda
;;; Preconditions:
;;;   
;;; Postconditions:
;;;   
(define panda-climb!
  (let ((d2r (/ pi 180)))
    (lambda (panda distance)
      (let ((col (inexact->exact (panda ':col)))
            (row (inexact->exact (panda ':row)))
            (angle (panda ':angle)))
        (let ((newcol (inexact->exact (round (+ col (* distance (cos (* d2r angle)))))))
              (newrow (inexact->exact (round (+ row (* distance (sin (* d2r angle)))))))
              (jungle-width (decrement (image-width (panda ':jungle))))
              (jungle-height (decrement (image-height (panda ':jungle)))))
          (if (and (and (<= 0 col jungle-width) (<= 0 newcol jungle-width)) 
                   (and (<= 0 row jungle-height) (<= 0 newrow jungle-height)))
              (image-compute-line! (panda ':jungle) (panda ':diet)
                                   col row
                                   newcol newrow)
              (image-compute-line***! (panda ':jungle) (panda ':diet)
                                      col row
                                      newcol newrow))
          (panda ':set-col! newcol)
          (panda ':set-row! newrow)))
      panda)))

;;; Procedure:
;;;   panda-crawl!
;;; Parameters:
;;;   panda; a panda
;;;   col, int, column
;;;   row, int, row
;;; Purpose:
;;;   panda will crawl to the new location without eating any leaves
;;; Produces:
;;;   panda

(define panda-crawl!
  (lambda (panda col row)
    (panda ':set-col! col)
    (panda ':set-row! row)
    panda))

;;; Procedure:
;;;   panda-look!
;;; Parameters:
;;;   panda; a panda
;;;   angle, int, and angle
;;; Purpose:
;;;   panda will look in the direction given
;;; Produces:
;;;   panda
;;; Preconditions:
;;;   angle must be an integer

;(define panda-look! 
;    (lambda (panda angle)
;      (panda ':set-angle!
;              (modulo (+ (panda ':angle) angle 360))
;      panda)))

(define panda-look!
  (letrec ((fixangle
            (lambda (angle)
              (if (>= angle 360) (fixangle (- angle 360))
                  (if (< angle 0) (fixangle (+ angle 360))
                      angle)))))  
    (lambda (panda angle)
      (panda ':set-angle!
             (fixangle (+ (panda ':angle) angle)))
      panda)))

;;; Procedure:
;;;   panda-face!
;;; Parameters:
;;;   panda; a panda
;;;   angle; a real number
;;; Purpose:
;;;   
;;; Produces:
;;;   
;;; Preconditions:
;;;   
;;; Postconditions:
;;;   

(define panda-face!
  (lambda (panda angle)
    (panda ':set-angle! angle)
    panda))

;;; Procedure:
;;;   panda-diet!
;;; Parameters:
;;;   diet; a single argument function (lambda (rgb) altered-rgb)
;;; Purpose:
;;;   causes the panda to change it's diet
;;; Produces:
;;;   panda
;;; Preconditions:
;;;   
;;; Postconditions:
;;;   

(define panda-diet!
  (lambda (panda diet)
    (panda ':set-diet! diet)
    panda))

;;;load required procedure
(load "/home/cowdenda/scheme/final_project/image-compute-line!.scm")
