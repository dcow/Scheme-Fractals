;;;project_library
;--see index for a list of procedures--;

;;;grab the cool hue remaping procedure, very useful to enliven the julia set
(load "/home/cowdenda/scheme/rgb-hue-remap.scm")

;;;grab our grid procedure, a slight modification of the carpet-holes procedure
(load "/home/cowdenda/scheme/final_project/grids.scm")


;;; Procedure:
;;;   rgb-invert
;;; Parameters:
;;;   rgb
;;; Purpose:
;;;   inverts the given rgb color
;;; Produces:
;;;   rgb value
;;; Preconditions:
;;;   rgb must be an rgb value
;;; Postconditions:
;;;   none

(define rgb-invert
  (lambda (rgb)
    (rgb-new (- 255 (rgb-red rgb)) (- 255 (rgb-green rgb)) (- 255 (rgb-blue rgb)))))

;;; Procedure:
;;;   rgb-light?
;;; Parameters:
;;;   color, an RGB color
;;; Purpose:
;;;   Determine if the color seems light.
;;; Produces:
;;;   light?, a Boolean value
;;; Preconditions:
;;;   [None]
;;; Postconditions:
;;;   light? is true (#t) if color's intensity is relatively high.
;;;   light? is false (#f) otherwise.
;;; Cited from:
;;;   http://www.cs.grinnell.edu/~rebelsky/Courses/CS151/2010S/Labs/boolean-lab.html

(define rgb-light?
  (lambda (color)
    (<= 192 (+ (* 0.30 (rgb-red color)) (* 0.59 (rgb-green color)) (* 0.11 (rgb-blue color))))))

;;;the following are rgb-transforming procedures
(define rgb-overlay
  (lambda (rgb_top rgb_bottom)
    (if (rgb-light? rgb_top)
        (rgb-screen rgb_top rgb_bottom)
        (rgb-multiply rgb_top rgb_bottom))))

(define rgb-slightlier
  (lambda (rgb) (rgb-new (+ (rgb-red rgb) 12) (+ (rgb-green rgb) 12) (+ (rgb-blue rgb) 12))))

(define ddarker
  (lambda (rgb)
    (rgb-darker (rgb-darker rgb))))

(define rgb-multiply
  (lambda (rgb_top rgb_bottom)
    (rgb-new (/ (* (rgb-red rgb_top) (rgb-red rgb_bottom)) 255) 
             (/ (* (rgb-green rgb_top) (rgb-green rgb_bottom)) 255) 
             (/ (* (rgb-blue rgb_top) (rgb-blue rgb_bottom)) 255))))

(define rgb-screen
  (lambda (rgb_top rgb_bottom)
    (rgb-new (- 255 (/ (* (rgb-red rgb_top) (rgb-red rgb_bottom)) 255)) 
             (- 255 (/ (* (rgb-green rgb_top) (rgb-green rgb_bottom)) 255)) 
             (- 255 (/ (* (rgb-blue rgb_top) (rgb-blue rgb_bottom)) 255)))))

