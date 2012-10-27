;;; Procedure:
;;;   julia!
;;; Parameters:
;;;   n; a number
;;;   img; an image (really should be a canvas)
;;;   [color-scheme]; procedure
;;; Purpose:
;;;   renders a julia-set type complex fractal on image
;;;   !!!warning, previous image will be altered!!!
;;; Produces:
;;;   image (img)
;;; Preconditions:
;;;   n should be between 0 and 1000 to get best results but it doesn't have to be
;;; Postconditions:
;;;   beauty
;;; Other:
;;;   Cool color schemes - - + - - - - - - - - - - - - - - - - - - - - - - - - - +
;blue                        | (rgb-new (+ 20 n) (+ 70 (* 1 n)) (+ 120 (* 7 n))) |
;aqua                        | (rgb-new (+ 20 n) (+ 70 (* 4 n)) (+ 120 (* 7 n))) |
;cool                        | (rgb-new (+ 120 n) (+ 70 (* 3 n)) (+ 80 (* 7 n))) |
;;;                      - - + - - - - - - - - - - - - - - - - - - - - - - - - - +
;;;--- if you are interested in more fractals, the mandelbrot set can be found here: 
;;;   /home/cowdenda/scheme/mandelbrot.scm
;;;   ~it's kinda messy

(define julia!
  (lambda (n img . color-scheme)
    (let ((n_ (+ 0.3520 (* n 0.0002288)))
          (c-scheme (if (null? color-scheme) 
                        (lambda (n) (rgb-new (+ 120 n) (+ 70 (* 3 n)) (+ 80 (* 7 n))))
                        (car color-scheme))))
      (let* ((min_real (* n_ -2.0))
             (max_real n_)
             (min_imag (* n_ -1.2))
             (max_imag (+ (* (- n_ min_real) (/ (image-height img) (image-width img))) min_imag))
             (real_offset (/ (- max_real min_real) (decrement (image-width img))))
             (imag_offset (/ (- max_imag min_imag) (decrement (image-height img)))))
        (let ((calculate-set (lambda (col row)
                               (let ((c_real (+ min_real (* col real_offset)))
                                     (c_imag (- max_imag (* row imag_offset))))
                                 (let seed ((Z_real c_real)
                                            (Z_imag c_imag)
                                            (n 0))
                                   (if (or (> (+ (expt Z_real 2) (expt Z_imag 2)) 4) (= n 100))
                                       (c-scheme n)
                                       (seed (+ (- (expt Z_real 2) (expt Z_imag 2)) n_) (+ (* 2 Z_real Z_imag) n_) (increment n))))))))
          
          (image-compute-pixels! img calculate-set)
          img
          )))
    ))