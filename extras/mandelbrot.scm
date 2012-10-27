;;; Procedure:
;;;   mandelbrot
;;; Parameters:
;;;   n; positive ints work best
;;;   width; image width
;;;   height; image height
;;; Purpose:
;;;   renders a 2D representation of the mandelbrot set
;;; Produces:
;;;   image (an int)
;;; Preconditions:
;;;   
;;; Postconditions:
;;;   
(define mandelbrot
  (lambda (n w h)
    (let ((mandy (image-new w h))
          (n_ (/ 1 (* 0.57 (increment n)))))
      (let* ((min_real (* n_ -2.0))
             (max_real n_)
             (min_imag (* n_ -1.2))
             (max_imag (+ (* (- n_ min_real) (/ h w)) min_imag))
             (real_offset (/ (- max_real min_real) (decrement w)))
             (imag_offset (/ (- max_imag min_imag) (decrement h))))
        (let ((mandel (lambda (col row)
                        (let ((c_real (+ min_real (* col real_offset)))
                              (c_imag (- max_imag (* row imag_offset))))
                          (let brot ((Z_real c_real)
                                     (Z_imag c_imag)
                                     (n 0))
                            (if (or (> (+ (expt Z_real 2) (expt Z_imag 2)) 4) (= n 100))
                                
                                ;((lambda () (display n) (display " ")  (rgb-new (+ 50 (* 2 n)) (* 10 n) (+ 100 n))))
                                (rgb-new (+ 50 (* 2 n)) (* 10 n) (+ 100 n))
                                
                                (brot (+ (- (expt Z_real 2) (expt Z_imag 2)) c_real) (+ (* 2 Z_real Z_imag) c_imag) (increment n))))))))
          (image-compute-pixels! mandy mandel)
          mandy
          )))
    ))

(define mandelbrot_2
  (lambda (n w h)
    (let ((mandy (image-new w h))
          (n_ (/ 1 (* 0.57 (increment n)))))
      (let* ((min_real (* n_ -2.0))
             (max_real n_)
             (min_imag (* n_ -1.2))
             (max_imag (+ (* (- n_ min_real) (/ h w)) min_imag))
             (real_offset (/ (- max_real min_real) (decrement w)))
             (imag_offset (/ (- max_imag min_imag) (decrement h))))
        (let ((mandel (lambda (col row)
                        (let ((c_real (+ min_real (* col real_offset)))
                              (c_imag (- max_imag (* row imag_offset))))
                          (let brot ((Z_real c_real)
                                     (Z_imag c_imag)
                                     (n 0))
                            (if (or (> (+ (expt Z_real 2) (expt Z_imag 2)) 4) (= n 100))
                                
                                ;((lambda () (display n) (display " ")  (rgb-new (+ 50 (* 2 n)) (* 10 n) (+ 100 n))))
                                (cond ((< n 16) (rgb-new (* 3 n) (* n 16) (* n 8)))
                                      ((= n 100) (rgb-new (* row (/ row (* h 3/2))) (* col row (/ col (* h  3/2) (/ row w))) (* col (/ col (* w 7/4)))))
                                      (else (rgb-new (- 100 n) n (- 220 n))))
                                
                                (brot (+ (- (expt Z_real 2) (expt Z_imag 2)) c_real) (+ (* 2 Z_real Z_imag) c_imag) (increment n))))))))
          (image-compute-pixels! mandy mandel)
          mandy
          )))
    ))

(define mandelbrot_sandbox
  (lambda (n w h)
    (let ((mandy (image-new w h))
          (n_ (/ 1 (* 0.37 (increment n)))))
      (let* ((min_real (+ n_ (* n_ -2.0)))
             (max_real (/ (expt n_ n_) (expt n_ 2)))  
             (min_imag (+ n_ (* n_ -1.2)))
             (max_imag (+ (* (- n_ min_real) (/ h w)) min_imag))
             (real_offset (/ (- max_real min_real) (decrement w)))
             (imag_offset (/ (- max_imag min_imag) (decrement h))))
        (let ((mandel (lambda (col row)
                        (let ((c_real (+ min_real (* col real_offset)))
                              (c_imag (- max_imag (* row imag_offset))))
                          (let brot ((Z_real c_real)
                                     (Z_imag c_imag)
                                     (n 0))
                            (if (or (> (+ (expt Z_real 2) (expt Z_imag 2)) 4) (= n 100))
                                (rgb-new (+ 150 n) (+ 50 n) (+ 100 n))
                                (brot (+ (- (expt Z_real 2) (expt Z_imag 2)) c_real) (+ (* 2 Z_real Z_imag) c_imag) (increment n))))))))
          (image-compute-pixels! mandy mandel)
          mandy
          )))
    ))
