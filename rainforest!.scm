;;;===+Project+===;;;
;;;
;;;   *library:
(load "/home/cowdenda/scheme/final_project/project_library.scm")

;;;   ***Part 1:
(load "/home/cowdenda/scheme/final_project/julia!.scm")

;;;   ***Part 2:
(load "/home/cowdenda/scheme/final_project/pandas.scm")
(load "/home/cowdenda/scheme/final_project/dragons!.scm")

;;;   ***Part 3:
(load "/home/cowdenda/scheme/final_project/circles.scm")


;;; Procedure:
;;;   rainforest! (rainforest! n, width, height, [hue-remap-args-list], [color-scheme-list])
;;; Parameters:
;;;   n; your favorite integer between 0 and 1000, inclusive
;;;   width; positive integer
;;;   height; positive integer
;;;   [hue-remap-args-list]; list
;;;   *** hue-remap-args-list should have the form (':remap-list angle range [crazy])
;;;      *to turn hue remaping off, pass #f as the second elemnet in the list
;;;   [color-scheme-list]; list
;;;   *** color-scheme-list should have the form (':color-scheme (lambda (n) (rgb-n)))
;;;   
;;; Purpose:
;;;   Utilizing an exotic blend of recursion, complex-numbers, hue-remaps,
;;;   grids, circles, pandas, and dragons, rainforest! captures just one (n)
;;;   of the near limitless frames produced by this jungle of procedures.
;;; Produces:
;;;   an image (int)
;;; Preconditions:
;;;   an open mind
;;; Postconditions:
;;;   *The jungle will have an increasingly interesting julia set the lower n is.
;;;     The julia sets are personal preference but it's arguable that most 
;;;     people would agree that the julia sets w/ n < 500 look the coolest.
;;;   *The dragon's size is determined by the tens digit of n.  Note, larger
;;;     dragon and dragoons take longer to grow.
;;;   *The configuration of the circles is determined by the ones place.
;;;   *If n is even, the jungle will be lighter. If n is odd, the jungle will be darker.

(define rainforest!
  (lambda (n width height . flair)
    (let* ((jungle (image-new width height))
           (yun-zi (panda-new jungle))
           (drop-ths-plc (- n (* 100 (floor (/ n 100)))))
           (ones (- drop-ths-plc (* 10 (floor (/ drop-ths-plc 10)))))
           (tens (/ (- drop-ths-plc ones) 10))
           (dragon-width (ceiling (/ width 100)))
           (param-1 (if (and (not (null? flair)) (equal? (length flair) 1))
                        (car flair)
                        null))
           (param-2 (if (and (not (null? flair)) (equal? (length flair) 2))
                        (cadr flair)
                        null))
           (remap-args (if (and (not (null? param-1)) (equal? (car param-1) ':remap-list))
                           param-1
                           (if (and (not (null? param-2)) (equal? (car param-2) ':remap-list))
                               param-2
                               null)))                       
           (remap-angle  (if (null? remap-args)
                             (modulo (round (* n 1080/1000)) 360);angle
                             (and (cadr remap-args))))
           (remap-range  (if (null? remap-args)
                             (round (+ 360 (* n 720/1000)))
                             (caddr remap-args)))
           (c-scheme-list (if (and (not (null? param-1)) (equal? (car param-1) ':color-scheme))
                              param-1
                              (if (and (not (null? param-2)) (equal? (car param-2) ':color-scheme))
                                  param-2
                                  null))) 
           )
      
      ;;;***Part 1***;;;
      (if (null? c-scheme-list)
          (julia! n jungle)
          (julia! n jungle (cadr c-scheme-list))) 
      (when remap-angle (image-transform! jungle 
                                          (lambda (rgb) 
                                            (rgb-hue-remap rgb 
                                                           remap-angle
                                                           remap-range))))
      
      ;;;***Part 2***;;;
      (panda-crawl! yun-zi (* 1/4 width) (* 1/2 height))
      (dragoon! yun-zi (+ tens 3) rgb-slightlier dragon-width)
      
      (panda-crawl! yun-zi (* width 2/5) (* height 3/5))
      (dragon! yun-zi (+ tens 5) rgb-lighter dragon-width)
      
      (panda-crawl! yun-zi (ceiling (/ width 10)) (ceiling (/ height 10)))
      (dragoon! yun-zi tens rgb-slightlier dragon-width)
      
      ;;;***Part 3***;;;
      (circles ones jungle width height)
      (selection-transform! jungle rgb-lighter)
      (image-select-nothing! jungle)
      
      ;;;---add a grid---;;;
      (mesh-grid jungle 0 0 width height 5 5)
      (image-select-inverse! jungle)
      (selection-transform! jungle rgb-darker)
      (image-select-nothing! jungle)
      
      ;;;---invert on even---;;;
      (when (even? n)
        (image-transform! jungle rgb-invert))
      
      ;;;---return the beautiful creation---;;;
      jungle
      )))

