;;; Procedure:
;;;   dragon!
;;; Parameters:
;;;   panda;  a panda
;;;   n; int, depth of recursion of the fractal
;;;   [diet]; an rbg changing procedure
;;;   [d]; sidelength of fractal
;;; Purpose:
;;;   dragon renders the dragon fractal with the specified panda to a depth
;;;   of n.  if a diet is specified for the panda the panda will eat the leaves 
;;;   (pixels) accordingly.  If d is specified, the side-length will be d,
;;;   otherwise d will be 5.
;;; Produces:
;;;   nada (a procedure)
;;; Preconditions:
;;;   panda must exist
;;; Postconditions:
;;;   panda will NOT be returned to it's original position; panda's like to explore
;;;   the image on which the panda resids will be altered
(define dragon!
  (lambda (panda n . diet)
    (let ((d 5))
      (when (not (null? diet)) 
        (panda ':set-diet! (car diet)) 
        (when (= (length diet) 2)
          (set! d (cadr diet))))
      (let dragon-egg ((n n)
                       (f #t))
        (if (= n 0)
            (panda-climb! panda d)
            (if f 
                ((lambda () 
                   (panda-look! panda 45)
                   (dragon-egg (decrement n) f)
                   (panda-look! panda -90)
                   (dragon-egg (decrement n) (not f))
                   (panda-look! panda 45)))
                ((lambda ()
                   (panda-look! panda -45)
                   (dragon-egg (decrement n) (not f))
                   (panda-look! panda 90)
                   (dragon-egg (decrement n) f)
                   (panda-look! panda -45)))
                ))))))


;;; Procedure:
;;;   dragoon!
;;; Parameters:
;;;   panda;  a panda
;;;   n; int, depth of recursion of the fractal
;;;   [diet]; an rbg changing procedure
;;;   [d]; sidelength of fractal
;;; Purpose:
;;;   Dragoons are stupider than dragons. They get lost and constantly
;;;   retrace their steps.  However, this also creates some unique looking
;;;   patterns. Dragoon! renders the slightly rewritten dragon fractal with 
;;;   the specified panda to a depth of n.  if a diet is specified for the 
;;;   panda the panda will eat the leaves (pixels) accordingly.  If d is 
;;;   specified, the side-length will be d, otherwise d will be 5.
;;; Produces:
;;;   nada (a procedure)
;;; Preconditions:
;;;   panda must exist
;;; Postconditions:
;;;   panda will NOT be returned to it's original position; panda's like to explore
;;;   the image on which the panda resids will be altered
(define dragoon!
  (lambda (panda n . diet)
    (let ((d 5))
      (when (not (null? diet)) 
        (panda ':set-diet! (car diet)) 
        (when (= (length diet) 2)
          (set! d (cadr diet))))
      (let dragon-egg ((n n)
                       (f #t))
        (if (= n 0)
            (panda-climb! panda d)
            (if f 
                ((lambda () 
                   (panda-look! panda 45)
                   (dragon-egg (decrement n) f)
                   (panda-look! panda -90)
                   (dragon-egg (decrement n) (not f))
                   (panda-look! panda 45)))
                ((lambda ()
                   (panda-look! panda -45)
                   (dragon-egg (decrement n) f)
                   (panda-look! panda 90)
                   (dragon-egg (decrement n) (not f))
                   (panda-look! panda -45)))
                ))))))
