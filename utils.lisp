;;;; +----------------------------------------------------------------+
;;;; | TOWERS - A silly geoDefense clone wannabe          DEATH, 2009 |
;;;; +----------------------------------------------------------------+

(in-package #:towers)


;;;; Utilities

(defconstant single-pi (coerce pi 'single-float))

(defun rad (deg)
  (/ (* single-pi deg) 180.0))

(defun deg (rad)
  (/ (* 180.0 rad) single-pi))

(defun normalize-deg (deg)
  (loop while (>= deg 360.0) do (decf deg 360.0))
  (loop while (< deg 0.0) do (incf deg 360.0))
  deg)

(defun sind (deg)
  (sin (rad deg)))

(defun cosd (deg)
  (cos (rad deg)))

(defun collect-results (fn &rest args)
  (let ((collection '()))
    (apply fn
           (lambda (&rest results)
             (push results collection))
           args)
    (nreverse collection)))

(defun call-with-circle-multipliers (fn &optional (segments 30))
  ;; http://github.com/sykopomp/until-it-dies/blob/master/src/primitives.lisp
  ;; Stole implementation of draw-circle and modified it a bit
  (let* ((theta (* 2.0 (/ single-pi segments)))
         (tangential-factor (tan theta))
         (radial-factor (- 1.0 (cos theta))))
    (loop with x = 1.0
          with y = 0.0
          repeat segments
          do (funcall fn x y)
          (let ((tx (- y))
                (ty x))
            (incf x (* tx tangential-factor))
            (incf y (* ty tangential-factor)))
          (let ((rx (- x))
                (ry (- y)))
            (incf x (* rx radial-factor))
            (incf y (* ry radial-factor))))))

(define-compiler-macro draw-circle (&whole form radius &optional (segments 30) (filledp nil))
  (if (integerp segments)
      (once-only (radius)
        `(gl:with-primitives (if ,filledp :triangle-fan :line-loop)
           (loop for (x y) in ',(collect-results #'call-with-circle-multipliers segments)
                 do (gl:vertex (* ,radius x) (* ,radius y)))))
      form))
      
(defun draw-circle (radius &optional (segments 30) (filledp nil))
  (gl:with-primitives (if filledp :triangle-fan :line-loop)
    (call-with-circle-multipliers
     (lambda (x y) (gl:vertex (* x radius) (* y radius)))
     segments)))

(defun mod+ (n m p)
  (mod (+ n m) p))
  
(defun call-with-star-multipliers (fn points density)
  (let ((xs (make-array points :element-type 'single-float))
        (ys (make-array points :element-type 'single-float)))
    (let ((i 0))
      (call-with-circle-multipliers
       (lambda (x y)
         (setf (aref xs i) x)
         (setf (aref ys i) y)
         (incf i))
       points))
    (dotimes (i points)
      (let ((j (mod+ i density points)))
        (funcall fn
                 (aref xs i) (aref ys i)
                 (aref xs j) (aref ys j))))))

(define-compiler-macro draw-star (&whole form radius points density)
  (if (and (integerp points) (integerp density))
      (once-only (radius)
        `(gl:with-primitive :lines
           (loop for (x1 y1 x2 y2) in ',(collect-results #'call-with-star-multipliers points density) do
                 (gl:vertex (* ,radius x1) (* ,radius y1))
                 (gl:vertex (* ,radius x2) (* ,radius y2)))))
      form))

(defun draw-star (radius points density)
  (gl:with-primitive :lines
    (call-with-star-multipliers
     (lambda (x1 y1 x2 y2)
       (gl:vertex (* x1 radius) (* y1 radius))
       (gl:vertex (* x2 radius) (* y2 radius)))
     points density)))

(defun call-with-curve-multipliers (fn &optional (segments 20))
  (funcall fn 1.0 0.0 0.0 0.0)
  (loop with step = (/ 1.0 segments)
        repeat (- segments 2)
        for u = step then (+ u step)
        for v = (- 1.0 u)
        for am = (* 1.0 v v v)
        for bm = (* 3.0 v v u)
        for cm = (* 3.0 v u u)
        for dm = (* 1.0 u u u)
        do (funcall fn am bm cm dm))
  (funcall fn 0.0 0.0 0.0 1.0))

(define-compiler-macro draw-cubic-curve (&whole form ax ay bx by cx cy dx dy &optional (segments 20))
  (if (integerp segments)
      (once-only (ax ay bx by cx cy dx dy)
        `(gl:with-primitive :line-strip
           (loop for (am bm cm dm) in ',(collect-results #'call-with-curve-multipliers segments)
                 do (gl:vertex (+ (* am ,ax) (* bm ,bx) (* cm ,cx) (* dm ,dx))
                               (+ (* am ,ay) (* bm ,by) (* cm ,cy) (* dm ,dy))))))
      form))

(defun draw-cubic-curve (ax ay bx by cx cy dx dy &optional (segments 20))
  (gl:with-primitive :line-strip
    (call-with-curve-multipliers
     (lambda (am bm cm dm)
       (gl:vertex (+ (* am ax) (* bm bx) (* cm cx) (* dm dx))
                  (+ (* am ay) (* bm by) (* cm cy) (* dm dy))))
     segments)))

(defun square (x)
  (* x x))

(defun best-element (list &key (key #'identity) (test #'<))
  (assert list)
  (let ((best (first list))
        (best-key (funcall key (first list))))
    (dolist (x (rest list))
      (let ((key (funcall key x)))
        (when (funcall test key best-key)
          (setf best x best-key key))))
    (values best best-key)))

(defun cddddddr (cons) (cddddr (cddr cons)))

(defun nothing (&rest whatever)
  (declare (ignore whatever)))

(defmacro define-symmetric (name ((a class1) (b class2)) &body forms)
  `(progn
     (defmethod ,name ((,a ,class1) (,b ,class2)) ,@forms)
     ,@(unless (equal class1 class2)
         `((defmethod ,name ((,b ,class2) (,a ,class1)) (,name ,a ,b))))
     ',name))
