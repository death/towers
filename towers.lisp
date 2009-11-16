(defpackage #:towers
  (:use #:cl))

(in-package #:towers)


;;;; Utilities

(defmacro with-pushed-matrix-in-mode (mode &body forms)
  (alexandria:once-only (mode)
    `(prog2
         (progn
           (gl:matrix-mode ,mode)
           (gl:push-matrix))
         (progn ,@forms)
       (progn
         (gl:matrix-mode ,mode)
         (gl:pop-matrix)))))


;;;; Game object protocol

(defgeneric update (object tick world))
(defgeneric render (object))


;;;; Grid object

(defclass grid ()
  ())

(defmethod update ((grid grid) tick world)
  (declare (ignore tick world)))

(defmethod render ((grid grid))
  (gl:with-pushed-matrix
    (gl:color 0.2 0.2 0.2)
    (gl:translate -100 100 0)
    (gl:scale 1 -1 1)
    (gl:with-primitive :lines
      (dotimes (y 18)
        (gl:vertex 0 (* y 10))
        (gl:vertex 200 (* y 10)))
      (dotimes (x 20)
        (gl:vertex (* x 10) 0)
        (gl:vertex (* x 10) 170)))))


;;;; Path object

(defclass path ()
  ((vertices :initarg :vertices :accessor vertices)))

(defmethod update ((path path) tick world)
  (declare (ignore tick world)))

(defmethod render ((path path))
  (gl:color 0 0 0.6)
  (gl:with-primitive :line-strip
    (loop for (x y) in (vertices path) do
          (gl:vertex x y))))



(defclass stupid ()
  ((angle :initform 0 :accessor angle)))

(defmethod update ((object stupid) tick world)
  (declare (ignore tick world))
  (when (>= (incf (angle object) 5) 360)
    (setf (angle object) 0)))

(defmethod render ((object stupid))
  (gl:color 1 1 1)
  (display-text 10 15 "Hello stupid")
  (gl:with-pushed-matrix
    (gl:color 1 0 0)
    (gl:rotate (angle object) 0 0 1)
    (gl:with-primitive :line-loop
      (gl:vertex -50 50)
      (gl:vertex 50 50)
      (gl:vertex 50 -50)
      (gl:vertex -50 -50))))


;;;; Game world

(defclass world ()
  ((objects :initform '() :accessor objects)))

(defun make-world ()
  (make-instance 'world))

;;; Later on we'll have multiple containers, one for each object type.
;;; This will help in ordering the objects according to their types.

(defun add-object (object world)
  (push object (objects world)))

(defun map-objects (function world &key order)
  (ecase order
    ((:render :update :hit-test nil)
     (dolist (object (objects world))
       (funcall function object)))))

(defmethod update ((w world) tick world)
  (declare (ignore world))
  (map-objects (lambda (object)
                 (update object tick w))
               w
               :order :update))

(defmethod render ((w world))
  (map-objects #'render w :order :render))


;;;; Levels

(defun make-level-1-world ()
  (let ((world (make-instance 'world)))
    (add-object (make-instance 'path :vertices '((0 100) (0 0) (-50 -50))) world)
    (add-object (make-instance 'stupid) world)
    (add-object (make-instance 'grid) world)
    world))


;;;; Game window

(defparameter *window-width* 500)
(defparameter *window-height* 500)

(defclass game-window (glut:window)
  ((world :initarg :world :accessor world)
   (time-to-next-tick :initform nil :accessor time-to-next-tick)
   (tick :initform nil :accessor tick))
  (:default-initargs
   :width *window-width* :height *window-height*
   :title "Game"
   :mode '(:double :rgb)))

(defmethod glut:display-window :before ((w game-window))
  (gl:clear-color 0 0 0 0)
  (gl:shade-model :flat)
  (gl:disable :depth-test))

(defmethod glut:display ((w game-window))
  (gl:load-identity)
  (gl:clear-color 0 0 0 0)
  (gl:clear :color-buffer)
  (render (world w))
  (glut:swap-buffers))

(defmethod glut:reshape ((w game-window) width height)
  (gl:viewport 0 0 width height)
  (gl:matrix-mode :projection)
  (gl:load-identity)
  (gl:ortho -100 100 -100 100 0 1)
  (gl:matrix-mode :modelview))

(defmethod glut:keyboard ((w game-window) key x y)
  (declare (ignore x y))
  (case key
    (#\Esc (glut:destroy-current-window))))

(defparameter *frames-per-second* 30)
(defparameter *tick-duration* (floor 1000 *frames-per-second*))

(defmethod glut:idle ((w game-window))
  (let ((now (glut:get :elapsed-time)))
    (when (null (tick w))
      (setf (tick w) -1)
      (setf (time-to-next-tick w) now))
    (when (>= now (time-to-next-tick w))
      (incf (tick w))
      (setf (time-to-next-tick w) (+ now *tick-duration*))
      (update (world w) (tick w) (world w))
      (glut:post-redisplay))))

(defun display-text (x y object)
  (unless (stringp object)
    (setf object (princ-to-string object)))
  (with-pushed-matrix-in-mode :projection
    (gl:load-identity)
    (glu:ortho-2d 0 *window-width* 0 *window-height*)
    (gl:scale 1 -1 1)
    (gl:translate 0 (- *window-height*) 0)
    (with-pushed-matrix-in-mode :modelview
      (gl:load-identity)
      (gl:raster-pos x y)
      (glut:bitmap-string (cffi:make-pointer 3) object)))
  (gl:matrix-mode :modelview))


;;;; Game

(defun game ()
  (glut:display-window
   (make-instance 'game-window :world (make-level-1-world))))
