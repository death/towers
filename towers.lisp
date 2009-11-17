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

(defun rad (deg)
  (/ (* (load-time-value (coerce pi 'single-float)) deg) 180.0))

(defun deg (rad)
  (/ (* 180.0 rad) (load-time-value (coerce pi 'single-float))))

(defun sind (deg)
  (sin (rad deg)))

(defun cosd (deg)
  (cos (rad deg)))

(defun draw-circle (radius)
  (gl:with-primitive :line-loop
    (loop for angle from 0 below 360 by 5
          for x = (* radius (sind angle))
          for y = (* radius (cosd angle))
          do (gl:vertex x y))))

(defun square (x)
  (* x x))


;;;; 2D vectors

(defun vec (x y) (cons x y))

(defun x (vec) (car vec))

(defun y (vec) (cdr vec))

(defun (setf x) (new-x vec)
  (setf (car vec) new-x))

(defun (setf y) (new-y vec)
  (setf (cdr vec) new-y))

(defun copy-vec (v)
  (vec (x v) (y v)))

(defun unit (&optional (dir 0.0))
  (if (consp dir)
      (vec/ dir (vec-mag dir))
      (vec (sind dir) (cosd dir))))

(defun vec-clear (vec)
  (setf (x vec) 0.0)
  (setf (y vec) 0.0)
  vec)

(defun vec-mul (v1 v2)
  (+ (* (x v1) (x v2))
     (* (y v1) (y v2))))

(defun vec+ (v1 v2)
  (vec (+ (x v1) (x v2))
       (+ (y v1) (y v2))))

(defun vec+= (v1 v2)
  (incf (x v1) (x v2))
  (incf (y v1) (y v2))
  v1)

(defun vec- (v1 v2)
  (vec (- (x v1) (x v2))
       (- (y v1) (y v2))))

(defun vec-= (v1 v2)
  (decf (x v1) (x v2))
  (decf (y v1) (y v2))
  v1)

(defun vec* (v a)
  (vec (* (x v) a)
       (* (y v) a)))

(defun vec*= (v a)
  (setf (x v) (* (x v) a))
  (setf (y v) (* (y v) a))
  v)

(defun vec/ (v a)
  (vec (/ (x v) a)
       (/ (y v) a)))

(defun vec/= (v a)
  (setf (x v) (/ (x v) a))
  (setf (y v) (/ (y v) a))
  v)

(defun vec-mag (v)
  (sqrt (+ (square (x v)) (square (y v)))))

(defun vec-distance (v1 v2)
  (vec-distance-xy v1 (x v2) (y v2)))

(defun vec-distance-xy (v x y)
  (let ((ax (- (x v) x))
        (ay (- (y v) y)))
    (if (> ax ay)
        (+ ax (/ ay 2.0))
        (+ ay (/ ax 2.0)))))

(defun vec-distance-acc (v1 v2)
  (sqrt (+ (- (x v1) (x v2))
           (- (y v1) (y v2)))))

(defun vec-contains (v1 v2 &optional (r 1.0))
  (vec-contains-xy v1 (x v2) (y v2) r))

(defun vec-contains-xy (v x y &optional (r 1.0))
  (and (>= x (* (- (x v)) r))
       (<= x (* (x v) r))
       (>= y (* (- (y v)) r))
       (<= y (* (y v) r))))

(defun vec-roll (v d)
  (let ((tx (- (* (x v) (cos d))
               (* (y v) (sin d)))))
    (vec tx (+ (* (x v) (sin d))
               (* (y v) (cos d))))))

(defmacro with-vec ((x y vec &optional (update nil)) &body forms)
  (alexandria:once-only (vec)
    `(,(if update
           'symbol-macrolet
           'let)
       ((,x (car ,vec))
        (,y (cdr ,vec)))
       ,@forms)))

(defun vec=~ (v1 v2 &optional (epsilon 0.1))
  (flet ((=~ (a b) (< (abs (- a b)) epsilon)))
    (and (=~ (x v1) (x v2))
         (=~ (y v1) (y v2)))))

(defun vel-vec (mag dir)
  (vec*= (unit dir) mag))


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
    (loop for v across (vertices path) do
          (with-vec (x y v)
            (gl:vertex x y)))))


;;;; Towers

(defclass tower ()
  ((pos :initarg :pos :accessor pos)))

(defgeneric try-fire (tower tick world))
(defgeneric tower-projectile (tower))

(defclass shooting-tower-mixin ()
  ((last-shot-tick :initform nil :accessor last-shot-tick)
   (fire-rate :initarg :fire-rate :accessor fire-rate)))

(defmethod try-fire ((tower shooting-tower-mixin) tick world)
  (let ((last-shot-tick (last-shot-tick tower)))
    (when (or (null last-shot-tick)
              (>= (- tick last-shot-tick) (fire-rate tower)))
      (add-object (tower-projectile tower) world)
      (setf (last-shot-tick tower) tick))))

(defclass blaster-tower (tower shooting-tower-mixin)
  ((angle :initform 0.0 :accessor angle))
  (:default-initargs :fire-rate 20))

(defmethod update ((tower blaster-tower) tick world)
  (when (>= (incf (angle tower)) 360.0)
    (setf (angle tower) 0.0))
  (try-fire tower tick world))

(defmethod render ((tower blaster-tower))
  (gl:with-pushed-matrix
    (with-vec (x y (pos tower))
      (gl:translate x y 0))
    (gl:color 0 1 0)
    (draw-circle 5)
    (gl:rotate (angle tower) 0 0 1)
    (draw-circle 3)
    (gl:with-primitive :line-loop
      (gl:vertex -1 0)
      (gl:vertex 1 0)
      (gl:vertex 1 8)
      (gl:vertex -1 8))))

(defclass projectile ()
  ((pos :initarg :pos :accessor pos)
   (vel :initarg :vel :accessor vel)))

(defmethod update ((proj projectile) tick world)
  (declare (ignore tick))
  (vec+= (pos proj) (vel proj))
  (unless (vec-contains (dim world) (pos proj))
    (remove-object proj world)))

(defclass blaster-projectile (projectile)
  ())

(defmethod tower-projectile ((tower blaster-tower))
  (let ((vel (vel-vec 1.0 (- (angle tower)))))
    (make-instance 'blaster-projectile
                   :pos (vec+= (vec* vel 5.0) (pos tower))
                   :vel vel)))

(defmethod render ((proj blaster-projectile))
  (gl:with-pushed-matrix
    (with-vec (x y (pos proj))
      (gl:translate x y 0))
    (gl:color 0 1 0)
    (draw-circle 1)))


;;;; Enemies

(defclass enemy ()
  ((pos :initarg :pos :accessor pos)
   (spd :initarg :speed :accessor spd)
   (path :initarg :path :accessor path)
   (next-pos-idx :initform 0 :accessor next-pos-idx)))

(defmethod update ((e enemy) tick world)
  (declare (ignore tick))
  (let* ((pos (pos e))
         (vertices (vertices (path e)))
         (next-pos (aref vertices (next-pos-idx e))))
    (when (vec=~ pos next-pos (spd e))
      (incf (next-pos-idx e))
      (when (= (next-pos-idx e) (length vertices))
        (enemy-suicide e world)
        (return-from update))
      (setf next-pos (aref vertices (next-pos-idx e))))
    (vec+= pos (vel-vec (spd e) (vec- next-pos pos)))))

(defclass sqrewy (enemy)
  ((angle :initform 0 :accessor angle)
   (dir :initform '> :accessor dir)))

(defmethod update :after ((sq sqrewy) tick world)
  (declare (ignore tick world))
  (ecase (dir sq)
    (>
     (if (> (angle sq) 30)
         (setf (dir sq) '<)
         (incf (angle sq) 2)))
    (<
     (if (< (angle sq) -30)
         (setf (dir sq) '>)
         (decf (angle sq) 2)))))

(defmethod render ((sq sqrewy))
  (gl:with-pushed-matrix
    (with-vec (x y (pos sq))
      (gl:translate x y 0))
    (gl:rotate (angle sq) 0 0 1)
    (gl:color 0 0.5 0.5)
    (gl:with-primitive :line-loop
      (gl:vertex -2 -2)
      (gl:vertex 2 -2)
      (gl:vertex 2 2)
      (gl:vertex -2 2))))

(defun enemy-suicide (enemy world)
  (remove-object enemy world))



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


;;;; Waves

(defclass wave ()
  ((enemies :initarg :enemies :accessor enemies)
   (start-tick :initarg :start-tick :accessor start-tick)
   (wait-ticks :initarg :wait-ticks :accessor wait-ticks)
   (last-release-tick :initform nil :accessor last-release-tick)))

(defmethod update ((w wave) tick world)
  (when (>= tick (start-tick w))
    (cond ((null (enemies w))
           (remove-object w world))
          ((or (null (last-release-tick w))
               (>= (- tick (last-release-tick w)) (wait-ticks w)))
           (release-an-enemy w world)
           (setf (last-release-tick w) tick)))))

(defmethod render ((w wave)))  

(defun release-an-enemy (wave world)
  (add-object (pop (enemies wave)) world))


;;;; Game world

(defclass world ()
  ((projectile-objects :initform '() :accessor projectile-objects)
   (tower-objects :initform '() :accessor tower-objects)
   (other-objects :initform '() :accessor other-objects)
   (dim :initform (vec 100.0 100.0) :accessor dim)))

(defun make-world ()
  (make-instance 'world))

(defun add-object (object world)
  (typecase object
    (projectile (push object (projectile-objects world)))
    (tower (push object (tower-objects world)))
    (t (push object (other-objects world)))))

(defun remove-object (object world)
  (typecase object
    (projectile (alexandria:deletef (projectile-objects world) object :count 1))
    (tower (alexandria:deletef (tower-objects world) object :count 1))
    (t (alexandria:deletef (other-objects world) object :count 1))))

(defun map-objects (function world &key order)
  (ecase order
    ((:render :update :hit-test nil)
     (mapc function (other-objects world))
     (mapc function (tower-objects world))
     (mapc function (projectile-objects world)))))

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
  (let ((world (make-instance 'world))
        (path (make-instance 'path :vertices #((0.0 . 100.0)
                                               (0.0 . 0.0)
                                               (-50.0 . -50.0)))))
    (add-object (make-instance 'blaster-tower :pos (vec 0.0 0.0)) world)
    (add-object (make-instance 'blaster-tower :pos (vec 20.0 20.0)) world)
    (add-object
     (make-instance
      'wave
      :start-tick 100
      :wait-ticks 50
      :enemies (list (make-instance 'sqrewy :pos (vec 0.0 100.0) :speed 1.0 :path path)
                     (make-instance 'sqrewy :pos (vec 0.0 100.0) :speed 1.0 :path path)
                     (make-instance 'sqrewy :pos (vec 0.0 100.0) :speed 1.0 :path path)))
     world)
    (add-object
     (make-instance
      'wave
      :start-tick 500
      :wait-ticks 20
      :enemies (list (make-instance 'sqrewy :pos (vec 0.0 100.0) :speed 1.5 :path path)
                     (make-instance 'sqrewy :pos (vec 0.0 100.0) :speed 1.5 :path path)
                     (make-instance 'sqrewy :pos (vec 0.0 100.0) :speed 1.5 :path path)
                     (make-instance 'sqrewy :pos (vec 0.0 100.0) :speed 1.5 :path path)))
     world)
    (add-object path world)
    (add-object (make-instance 'stupid) world)
    (add-object (make-instance 'grid) world)
    world))


;;;; Game window

(defvar *window-width* 500)
(defvar *window-height* 500)

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
  (gl:translate .375 .375 0)
  (render (world w))
  (glut:swap-buffers))

(defmethod glut:reshape ((w game-window) width height)
  (setf *window-width* width)
  (setf *window-height* height)
  (gl:viewport 0 0 width height)
  (gl:matrix-mode :projection)
  (gl:load-identity)
  (with-vec (x y (dim (world w)))
    (gl:ortho (- x) x (- y) y 0 1))
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
