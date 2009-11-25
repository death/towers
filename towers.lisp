(defpackage #:towers
  (:use #:cl))

(in-package #:towers)


;;;; Utilities

(defun rad (deg)
  (/ (* (load-time-value (coerce pi 'single-float)) deg) 180.0))

(defun deg (rad)
  (/ (* 180.0 rad) (load-time-value (coerce pi 'single-float))))

(defun normalize-deg (deg)
  (loop while (>= deg 360.0) do (decf deg 360.0))
  (loop while (< deg 0.0) do (incf deg 360.0))
  deg)

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

(defun best-element (list &key (key #'identity) (test #'<))
  (assert list)
  (let ((best (first list))
        (best-key (funcall key (first list))))
    (dolist (x (rest list))
      (let ((key (funcall key x)))
        (when (funcall test key best-key)
          (setf best x best-key key))))
    (values best best-key)))


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
  (sqrt (vec-distance-sq v1 v2)))

(defun vec-distance-sq (v1 v2)
  (+ (square (- (x v1) (x v2)))
     (square (- (y v1) (y v2)))))

(defun vec-contains (v1 v2 &optional (r 1.0))
  (vec-contains-xy v1 (x v2) (y v2) r))

(defun vec-contains-xy (v x y &optional (r 1.0))
  (and (>= x (* (- (x v)) r))
       (<= x (* (x v) r))
       (>= y (* (- (y v)) r))
       (<= y (* (y v) r))))

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

(defun vec-angle (vec)
  (deg (atan (y vec) (x vec))))


;;;; Game object protocol

(defclass collidable-object ()
  ((collision-radius :initarg :collision-radius :accessor collision-radius)
   (pos :initarg :pos :accessor pos)))

(defclass selectable-object ()
  ((collision-radius :initarg :collision-radius :accessor collision-radius)
   (pos :initarg :pos :accessor pos)))

(defgeneric update (object tick world))
(defgeneric render (object))
(defgeneric select (object op pos world))

(defmethod select ((object null) op pos world)
  (declare (ignore op pos world)))


;;;; Collision detection

(defun collides-p (a b)
  (let ((pa (pos a))
        (pb (pos b))
        (ra (collision-radius a))
        (rb (collision-radius b)))
    (close-enough-p pa ra pb rb)))

(defun close-enough-p (pa ra pb rb)
  (< (vec-distance-sq pa pb) (square (+ ra rb))))


;;;; Player

(defclass player ()
  ((cash :initarg :cash :accessor cash)))

(defmethod update ((player player) tick world)
  (declare (ignore tick world)))

(defmethod render ((player player))
  (gl:with-pushed-matrix
    (gl:color 1.0 1.0 1.0)
    (display-text -90.0 -80.0 (cash player))))

(defun try-buy (tower cost player world)
  (when (>= (cash player) cost)
    (decf (cash player) cost)
    (add-object tower world)))


;;;; Grid

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


;;;; Path

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


;;;; Message

(defclass message ()
  ((pos :initarg :pos :accessor pos)
   (color :initarg :color :accessor color)
   (text :initarg :text :accessor text)))

(defmethod update ((m message) tick world)
  (declare (ignore tick world)))

(defmethod render ((m message))
  (gl:with-pushed-matrix
    (apply #'gl:color (color m))
    (with-vec (x y (pos m))
      (display-text x y (text m)))))


;;;; Homebase

(defclass homebase (collidable-object)
  ((lives :initarg :lives :accessor lives))
  (:default-initargs :collision-radius 8))

(defmethod update ((hb homebase) tick world)
  (declare (ignore tick world)))

(defmethod render ((hb homebase))
  (gl:color 0.6 0.2 0.8)
  (gl:with-pushed-matrix
    (with-vec (x y (pos hb))
      (gl:translate x y 0)
      (draw-circle 8)
      (display-text (- x 1) (- y 1) (lives hb)))))


;;;; Towers

(defclass tower (selectable-object)
  ())

(defgeneric try-fire (tower tick world))
(defgeneric tower-projectile (tower))

(defclass shooting-tower-mixin ()
  ((last-shot-tick :initform nil :accessor last-shot-tick)
   (fire-rate :initarg :fire-rate :accessor fire-rate)
   (detection-radius :initarg :detection-radius :accessor detection-radius)
   (active :initarg :active :accessor active-p))
  (:default-initargs :active t))

(defmethod try-fire ((tower shooting-tower-mixin) tick world)
  (let ((last-shot-tick (last-shot-tick tower)))
    (when (or (null last-shot-tick)
              (>= (- tick last-shot-tick) (fire-rate tower)))
      (add-object (tower-projectile tower) world)
      (setf (last-shot-tick tower) tick))))

(defmethod render :after ((tower shooting-tower-mixin))
  (when (active-p tower)
    (gl:with-pushed-matrix
      (with-vec (x y (pos tower))
        (gl:translate x y 0))
      (gl:color 0 1 1)
      (draw-circle (detection-radius tower)))))

(defclass blaster-tower (tower shooting-tower-mixin)
  ((angle :initform 0.0 :accessor angle))
  (:default-initargs :fire-rate 2 :collision-radius 8 :detection-radius 30))

(defmethod update ((tower blaster-tower) tick world)
  (let ((enemies (detect-enemies tower world)))
    (when enemies
      (when (some (lambda (enemy) (good-to-fire-p enemy tower)) enemies)
        (try-fire tower tick world))
      (aim tower (best-element enemies :key (lambda (enemy) (target-angle enemy tower)))))))

(defun detect-enemies (tower world)
  (let ((enemies '()))
    (map-objects (lambda (enemy)
                   (when (close-enough-p (pos enemy) (collision-radius enemy)
                                         (pos tower) (detection-radius tower))
                     (push enemy enemies)))
                 world :order :hit-test :type 'enemy)
    enemies))

(defun target-angle (enemy tower)
  ;; TODO: take account of projectile velocity
  (normalize-deg (+ 270.0 (vec-angle (vec- (pos enemy) (pos tower))))))

(defun good-to-fire-p (enemy tower)
  (let ((aim-angle (angle tower))
        (target-angle (target-angle enemy tower)))
    (< (abs (- aim-angle target-angle)) 3.0)))

(defun aim (tower enemy)
  (let* ((aim-angle (angle tower))
         (target-angle (target-angle enemy tower))
         (diff (- target-angle aim-angle)))
    (when (> diff 180.0) (decf diff 360.0))
    (when (< diff -180.0) (incf diff 360.0))
    (setf (angle tower)
          (normalize-deg (+ (angle tower)
                            (max -10.0 (min diff 10.0)))))))

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

(defmethod select ((tower blaster-tower) op pos world)
  (declare (ignore op pos world))
  ;; todo
  )

(defclass projectile (collidable-object)
  ((vel :initarg :vel :accessor vel)))

(defgeneric projectile-hit (projectile enemies world))

(defmethod update ((proj projectile) tick world)
  (declare (ignore tick))
  (vec+= (pos proj) (vel proj))
  (unless (vec-contains (dim world) (pos proj))
    (remove-object proj world)
    (return-from update))
  (let ((enemies-hit '()))
    (map-objects (lambda (enemy)
                   (when (collides-p proj enemy)
                     (push enemy enemies-hit)))
                 world :order :hit-test :type 'enemy)
    (when enemies-hit
      (projectile-hit proj enemies-hit world))))

(defclass blaster-projectile (projectile)
  ((damage :initarg :damage :accessor damage))
  (:default-initargs :collision-radius 1))

(defmethod tower-projectile ((tower blaster-tower))
  (let ((vel (vel-vec 2.0 (- (angle tower)))))
    (make-instance 'blaster-projectile
                   :pos (vec+= (vec* vel 5.0) (pos tower))
                   :vel vel
                   :damage 1)))

(defmethod render ((proj blaster-projectile))
  (gl:with-pushed-matrix
    (with-vec (x y (pos proj))
      (gl:translate x y 0))
    (gl:color 0 1 0)
    (draw-circle 1)))

(defmethod projectile-hit ((proj blaster-projectile) enemies world)
  (dolist (enemy enemies)
    (when (<= (decf (hit-points enemy) (damage proj)) 0)
      (remove-object enemy world)))
  (remove-object proj world))

(defclass tower-factory (selectable-object)
  ((cost :initarg :cost :accessor cost)
   (kind :initarg :kind :accessor kind)
   (prototype :accessor prototype)
   (new-tower :initform nil :accessor new-tower)))

(defmethod initialize-instance :after ((factory tower-factory) &rest initargs)
  (declare (ignore initargs))
  (setf (prototype factory)
        (make-instance (kind factory) :pos (pos factory) :active nil))
  (setf (collision-radius factory)
        (collision-radius (prototype factory))))

(defmethod update ((factory tower-factory) tick world)
  (declare (ignore tick world)))

(defmethod render ((factory tower-factory))
  (render (prototype factory))
  (when (new-tower factory)
    (render (new-tower factory)))
  (with-vec (x y (pos factory))
    (gl:color 1.0 1.0 1.0)
    (display-text (- x 2.0) (- y 10.0) (cost factory))))

(defmethod select ((factory tower-factory) op pos world)
  (ecase op
    (:obtain
     (setf (new-tower factory)
           (make-instance (kind factory) :pos (copy-vec pos) :active nil))
     (setf (active-p (new-tower factory)) t))
    (:release
     (try-buy (new-tower factory) (cost factory) (player world) world)
     (setf (new-tower factory) nil))
    (:move
     (let ((new (new-tower factory)))
       (when new
         (with-vec (sx sy pos)
           (with-vec (tx ty (pos new) t)
             (setf tx sx)
             (setf ty sy))))))))
                             

;;;; Enemies

(defclass enemy (collidable-object)
  ((spd :initarg :speed :accessor spd)
   (path :initarg :path :accessor path)
   (next-pos-idx :initform 0 :accessor next-pos-idx)
   (hit-points :initarg :hit-points :accessor hit-points)))

(defmethod update ((e enemy) tick world)
  (declare (ignore tick))
  (let* ((pos (pos e))
         (vertices (vertices (path e)))
         (next-pos (aref vertices (next-pos-idx e))))
    (map-objects (lambda (hb)
                   (when (collides-p e hb)
                     (enemy-suicide e world)
                     (when (= (decf (lives hb)) 0)
                       (game-over world))
                     (return-from update)))
                 world :order :hit-test :type 'homebase)
    (when (vec=~ pos next-pos (spd e))
      (incf (next-pos-idx e))
      (when (= (next-pos-idx e) (length vertices))
        (enemy-suicide e world)
        (return-from update))
      (setf next-pos (aref vertices (next-pos-idx e))))
    (vec+= pos (vel-vec (spd e) (vec- next-pos pos)))))

(defun enemy-suicide (enemy world)
  (remove-object enemy world))

(defun game-over (world)
  (add-object
   (make-instance 'message
                  :pos (vec -14.0 0.0)
                  :color '(1 0 0)
                  :text "GAME OVER")
   world))
                  
(defclass sqrewy (enemy)
  ((angle :initform 0 :accessor angle)
   (dir :initform '> :accessor dir))
  (:default-initargs :collision-radius 2))

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
  ((objects :initform (make-array 6 :initial-element '()) :accessor objects)
   (dim :initform (vec 100.0 100.0) :accessor dim)))

(defun make-world ()
  (make-instance 'world))

(defun add-object (object world)
  (push object (aref (objects world) (object-list-index object))))

(defun object-list-index (object)
  (typecase object
    (player 5)
    (message 4)
    (projectile 3)
    (enemy 2)
    (tower 1)
    (t 0)))

(defun remove-object (object world)
  (alexandria:deletef
   (aref (objects world) (object-list-index object))
   object :count 1))

(defun map-objects (function world &key order (type t))
  (unless (alexandria:type= type 'nil)
    (flet ((maybe-call-function (object)
             (when (typep object type)
               (funcall function object))))
      (ecase order
        ((:render :update :hit-test nil)
         (loop for list across (objects world) do
               (mapc #'maybe-call-function list)))))))

(defmethod update ((w world) tick world)
  (declare (ignore world))
  (map-objects (lambda (object)
                 (update object tick w))
               w :order :update))

(defparameter *draw-collision-circle-for-type*
  'nil)

(defmethod render ((w world))
  (map-objects #'render w :order :render)
  (map-objects
   (lambda (object)
     (gl:with-pushed-matrix
       (with-vec (x y (pos object))
         (gl:translate x y 0))
       (gl:color 1 0 0)
       (draw-circle (collision-radius object))))
   w :order :render :type *draw-collision-circle-for-type*))

(defun player (world)
  (first (aref (objects world) 5)))


;;;; Levels

(defun make-level-1-world ()
  (let ((world (make-instance 'world))
        (path (make-instance 'path :vertices #((0.0 . 100.0)
                                               (0.0 . 0.0)
                                               (-50.0 . -50.0)))))
    (add-object (make-instance 'player :cash 10) world)
    (add-object (make-instance 'homebase :lives 2 :pos (vec -50.0 -50.0)) world)
    (add-object (make-instance 'blaster-tower :pos (vec -20.0 -5.0)) world)
    (add-object (make-instance 'blaster-tower :pos (vec 15.0 50.0)) world)
    (add-object (make-instance 'tower-factory :kind 'blaster-tower :pos (vec -60.0 -85.0) :cost 5) world)
    (add-object
     (make-instance
      'wave
      :start-tick 100
      :wait-ticks 50
      :enemies (loop repeat 3 collecting
                     (make-instance 'sqrewy
                                    :pos (vec 0.0 100.0)
                                    :speed 0.8
                                    :path path
                                    :hit-points 1)))
     world)
    (add-object
     (make-instance
      'wave
      :start-tick 500
      :wait-ticks 20
      :enemies (loop repeat 4 collecting
                     (make-instance 'sqrewy
                                    :pos (vec 0.0 100.0)
                                    :speed 1.0
                                    :path path
                                    :hit-points 4)))
     world)
    (add-object path world)
    (add-object (make-instance 'grid) world)
    world))


;;;; Game window

(defclass mouse ()
  ((pos :initform (vec 0.0 0.0) :accessor pos)
   (selection :initform nil :accessor selection)))

(defmethod collision-radius ((m mouse)) 2)

(defun pick-object (mouse world)
  (map-objects
   (lambda (object)
     (when (collides-p object mouse)
       (return-from pick-object object)))
   world :order :hit-test :type 'selectable-object))

(defclass game-window (glut:window)
  ((world :initarg :world :accessor world)
   (time-to-next-tick :initform nil :accessor time-to-next-tick)
   (tick :initform nil :accessor tick)
   (mouse :initform (make-instance 'mouse) :accessor mouse))
  (:default-initargs
   :width 500 :height 500
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

(defmethod glut:motion ((w game-window) x y)
  (multiple-value-bind (x y)
      (glu:un-project x y 0.0)
    (with-vec (mx my (pos (mouse w)) t)
      (setf mx x)
      (setf my (- y))))
  (select (selection (mouse w)) :move (pos (mouse w)) (world w)))

(defmethod glut:mouse ((w game-window) button state x y)
  (glut:motion w x y)
  (case button
    (:left-button
     (let ((m (mouse w)))
       (case state
         (:down
          (setf (selection m) (pick-object m (world w)))
          (select (selection m) :obtain (pos m) (world w)))
         (:up
          (select (selection m) :release (pos m) (world w))
          (setf (selection m) nil)))))))

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
  (gl:with-pushed-matrix
    (gl:load-identity)
    (gl:raster-pos x y)
    (glut:bitmap-string (cffi:make-pointer 3) object)))


;;;; Game

(defun game ()
  (glut:display-window
   (make-instance 'game-window :world (make-level-1-world))))
