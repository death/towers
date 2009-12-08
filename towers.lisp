(defpackage #:towers
  (:use #:cl #:alexandria))

(in-package #:towers)


;;;; Configuration

(defparameter *frames-per-second* 30)
(defparameter *tick-duration* (floor 1000 *frames-per-second*))
(defparameter *draw-collision-circle-for-type* 'nil)


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

(defun draw-circle (radius &optional (resolution 30) (filledp nil))
  ;; http://github.com/sykopomp/until-it-dies/blob/master/src/primitives.lisp
  ;; Stolen implementation and modified it a bit
  (let* ((theta (* 2.0 (/ single-pi resolution)))
         (tangential-factor (tan theta))
         (radial-factor (- 1.0 (cos theta))))
    (gl:with-primitives (if filledp :triangle-fan :line-loop)
      (loop with x = radius
            with y = 0.0
            repeat resolution
            do (gl:vertex x y)
            (let ((tx (- y))
                  (ty x))
              (incf x (* tx tangential-factor))
              (incf y (* ty tangential-factor)))
            (let ((rx (- x))
                  (ry (- y)))
              (incf x (* rx radial-factor))
              (incf y (* ry radial-factor)))))))

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
        (let ((instructions '()))
          (call-with-curve-multipliers
           (lambda (am bm cm dm)
             (push
              `(gl:vertex (+ (* ,am ,ax) (* ,bm ,bx) (* ,cm ,cx) (* ,dm ,dx))
                          (+ (* ,am ,ay) (* ,bm ,by) (* ,cm ,cy) (* ,dm ,dy)))
              instructions))
           segments)
          `(progn (nreverse ,@instructions))))
      form))

(defun draw-cubic-curve (ax ay bx by cx cy dx dy &optional (segments 20))
  (call-with-curve-multipliers
   (lambda (am bm cm dm)
     (gl:vertex (+ (* am ax) (* bm bx) (* cm cx) (* dm dx))
                (+ (* am ay) (* bm by) (* cm cy) (* dm dy))))
   segments))

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

(defun vec-assign (v x y)
  (setf (x v) x)
  (setf (y v) y)
  v)

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
  (once-only (vec)
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


;;;; Wavefront objects

(defclass wf-object ()
  ((coordinates :initarg :coordinates :accessor coordinates)
   (faces :initarg :faces :accessor faces)
   (parts :initarg :parts :accessor parts)))

(defun load-wf-object (filename)
  (with-open-file (in filename :direction :input)
    (let ((coordinates (make-array 0 :adjustable t :fill-pointer t :element-type 'single-float))
          (faces (make-array 0 :adjustable t :fill-pointer t :element-type 'fixnum))
          (parts (make-hash-table))
          (part-start 0)
          (part-end 0)
          (part-name nil))
      (flet ((sexp-stream (string)
               (make-concatenated-stream
                (make-string-input-stream "(")
                (make-string-input-stream string)
                (make-string-input-stream ")"))))
        (loop for line = (read-line in nil nil)
              while line do
              (let ((line (string-trim '(#\Space #\Tab #\Return) line)))
                (cond ((emptyp line))
                      ((starts-with #\# line))
                      (t (let ((sexp (read (sexp-stream line))))
                           (case (car sexp)
                             (v (dolist (x (cdr sexp))
                                  (vector-push-extend x coordinates)))
                             (f (dolist (x (cdr sexp))
                                  (vector-push-extend (1- x) faces))
                                (incf part-end))
                             (o (cond ((null part-name)
                                       (setf part-name (cadr sexp)))
                                      (t
                                       (setf (gethash part-name parts)
                                             (list part-start part-end))
                                       (setf part-name (cadr sexp))
                                       (setf part-start part-end))))
                             (g)
                             (t (warn "Unexpected first element: ~S" (car sexp))))))))))
      (when part-name
        (setf (gethash part-name parts)
              (list part-start part-end)))
      (make-instance 'wf-object :coordinates coordinates :faces faces :parts parts))))

(defun wf-part-names (wf-object)
  (hash-table-keys (parts wf-object)))

(defun wf-draw (wf-object)
  (gl:with-primitive :triangles
    (let ((coords (coordinates wf-object))
          (faces (faces wf-object)))
      (loop for face across faces
            do (gl:vertex (aref coords (+ 0 (* 3 face)))
                          (- (aref coords (+ 2 (* 3 face))))
                          (aref coords (+ 1 (* 3 face))))))))

(defun wf-draw-part (part-name wf-object)
  (gl:with-primitive :triangles
    (let ((coords (coordinates wf-object))
          (faces (faces wf-object)))
      (destructuring-bind (start end)
          (gethash part-name (parts wf-object))
        (loop for i from (* 3 start) below (* end 3)
              for face = (aref faces i)
              do (gl:vertex (aref coords (+ 0 (* 3 face)))
                            (- (aref coords (+ 2 (* 3 face))))
                            (aref coords (+ 1 (* 3 face)))))))))

(defparameter *wf-object-repository* (make-hash-table))

(defun register-wf-object (name filename)
  (setf (gethash name *wf-object-repository*) (load-wf-object filename)))

(defun find-wf-object (name)
  (or (gethash name *wf-object-repository*)
      (error "Can't find Wavefront object ~S." name)))


;;;; Collision detection

(defclass collidable-object ()
  ())

(defgeneric collide-p (a b))

(defmethod collide-p :around (a b)
  (or (eql a b)
      (call-next-method)))

(defclass circle-collidable-object ()
  ((collision-radius :initarg :collision-radius :accessor collision-radius)
   (pos :initarg :pos :accessor pos)))

(defun close-enough-p (pa ra pb rb)
  (< (vec-distance-sq pa pb) (square (+ ra rb))))

(defmethod collide-p ((a circle-collidable-object)
                      (b circle-collidable-object))
  (close-enough-p (pos a) (collision-radius a)
                  (pos b) (collision-radius b)))


;;;; Game object protocol

(defclass pickable-object (collidable-object)
  ())

(defclass draggable-object (pickable-object)
  ())

(defclass selectable-object (pickable-object)
  ())

(defclass clickable-object (pickable-object)
  ())

(defgeneric update (object tick world)
  (:method (object tick world)
    (declare (ignore object tick world))))

(defgeneric render (object))

(defgeneric select (object op pos world)
  (:method ((object null) op pos world)
    (declare (ignore op pos world))))


;;;; Player

(defclass player ()
  ((cash :initarg :cash :accessor cash)))

(defmethod render ((player player))
  (gl:with-pushed-matrix
    (gl:color 1.0 1.0 1.0)
    (display-text -90.0 -80.0 (cash player))))

(defun try-buy (tower player world)
  (when (>= (cash player) (buy-price tower))
    (decf (cash player) (buy-price tower))
    (incf (level tower))
    (add-object tower world)))

(defun try-upgrade (tower player world)
  (declare (ignore world))
  (when (and (< (level tower) (max-level tower))
             (>= (cash player) (buy-price tower)))
    (decf (cash player) (buy-price tower))
    (incf (level tower))))

(defun sell (tower player world)
  (incf (cash player) (sell-price tower))
  (setf (tower (tower-control world)) nil)
  (remove-object tower world))


;;;; Tower control

(defclass tower-control (clickable-object circle-collidable-object)
  ((tower :initarg :tower :accessor tower))
  (:default-initargs :tower nil :collision-radius 16 :pos (vec 62.0 -82.0)))

(defmethod render ((control tower-control))
  (when-let (tower (tower control))
    (gl:with-pushed-matrix
      (gl:color 0.2 0.5 1)
      (display-text 50.0 -75.0 (type-of tower))
      (display-text 50.0 -80.0 (format nil "Level ~D" (level tower)))
      (display-text 50.0 -85.0 "Upgrade")
      (display-text 50.0 -90.0 "Sell"))))

(defmethod select ((control tower-control) op pos world)
  (ecase op
    (:obtain
     (when-let (tower (tower control))
       (with-vec (x y pos)
         (cond ((and (>= x 50.0) (>= y -86.0) (<= y -81.0))
                (try-upgrade tower (player world) world))
               ((and (>= x 50.0) (>= y -91.0) (<= y -86.0))
                (sell tower (player world) world))))))
    (:release)
    (:move)))


;;;; Grid

(defclass grid ()
  ())

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

(defclass path (collidable-object)
  ((vertices :initarg :vertices :accessor vertices)))

(defmethod initialize-instance :after ((path path) &rest initargs &key spline &allow-other-keys)
  (declare (ignore initargs))
  (when spline
    (setf (vertices path) (compile-path spline))))

(defmethod render ((path path))
  (gl:color 0 0 0.6)
  (gl:with-primitive :line-strip
    (loop for v across (vertices path) do
          (with-vec (x y v)
            (gl:vertex x y)))))

(defun compile-path (spline)
  (coerce
   (loop for (ax ay bx by cx cy dx dy) on spline by #'cddddddr
         when (and ax ay bx by cx cy dx dy)
         nconc (let ((vs '()))
                 (call-with-curve-multipliers
                  (lambda (am bm cm dm)
                   (push (vec (+ (* am ax) (* bm bx) (* cm cx) (* dm dx))
                              (+ (* am ay) (* bm by) (* cm cy) (* dm dy)))
                         vs)))
                 (nreverse vs)))
   'vector))

(defparameter *path-collision-radius* 5)

(defmethod collide-p ((a path) (b path))
  (some (lambda (va)
          (some (lambda (vb)
                  (close-enough-p va *path-collision-radius*
                                  vb *path-collision-radius*))
                (vertices b)))
        (vertices a)))

(defmethod collide-p ((a path) (b circle-collidable-object))
  (collide-p b a))

(defmethod collide-p ((a circle-collidable-object) (b path))
  (some (lambda (v)
          (close-enough-p (pos a) (collision-radius a)
                          v *path-collision-radius*))
        (vertices b)))


;;;; Message

(defclass message ()
  ((pos :initarg :pos :accessor pos)
   (color :initarg :color :accessor color)
   (text :initarg :text :accessor text)))

(defmethod render ((m message))
  (gl:with-pushed-matrix
    (apply #'gl:color (color m))
    (with-vec (x y (pos m))
      (display-text x y (text m)))))


;;;; Homebase

(register-wf-object 'homebase "c:/dev/prj/towers/data/homebase.obj")

(defclass homebase (circle-collidable-object)
  ((lives :initarg :lives :accessor lives)
   (angle :initform 0.0 :accessor angle))
  (:default-initargs :collision-radius 8))

(defmethod update ((hb homebase) tick world)
  (declare (ignore tick world))
  (incf (angle hb) 2))

(defmethod render ((hb homebase))
  (gl:color 0.6 0.2 0.8)
  (gl:with-pushed-matrix
    (with-vec (x y (pos hb))
      (gl:translate x y 0)
      (gl:scale 1.5 1.5 1.0)
      (gl:polygon-mode :front-and-back :line)
      (gl:rotate (angle hb) 0.0 0.0 1.0)
      (wf-draw (find-wf-object 'homebase))
      (gl:polygon-mode :front-and-back :fill)
      (display-text (- x 0.5) (- y 0.5) (lives hb)))))


;;;; Towers

(defclass tower (selectable-object circle-collidable-object)
  ((level :initarg :level :accessor level)
   (factory :initarg :factory :accessor tower-factory))
  (:default-initargs :level 0))

(defgeneric try-fire (tower tick world))
(defgeneric tower-projectile (tower))
(defgeneric fire-rate (tower))

(defgeneric buy-price (object))
(defgeneric sell-price (object))
(defgeneric max-level (object))

(defmethod buy-price ((tower tower))
  (aref (buy-prices (tower-factory tower)) (level tower)))

(defmethod sell-price ((tower tower))
  (aref (sell-prices (tower-factory tower)) (level tower)))

(defmethod max-level ((tower tower))
  (length (buy-prices (tower-factory tower))))

(defclass shooting-tower-mixin ()
  ((last-shot-tick :initform nil :accessor last-shot-tick)
   (fire-rate :initarg :base-fire-rate :accessor base-fire-rate)
   (detection-radius :initarg :detection-radius :accessor detection-radius)
   (draw-detection-circle :initarg :draw-detection-circle :accessor draw-detection-circle-p)
   (detection-circle-color :initform :red :accessor detection-circle-color))
  (:default-initargs :draw-detection-circle nil))

(defmethod fire-rate ((tower shooting-tower-mixin))
  (* (base-fire-rate tower) (level tower)))

(defmethod try-fire ((tower shooting-tower-mixin) tick world)
  (let ((last-shot-tick (last-shot-tick tower)))
    (when (or (null last-shot-tick)
              (>= (- tick last-shot-tick) (floor *frames-per-second* (fire-rate tower))))
      (add-object (tower-projectile tower) world)
      (setf (last-shot-tick tower) tick))))

(defmethod render :after ((tower shooting-tower-mixin))
  (when (draw-detection-circle-p tower)
    (gl:with-pushed-matrix
      (with-vec (x y (pos tower))
        (gl:translate x y 0))
      (gl:color 0.0 1.0 1.0)
      (draw-circle (detection-radius tower))
      (ecase (detection-circle-color tower)
        (:red (gl:color 1.0 0.0 0.0 0.2))
        (:green (gl:color 0.0 1.0 0.0 0.2)))
      (draw-circle (detection-radius tower) 30 t))))

(register-wf-object 'blaster-tower "c:/dev/prj/towers/data/blaster.obj")

(defclass blaster-tower (tower shooting-tower-mixin)
  ((angle :initform 0.0 :accessor angle)
   (projectile-speed :initarg :projectile-speed :accessor projectile-speed))
  (:default-initargs
   :base-fire-rate 2
    :collision-radius 5
    :detection-radius 20
    :projectile-speed 2.0))

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
  (let ((pe (pos enemy))
        (ve (vel enemy))
        (pp (projectile-initial-position tower))
        (vp (projectile-initial-velocity tower)))
    (let* ((tc (/ (vec-distance pe pp) (vec-distance vp ve)))
           (pe-prime (vec+= (vec* ve tc) pe)))
      (normalize-deg (+ 270.0 (vec-angle (vec- pe-prime (pos tower))))))))

(defun good-to-fire-p (enemy tower)
  (let ((aim-angle (angle tower))
        (target-angle (target-angle enemy tower)))
    (< (abs (- aim-angle target-angle)) 4.0)))

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
  (let ((wf-object (find-wf-object 'blaster-tower)))
    (gl:with-pushed-matrix
      (with-vec (x y (pos tower))
        (gl:translate x y 0))
      (gl:color 0 1 0)
      (gl:scale 1.5 1.5 1.0)
      (gl:polygon-mode :front-and-back :line)
      (wf-draw-part 'blaster wf-object)
      (wf-draw-part 'inner-base wf-object)
      (gl:rotate (angle tower) 0.0 0.0 1.0)
      (wf-draw-part 'cannon wf-object)
      (gl:polygon-mode :front-and-back :fill))))

(defmethod select ((tower blaster-tower) op pos world)
  (declare (ignore pos))
  (ecase op
    (:obtain
     (setf (draw-detection-circle-p tower) t)
     (setf (tower (tower-control world)) tower))
    (:release
     (setf (tower (tower-control world)) nil)
     (setf (draw-detection-circle-p tower) nil))
    (:move)))

(defclass projectile (circle-collidable-object)
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
                   (when (collide-p proj enemy)
                     (push enemy enemies-hit)))
                 world :order :hit-test :type 'enemy)
    (when enemies-hit
      (projectile-hit proj enemies-hit world))))

(defclass blaster-projectile (projectile)
  ((damage :initarg :damage :accessor damage))
  (:default-initargs :collision-radius 0.5))

(defmethod projectile-initial-position ((tower blaster-tower))
  (vec+= (vel-vec 4.0 (- (angle tower))) (pos tower)))

(defmethod projectile-initial-velocity ((tower blaster-tower))
  (vel-vec (projectile-speed tower) (- (angle tower))))

(defmethod tower-projectile ((tower blaster-tower))
  (make-instance 'blaster-projectile
                 :pos (projectile-initial-position tower)
                 :vel (projectile-initial-velocity tower)
                 :damage 1))

(defmethod render ((proj blaster-projectile))
  (gl:with-pushed-matrix
    (with-vec (x y (pos proj))
      (gl:translate x y 0))
    (gl:color 0 1 0)
    (draw-circle 0.6)))

(defmethod projectile-hit ((proj blaster-projectile) enemies world)
  (dolist (enemy enemies)
    (when (<= (decf (hit-points enemy) (damage proj)) 0)
      (enemy-kill enemy world)))
  (remove-object proj world))

(defun enemy-kill (enemy world)
  (incf (cash (player world)) (cash-reward enemy))
  (remove-object enemy world)
  (add-object (make-instance 'explosion
                             :number-of-particles 100
                             :color (explosion-color enemy)
                             :center (pos enemy))
              world))

(defclass tower-factory (draggable-object circle-collidable-object)
  ((buy-prices :initarg :buy-prices :accessor buy-prices)
   (sell-prices :initarg :sell-prices :accessor sell-prices)
   (kind :initarg :kind :accessor kind)
   (prototype :accessor prototype)
   (new-tower :initform nil :accessor new-tower)))

(defmethod initialize-instance :after ((factory tower-factory) &rest initargs)
  (declare (ignore initargs))
  (setf (prototype factory)
        (make-instance (kind factory) :pos (pos factory)))
  (setf (collision-radius factory)
        (collision-radius (prototype factory))))

(defmethod render ((factory tower-factory))
  (render (prototype factory))
  (when (new-tower factory)
    (render (new-tower factory)))
  (with-vec (x y (pos factory))
    (gl:color 1.0 1.0 1.0)
    (display-text (- x 0.5) (- y 10.0) (aref (buy-prices factory) 0))))

(defmethod select ((factory tower-factory) op pos world)
  (with-slots (new-tower) factory
    (flet ((can-place-here-p ()
             (map-objects (lambda (object)
                            (when (collide-p new-tower object)
                              (return-from can-place-here-p nil)))
                          world :order :hit-test :type 'collidable-object)
             t))
      (ecase op
        (:obtain
         (setf new-tower
               (make-instance (kind factory)
                              :pos (copy-vec pos)
                              :draw-detection-circle t
                              :factory factory)))
        (:release
         (when (can-place-here-p)
           (try-buy new-tower (player world) world)
           (setf (draw-detection-circle-p new-tower) nil))
         (setf new-tower nil))
        (:move
         (when new-tower
           (vec-assign (pos new-tower) (x pos) (y pos))
           (setf (detection-circle-color new-tower)
                 (if (can-place-here-p) :green :red))))))))
                             

;;;; Enemies

(defclass enemy (circle-collidable-object)
  ((spd :initarg :speed :accessor spd)
   (path :initarg :path :accessor path)
   (next-pos-idx :initform 1 :accessor next-pos-idx)
   (hit-points :initarg :hit-points :accessor hit-points)
   (cash-reward :initarg :cash-reward :accessor cash-reward)
   (vel :initform (vec 0.0 0.0) :accessor vel)
   (explosion-color :initarg :explosion-color :accessor explosion-color)))

(defmethod update ((e enemy) tick world)
  (declare (ignore tick))
  ;; Check collision with homebase
  (map-objects (lambda (hb)
                 (when (collide-p e hb)
                   (enemy-suicide e world)
                   (when (= (decf (lives hb)) 0)
                     (game-over world))
                   (return-from update)))
               world :order :hit-test :type 'homebase)
  ;; Compute position and velocity
  (let* ((pos (pos e))
         (vertices (vertices (path e)))
         (next-pos (aref vertices (next-pos-idx e))))
    (when (vec=~ pos next-pos (spd e))
      (incf (next-pos-idx e))
      (when (= (next-pos-idx e) (length vertices))
        ;; This shouldn't happen, as we're supposed to crash into
        ;; homebase before reaching the endpoint
        (enemy-suicide e world)
        (return-from update))
      (setf next-pos (aref vertices (next-pos-idx e))))
    (setf (vel e) (vel-vec (spd e) (vec- next-pos pos)))
    (vec+= pos (vel e))))

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
  (:default-initargs :collision-radius 2 :explosion-color (list 0.0 0.5 0.5)))

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
    (apply #'gl:color (explosion-color sq))
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


;;;; Explosions

(defclass explosion ()
  ((positions :accessor positions)
   (velocities :accessor velocities)
   (accelerations :accessor accelerations)
   (angles :accessor angles)
   (angles-mul :accessor angles-mul)
   (energies :accessor energies)
   (color :initarg :color :accessor color)
   (center :initarg :center :accessor center)
   (number-of-particles :initarg :number-of-particles :accessor number-of-particles)))

(defmethod initialize-instance :after ((e explosion) &rest initargs &key number-of-particles center &allow-other-keys)
  (declare (ignore initargs))
  (let ((n number-of-particles))
    (with-vec (cx cy center)
      (with-slots (positions velocities accelerations energies angles angles-mul) e
        (setf positions (make-array n))
        (setf velocities (make-array n))
        (setf accelerations (make-array n))
        (setf energies (make-array n))
        (setf angles (make-array n))
        (setf angles-mul (make-array n))
        (dotimes (i n)
          (setf (aref positions i) (vec cx cy))
          (let ((a (random 360.0))
                (e (+ 50 (random 20)))
                (v (random 1.0)))
            (setf (aref velocities i) (vel-vec v a))
            (setf (aref accelerations i) (vel-vec (random (/ v e)) (- 360.0 a)))
            (setf (aref energies i) e))
          (setf (aref angles i) (random 360.0))
          (setf (aref angles-mul i) (/ (- (random 2.0) 1.0) 3.0)))))))

(defmethod update ((e explosion) tick world)
  (declare (ignore tick))
  (with-slots (positions velocities accelerations energies angles angles-mul) e
    (let ((n (number-of-particles e))
          (dead 0))
      (dotimes (i n)
        (cond ((plusp (aref energies i))
               (vec+= (aref positions i) (aref velocities i))
               (vec+= (aref velocities i) (aref accelerations i))
               (decf (aref energies i))
               (incf (aref angles i) (* (aref angles-mul i) (aref energies i))))
              (t
               (incf dead))))
      (when (= dead n)
        (remove-object e world)))))

(defmethod render ((e explosion))
  (with-slots (positions angles energies) e
    (let ((n (number-of-particles e)))
      (destructuring-bind (r g b) (color e)
        (dotimes (i n)
          (when (plusp (aref energies i))
            (gl:with-pushed-matrix
              (with-vec (px py (aref positions i))
                (gl:translate px py 0.0))
              (gl:rotate (aref angles i) 0.0 0.0 1.0)
              (gl:color r g b (/ (aref energies i) 70.0))
              (gl:with-primitive :line-loop
                (gl:vertex -1.0 0.0)
                (gl:vertex 1.0 0.0)
                (gl:vertex 0.0 1.0)))))))))


;;;; Game world

(defclass world ()
  ((objects :initform (make-array 7 :initial-element '()) :accessor objects)
   (dim :initform (vec 100.0 100.0) :accessor dim)))

(defun make-world ()
  (make-instance 'world))

(defun add-object (object world)
  (push object (aref (objects world) (object-list-index object))))

(defun object-list-index (object)
  (typecase object
    (tower-control 6)
    (player 5)
    (message 4)
    (projectile 3)
    (enemy 2)
    (tower 1)
    (t 0)))

(defun remove-object (object world)
  (deletef (aref (objects world) (object-list-index object)) object :count 1))

(defun map-objects (function world &key order (type t))
  (unless (type= type 'nil)
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

(defun tower-control (world)
  (first (aref (objects world) 6)))


;;;; Levels

(defgeneric make-level (name))

(defmacro define-level (name &body objects)
  (labels ((object-name (object)
             (getf (cdr object) :named))
           (object-class-name (object)
             (car object))
           (object-initargs (object)
             (let ((list (copy-list (cdr object))))
               (remf list :named)
               list)))
    (let ((object-names (loop for object in objects
                              when (object-name object)
                              collect it)))
      `(defmethod make-level ((name (eql ',name)))
         (let ((world (make-instance 'world)) ,@object-names)
           ,@(loop for object in objects
                   for name = (object-name object)
                   for make = `(make-instance ',(object-class-name object) ,@(object-initargs object))
                   collect `(add-object ,(if name `(setf ,name ,make) make) world))
           world)))))

(define-level level-1
  (homebase :lives 2 :pos (vec -50.0 -50.0))
  (path :named path :spline '(0.0 100.0 10.0 10.0 -10.0 -10.0 -50.0 -50.0))
  (player :cash 10)
  (tower-control)
  (tower-factory :kind 'blaster-tower :pos (vec -60.0 -85.0)
                 :buy-prices #(5 5 7 10 15 20 30)
                 :sell-prices #(0 2 5 7 10 15 25))
  (wave :start-tick 100 :wait-ticks 50 :enemies
        (loop repeat 5 collecting
              (make-instance 'sqrewy
                             :pos (vec 0.0 100.0)
                             :speed 0.4
                             :path path
                             :hit-points 5
                             :cash-reward 1)))
  (wave :start-tick 500 :wait-ticks 50 :enemies
        (loop repeat 6 collecting
              (make-instance 'sqrewy
                             :pos (vec 0.0 100.0)
                             :speed 0.6
                             :path path
                             :hit-points 10
                             :cash-reward 3)))
  (wave :start-tick 1000 :wait-ticks 40 :enemies
        (loop repeat 7 collecting
              (make-instance 'sqrewy
                             :pos (vec 0.0 100.0)
                             :speed 0.7
                             :path path
                             :hit-points 15
                             :cash-reward 5)))
  (grid))

(define-level level-2
  (homebase :lives 5 :pos (vec -57.375 -49.375))
  (path :named path :spline '(0.0 100.0 14.125 74.375 -44.625 69.125 -57.375
                              39.625 -67.125 1.875 -6.125 23.625 34.625
                              46.125 64.625 62.875 118.875 15.125 60.125
                              -36.375 13.125 -61.875 -32.875 -26.125 -57.375 -49.375))
  (player :cash 20)
  (tower-control)
  (tower-factory :kind 'blaster-tower :pos (vec -60.0 -85.0)
                 :buy-prices #(5 5 7 10 15 20 30)
                 :sell-prices #(0 2 5 7 10 15 25))
  (wave :start-tick 100 :wait-ticks 50 :enemies
        (loop repeat 10 collecting
              (make-instance 'sqrewy
                             :pos (vec 0.0 100.0)
                             :speed 0.5
                             :path path
                             :hit-points 10
                             :cash-reward 2)))
  (wave :start-tick 500 :wait-ticks 40 :enemies
        (loop repeat 5 collecting
              (make-instance 'sqrewy
                             :pos (vec 0.0 100.0)
                             :speed 0.6
                             :path path
                             :hit-points 15
                             :cash-reward 5)))
  (wave :start-tick 1000 :wait-ticks 60 :enemies
        (loop repeat 10 collecting
              (make-instance 'sqrewy
                             :pos (vec 0.0 100.0)
                             :speed 0.8
                             :path path
                             :hit-points 30
                             :cash-reward 10)))
  (grid))


;;;; Game window

(defclass mouse (circle-collidable-object)
  ((selection :initform nil :accessor selection))
  (:default-initargs :pos (vec 0.0 0.0) :collision-radius 2))

(defun pick-object (mouse world)
  (map-objects
   (lambda (object)
     (when (collide-p object mouse)
       (return-from pick-object object)))
   world :order :hit-test :type 'pickable-object))

(defclass game-window (glut:window)
  ((world :initarg :world :accessor world)
   (time-to-next-tick :initform nil :accessor time-to-next-tick)
   (tick :initform nil :accessor tick)
   (mouse :initform (make-instance 'mouse) :accessor mouse))
  (:default-initargs
   :width 800 :height 800
   :title "Game"
   :mode '(:double :rgb)))

(defmethod glut:display-window :before ((w game-window))
  (gl:clear-color 0 0 0 0)
  (gl:shade-model :flat)
  (gl:disable :depth-test)
  (gl:enable :blend)
  (gl:blend-func :src-alpha :one-minus-src-alpha))

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
  (multiple-value-bind (x y) (glu:un-project x y 0.0)
    (vec-assign (pos (mouse w)) x (- y)))
  (select (selection (mouse w)) :move (pos (mouse w)) (world w)))

(defgeneric left-button (state mouse selected-object picked-object world)
  (:method (state mouse selected-object picked-object world)
    (declare (ignore state mouse selected-object picked-object world))))

(defmethod glut:mouse ((w game-window) button state x y)
  (glut:motion w x y)
  (case button
    (:left-button
     (let ((m (mouse w)))
       (left-button state m (selection m) (pick-object m (world w)) (world w))))))

(defun obtain-object (object mouse world)
  (when (selection mouse)
    (release-object mouse world))
  (setf (selection mouse) object)
  (select (selection mouse) :obtain (pos mouse) world))

(defun release-object (mouse world)
  (when (selection mouse)
    (select (selection mouse) :release (pos mouse) world)
    (setf (selection mouse) nil)))

(defmethod left-button ((state (eql :down)) mouse (selected-object selectable-object) (picked-object null) world)
  (release-object mouse world))

(defmethod left-button ((state (eql :down)) mouse selected-object (picked-object selectable-object) world)
  (declare (ignore selected-object))
  (obtain-object picked-object mouse world))

(defmethod left-button ((state (eql :down)) mouse selected-object (picked-object draggable-object) world)
  (declare (ignore selected-object))
  (obtain-object picked-object mouse world))

(defmethod left-button ((state (eql :up)) mouse (selected-object draggable-object) picked-object world)
  (declare (ignore picked-object))
  (release-object mouse world))

(defmethod left-button ((state (eql :down)) mouse selected-object (picked-object clickable-object) world)
  (declare (ignore selected-object))
  (select picked-object :obtain (pos mouse) world))

(defmethod left-button ((state (eql :up)) mouse selected-object (picked-object clickable-object) world)
  (declare (ignore selected-object))
  (select picked-object :release (pos mouse) world))

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
   (make-instance 'game-window :world (make-level 'level-2))))


;;;; Spline editor

(defclass spliner ()
  ((points :initform '() :accessor points)))

(defmethod render ((sp spliner))
  (gl:color 1.0 1.0 1.0)
  (let ((spline (loop for p in (points sp)
                      nconc (with-vec (x y (pos p)) (list x y)))))
    (let ((path (compile-path spline)))
      (when (not (emptyp path))
        (gl:with-primitive :line-strip
          (loop for v across path do
                (with-vec (x y v)
                  (gl:vertex x y))))))))

(defclass spline-point (draggable-object circle-collidable-object)
  ()
  (:default-initargs :collision-radius 2))

(defmethod render ((sp spline-point))
  (gl:color 1.0 1.0 1.0)
  (gl:with-pushed-matrix
    (with-vec (x y (pos sp))
      (gl:translate x y 0.0)
      (draw-circle 2))))

(defmethod select ((sp spline-point) op pos world)
  (declare (ignore world))
  (ecase op
    (:obtain)
    (:release)
    (:move (vec-assign (pos sp) (x pos) (y pos)))))

(define-level spline-editor
  (spliner)
  (grid))

(defclass spline-editor (game-window)
  ((mouse :initform (make-instance 'mouse) :accessor mouse)))

(defmethod glut:keyboard ((w spline-editor) key x y)
  (let ((mouse (mouse w))
        (spliner (block nil
                   (map-objects (lambda (x) (return x))
                                (world w) :type 'spliner))))
    (multiple-value-bind (x y) (glu:un-project x y 0.0)
      (vec-assign (pos (mouse w)) x (- y)))
    (case key
      (#\a
       (let ((sp (make-instance 'spline-point :pos (copy-vec (pos mouse)))))
         (appendf (points spliner) (list sp))
         (add-object sp (world w))))
      (#\d
       (when-let (sp (pick-object (mouse w) (world w)))
         (deletef (points spliner) sp)
         (remove-object sp (world w))))
      (#\p
       (format t "(")
       (dolist (sp (points spliner))
         (with-vec (x y (pos sp))
           (format t " ~,3F ~,3F~%" x y)))
       (format t ")~%")
       (finish-output))
      (t (call-next-method)))))

(defun spline-editor ()
  (glut:display-window
   (make-instance 'spline-editor :world (make-level 'spline-editor))))
