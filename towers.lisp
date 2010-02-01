;;;; +----------------------------------------------------------------+
;;;; | TOWERS - A silly geoDefense clone wannabe          DEATH, 2009 |
;;;; +----------------------------------------------------------------+

(in-package #:towers)


;;;; Configuration

(defparameter *frames-per-second* 30)
(defparameter *tick-duration* (floor 1000 *frames-per-second*))
(defparameter *draw-collision-circle-for-type* 'nil)
(defparameter *draw-tick* nil)

(defparameter *data-directory*
  (let ((pathname (make-pathname :directory '(:relative "data"))))
    (if (find-package "ASDF")
        (let* ((system (funcall (find-symbol "FIND-SYSTEM" "ASDF") "towers"))
               (home (funcall (find-symbol "COMPONENT-PATHNAME" "ASDF") system)))
          (setf pathname (merge-pathnames pathname home)))
        pathname)))


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
  (with-open-file (in (merge-pathnames filename *data-directory*) :direction :input)
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

(defclass point-collidable-object (collidable-object)
  ((pos :initarg :pos :accessor pos)))

(defclass circle-collidable-object (collidable-object)
  ((collision-radius :initarg :collision-radius :accessor collision-radius)
   (pos :initarg :pos :accessor pos)))

(defun close-enough-p (pa ra pb rb)
  (< (vec-distance-sq pa pb) (square (+ ra rb))))

(define-symmetric collide-p ((a circle-collidable-object)
                             (b circle-collidable-object))
  (close-enough-p (pos a) (collision-radius a)
                  (pos b) (collision-radius b)))

(define-symmetric collide-p ((a circle-collidable-object)
                             (b point-collidable-object))
  (close-enough-p (pos a) (collision-radius a)
                  (pos b) 1))

(defclass line-segment-collidable-object (collidable-object)
  ((start-pos :initarg :start-pos :accessor start-pos)
   (end-pos :initarg :end-pos :accessor end-pos)))

(defun closest-point-on-segment (start-pos end-pos circle-pos)
  (let* ((seg-v (vec- end-pos start-pos))
         (pt-v (vec- circle-pos start-pos))
         (seg-v-unit (unit seg-v))
         (proj (vec-mul pt-v seg-v-unit)))
    (cond ((<= proj 0) start-pos)
          ((>= proj (vec-mag seg-v)) end-pos)
          (t (vec+= (vec* seg-v-unit proj) start-pos)))))

(defun segment-collides-with-circle-p (start-pos end-pos circle-pos circle-radius)
  (let ((closest (closest-point-on-segment start-pos end-pos circle-pos)))
    (<= (vec-mag (vec- circle-pos closest)) circle-radius)))

(define-symmetric collide-p ((a line-segment-collidable-object)
                             (b circle-collidable-object))
  (segment-collides-with-circle-p (start-pos a) (end-pos a) (pos b) (collision-radius b)))



;;;; Game object protocol

(defclass pickable-object (collidable-object)
  ())

(defclass draggable-object (pickable-object)
  ())

(defclass selectable-object (pickable-object)
  ())

(defclass clickable-object (pickable-object)
  ())

(defgeneric update (object)
  (:method (object)
    (declare (ignore object))))

(defgeneric render (object)
  (:method (object)
    (declare (ignore object))))

(defgeneric select (object op pos)
  (:method ((object null) op pos)
    (declare (ignore op pos))))


;;;; Game world

(defparameter *half-world-dimensions* (vec 100.0 100.0))

(defvar *world*)
(defvar *tick*)

(defclass world ()
  ((objects-to-delete :initform '() :accessor objects-to-delete)
   (objects :initform (make-array 7 :initial-element '()) :accessor objects)
   (tick :initform nil :accessor tick)))

(defmethod reinitialize-instance :before ((world world) &rest initargs)
  (declare (ignore initargs))
  (setf (tick world) nil)
  (clear-objects world))

(defun ensure-world (world-designator)
  (etypecase world-designator
    (symbol (make-instance world-designator))
    (world world-designator)))

(defun add-object (object &optional (world *world*))
  (push object (aref (objects world) (object-list-index object))))

(defun object-list-index (object)
  (typecase object
    (message 6)
    (tower-control 5)
    (player 4)
    (projectile 3)
    (enemy 2)
    (tower 1)
    (t 0)))

(defgeneric object-got-removed (object world)
  (:method (object world)
    (declare (ignore object world))))

(defun remove-object (object &optional (world *world*))
  (push object (objects-to-delete world))
  (object-got-removed object world))

(defun expunge-objects (&optional (world *world*))
  (dolist (object (objects-to-delete world))
    (deletef (aref (objects world) (object-list-index object))
             object :count 1))
  (setf (objects-to-delete world) '()))

(defun clear-objects (&optional (world *world*))
  (dotimes (i (length (objects world)))
    (setf (aref (objects world) i) '()))
  (setf (objects-to-delete world) '()))

(defun map-objects (function world order type)
  (unless (type= type 'nil)
    (flet ((maybe-call-function (object)
             (when (and (typep object type)
                        (not (member object (objects-to-delete world))))
               (funcall function object))))
      (ecase order
        ((:render :update)
         (loop for list across (objects world) do
               (mapc #'maybe-call-function list)))
        ((:hit-test)
         (loop for i from (1- (length (objects world))) downto 0 do
               (mapc #'maybe-call-function (aref (objects world) i))))))))

(defmacro do-objects ((object-var &key (world '*world*) (order :hit-test) (type t) collecting) &body forms)
  (if collecting
      (with-gensyms (collection)
        `(let ((,collection '()))
           (flet ((collect (x) (push x ,collection)))
             (block nil
               (map-objects (lambda (,object-var) ,@forms) ,world ,order ,type)))
           ,collection))
      `(block nil
         (map-objects (lambda (,object-var) ,@forms) ,world ,order ,type))))

(defmethod update ((w world))
  (let ((*world* w)
        (*tick* (tick w)))
    (do-objects (object :order :update)
      (update object))
    (expunge-objects)))

(defmethod render ((w world))
  (let ((*world* w))
    (do-objects (object :order :render)
      (render object)
      (when (typep object *draw-collision-circle-for-type*)
        (gl:with-pushed-matrix
          (with-vec (x y (pos object))
            (gl:translate x y 0.0))
          (gl:color 1.0 0.0 0.0)
          (draw-circle (collision-radius object)))))
    (when *draw-tick*
      (gl:color 1.0 1.0 1.0)
      (display-text -98.0 95.0 (tick w)))))

(defun player (&optional (world *world*))
  (first (aref (objects world) 4)))

(defun tower-control (&optional (world *world*))
  (first (aref (objects world) 5)))


;;;; Game window

(defclass mouse (point-collidable-object)
  ((selection :initform nil :accessor selection))
  (:default-initargs :pos (vec 0.0 0.0)))

(defun pick-object (mouse)
  (do-objects (object :type 'pickable-object)
    (when (collide-p object mouse)
      (return-from pick-object object))))

(defclass game-window (glut:window)
  ((world :accessor world)
   (world-generator :initarg :world-generator :accessor world-generator)
   (time-to-next-tick :initform nil :accessor time-to-next-tick)
   (mouse :initform (make-instance 'mouse) :accessor mouse))
  (:default-initargs
   :name 'towers
   :width 800 :height 800
   :title "Towers"
   :mode '(:double :rgb)))

(defmethod initialize-instance :after ((w game-window) &rest initargs &key world)
  (declare (ignore initargs))
  (when world
    (setf (world-generator w)
          (let ((worlds (if (listp world)
                            (mapcar #'ensure-world world)
                            (list (ensure-world world)))))
            (lambda ()
              (pop worlds)))))
  (next-world w))

(defun find-game-window ()
  (glut:find-window 'towers))

(defun next-world (&optional (w (find-game-window)))
  (when (null (setf (world w) (funcall (world-generator w))))
    (glut:destroy-current-window)))

(defun this-world-again (&optional (w (find-game-window)))
  (reinitialize-instance (world w)))

(defmethod tick ((w game-window))
  (tick (world w)))

(defmethod (setf tick) (new-value (w game-window))
  (setf (tick (world w)) new-value))

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
  (gl:translate .375 .375 0.0)
  (render (world w))
  (glut:swap-buffers))

(defmethod glut:reshape ((w game-window) width height)
  (gl:viewport 0 0 width height)
  (gl:matrix-mode :projection)
  (gl:load-identity)
  (with-vec (x y *half-world-dimensions*)
    (gl:ortho (- x) x (- y) y 0 1))
  (gl:matrix-mode :modelview))

(defmethod glut:keyboard ((w game-window) key x y)
  (declare (ignore x y))
  (case key
    (#\Esc (glut:destroy-current-window))))

(defmethod glut:motion ((w game-window) x y)
  (let ((*world* (world w)))
    (multiple-value-bind (x y) (glu:un-project x y 0.0)
      (vec-assign (pos (mouse w)) x (- y)))
    (select (selection (mouse w)) :move (pos (mouse w)))))

(defgeneric left-button (state mouse selected-object picked-object)
  (:method (state mouse selected-object picked-object)
    (declare (ignore state mouse selected-object picked-object))))

(defmethod glut:mouse ((w game-window) button state x y)
  (glut:motion w x y)
  (let ((*world* (world w)))
    (case button
      (:left-button
       (let ((m (mouse w)))
         (left-button state m (selection m) (pick-object m)))))))

(defun obtain-object (object mouse)
  (when (selection mouse)
    (release-object mouse))
  (setf (selection mouse) object)
  (select (selection mouse) :obtain (pos mouse)))

(defun release-object (mouse)
  (when (selection mouse)
    (select (selection mouse) :release (pos mouse))
    (setf (selection mouse) nil)))

(defmethod left-button ((state (eql :down)) mouse (selected-object selectable-object) (picked-object null))
  (release-object mouse))

(defmethod left-button ((state (eql :down)) mouse selected-object (picked-object selectable-object))
  (declare (ignore selected-object))
  (obtain-object picked-object mouse))

(defmethod left-button ((state (eql :down)) mouse selected-object (picked-object draggable-object))
  (declare (ignore selected-object))
  (obtain-object picked-object mouse))

(defmethod left-button ((state (eql :up)) mouse (selected-object draggable-object) picked-object)
  (declare (ignore picked-object))
  (release-object mouse))

(defmethod left-button ((state (eql :down)) mouse selected-object (picked-object clickable-object))
  (declare (ignore selected-object))
  (select picked-object :obtain (pos mouse)))

(defmethod left-button ((state (eql :up)) mouse selected-object (picked-object clickable-object))
  (declare (ignore selected-object))
  (select picked-object :release (pos mouse)))

(defmethod glut:idle ((w game-window))
  (let ((now (glut:get :elapsed-time)))
    (when (null (tick w))
      (setf (tick w) -1)
      (setf (time-to-next-tick w) now))
    (when (>= now (time-to-next-tick w))
      (incf (tick w))
      (setf (time-to-next-tick w) (+ now *tick-duration*))
      (update (world w))
      (glut:post-redisplay))))

(defun display-text (x y object &rest format-args)
  (let ((string (if (stringp object)
                    (apply #'format nil object format-args)
                    (princ-to-string object))))
    (gl:with-pushed-matrix
      (gl:load-identity)
      (gl:raster-pos x y)
      (glut:bitmap-string glut:+bitmap-8-by-13+ string))))


;;;; Game

(defun game ()
  (glut:display-window
   (make-instance 'game-window :world '(level-1 level-2 level-3))))


;;;; Player

(defclass player ()
  ((cash :initarg :cash :accessor cash)))

(defmethod render ((player player))
  (gl:with-pushed-matrix
    (gl:color 1.0 1.0 1.0)
    (display-text -90.0 -80.0 (cash player))))

(defun try-buy (tower &optional (player (player)))
  (when (>= (cash player) (buy-price tower))
    (decf (cash player) (buy-price tower))
    (incf (level tower))
    (add-object tower)))

(defun try-upgrade (tower &optional (player (player)))
  (when (and (< (level tower) (max-level tower))
             (>= (cash player) (buy-price tower)))
    (decf (cash player) (buy-price tower))
    (incf (level tower))))

(defun sell (tower &optional (player (player)))
  (incf (cash player) (sell-price tower))
  (setf (tower (tower-control)) nil)
  (remove-object tower))


;;;; Tower control

(defclass tower-control (clickable-object circle-collidable-object)
  ((tower :initarg :tower :accessor tower))
  (:default-initargs :tower nil :collision-radius 16 :pos (vec 62.0 -82.0)))

(defmethod render ((control tower-control))
  (when-let (tower (tower control))
    (gl:with-pushed-matrix
      (gl:color 0.2 0.5 1.0)
      (display-text 50.0 -75.0 (type-of tower))
      (display-text 50.0 -80.0 "Level ~D" (level tower))
      (when (< (level tower) (max-level tower))
        (display-text 50.0 -85.0 "Upgrade (~D)" (buy-price tower)))
      (display-text 50.0 -90.0 "Sell (~D)" (sell-price tower)))))

(defmethod select ((control tower-control) op pos)
  (ecase op
    (:obtain
     (when-let (tower (tower control))
       (with-vec (x y pos)
         (cond ((and (>= x 50.0) (>= y -86.0) (<= y -81.0))
                (try-upgrade tower))
               ((and (>= x 50.0) (>= y -91.0) (<= y -86.0))
                (sell tower))))))
    (:release)
    (:move)))


;;;; Grid

(defclass grid ()
  ())

(defmethod render ((grid grid))
  (gl:with-pushed-matrix
    (gl:color 0.2 0.2 0.2)
    (gl:translate -100.0 100.0 0.0)
    (gl:scale 1 -1 1)
    (gl:with-primitive :lines
      (dotimes (y 18)
        (gl:vertex 0.0 (* y 10.0))
        (gl:vertex 200.0 (* y 10.0)))
      (dotimes (x 20)
        (gl:vertex (* x 10.0) 0.0)
        (gl:vertex (* x 10.0) 170.0)))))


;;;; Path

(defclass path (collidable-object)
  ((vertices :initarg :vertices :accessor vertices)))

(defmethod initialize-instance :after ((path path) &rest initargs &key spline &allow-other-keys)
  (declare (ignore initargs))
  (when spline
    (setf (vertices path) (compile-path spline))))

(defmethod render ((path path))
  (gl:color 0.0 0.0 0.6)
  (gl:with-primitive :line-strip
    (loop for v across (vertices path) do
          (with-vec (x y v)
            (gl:vertex x y)))))

(defun compile-path (spline)
  (coerce
   (loop for (ax ay bx by cx cy dx dy) on spline by #'cddddddr
         when (and ax ay bx by cx cy dx dy)
         nconc (loop for (am bm cm dm) in (collect-results #'call-with-curve-multipliers)
                     collect (vec (+ (* am ax) (* bm bx) (* cm cx) (* dm dx))
                                  (+ (* am ay) (* bm by) (* cm cy) (* dm dy)))))
   'vector))

(define-symmetric collide-p ((a circle-collidable-object) (b path))
  (loop for i from 0 below (1- (length (vertices b)))
        for v1 = (aref (vertices b) i)
        for v2 = (aref (vertices b) (1+ i))
        thereis (segment-collides-with-circle-p v1 v2 (pos a) (collision-radius a))))


;;;; Homebase

(register-wf-object 'homebase "homebase.obj")

(defclass homebase (circle-collidable-object)
  ((lives :initarg :lives :accessor lives)
   (angle :initform 0.0 :accessor angle))
  (:default-initargs :collision-radius 8))

(defmethod update ((hb homebase))
  (incf (angle hb) 2))

(defmethod render ((hb homebase))
  (gl:color 0.6 0.2 0.8)
  (gl:with-pushed-matrix
    (with-vec (x y (pos hb))
      (gl:translate x y 0.0)
      (gl:scale 1.5 1.5 1.0)
      (gl:polygon-mode :front-and-back :line)
      (gl:rotate (angle hb) 0.0 0.0 1.0)
      (wf-draw (find-wf-object 'homebase))
      (gl:polygon-mode :front-and-back :fill)
      (display-text (- x 0.5) (- y 0.5) (max 0 (lives hb))))))


;;;; Towers

(defclass tower (selectable-object circle-collidable-object)
  ((level :initarg :level :accessor level)
   (factory :initarg :factory :accessor tower-factory))
  (:default-initargs :level 0))

(defgeneric try-fire (tower tick))
(defgeneric tower-projectile (tower))
(defgeneric fire-rate (tower))
(defgeneric detection-radius (tower))

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
   (base-fire-rate :initarg :base-fire-rate :accessor base-fire-rate)
   (base-detection-radius :initarg :base-detection-radius :accessor base-detection-radius)
   (draw-detection-circle :initarg :draw-detection-circle :accessor draw-detection-circle-p)
   (detection-circle-color :initform :red :accessor detection-circle-color))
  (:default-initargs :draw-detection-circle nil))

(defun level-multiplier (level max-level &optional (scale 1.0) (add 1.0))
  (+ add (* scale (- 1.0 (log (- max-level (1- level)) max-level)))))

(defmethod fire-rate ((tower shooting-tower-mixin))
  (* (base-fire-rate tower)
     (level-multiplier (level tower) (max-level tower))))

(defmethod detection-radius ((tower shooting-tower-mixin))
  (* (base-detection-radius tower)
     (level-multiplier (level tower) (max-level tower))))

(defgeneric fire (tower))

(defmethod try-fire ((tower shooting-tower-mixin) tick)
  (let ((last-shot-tick (last-shot-tick tower)))
    (when (or (null last-shot-tick)
              (>= (- tick last-shot-tick) (floor *frames-per-second* (fire-rate tower))))
      (fire tower)
      (setf (last-shot-tick tower) tick))))

(defmethod render :after ((tower shooting-tower-mixin))
  (when (draw-detection-circle-p tower)
    (gl:with-pushed-matrix
      (with-vec (x y (pos tower))
        (gl:translate x y 0.0))
      (gl:color 0.0 1.0 1.0)
      (draw-circle (detection-radius tower))
      (ecase (detection-circle-color tower)
        (:red (gl:color 1.0 0.0 0.0 0.2))
        (:green (gl:color 0.0 1.0 0.0 0.2)))
      (draw-circle (detection-radius tower) 30 t))))

(defmethod select ((tower shooting-tower-mixin) op pos)
  (declare (ignore pos))
  (ecase op
    (:obtain
     (setf (draw-detection-circle-p tower) t)
     (setf (tower (tower-control)) tower))
    (:release
     (setf (tower (tower-control)) nil)
     (setf (draw-detection-circle-p tower) nil))
    (:move)))

(defun detect-enemies (tower)
  (do-objects (enemy :type 'enemy :collecting t)
    (when (close-enough-p (pos enemy) (collision-radius enemy)
                          (pos tower) (detection-radius tower))
      (collect enemy))))

(defgeneric target-angle (enemy tower))
(defgeneric aim (tower enemy))

(defun good-to-fire-p (enemy tower)
  (let ((aim-angle (angle tower))
        (target-angle (target-angle enemy tower)))
    (when (< (abs (- aim-angle target-angle)) 4.0)
      enemy)))

(defclass projectile (collidable-object)
  ())

(defun projectile-hit-list (projectile)
  (do-objects (enemy :type 'enemy :collecting t)
    (when (collide-p projectile enemy)
      (collect enemy))))


;;;; Blaster Tower

(register-wf-object 'blaster-tower "blaster.obj")

(defclass blaster-tower (tower shooting-tower-mixin)
  ((angle :initform 0.0 :accessor angle)
   (projectile-speed :initarg :projectile-speed :accessor projectile-speed))
  (:default-initargs
   :base-fire-rate 2
    :collision-radius 5
    :base-detection-radius 15
    :projectile-speed 2.0))

(defmethod update ((tower blaster-tower))
  (when-let (enemies (detect-enemies tower))
    (let ((victim (some (lambda (enemy) (good-to-fire-p enemy tower)) enemies)))
      (cond (victim
             (try-fire tower *tick*)
             (aim tower victim))
            (t
             (aim tower (best-element enemies :key (lambda (enemy) (target-angle enemy tower)))))))))

(defmethod render ((tower blaster-tower))
  (let ((wf-object (find-wf-object 'blaster-tower)))
    (gl:with-pushed-matrix
      (with-vec (x y (pos tower))
        (gl:translate x y 0.0))
      (gl:color 0.0 1.0 0.0)
      (gl:scale 1.5 1.5 1.0)
      (gl:polygon-mode :front-and-back :line)
      (wf-draw-part 'blaster wf-object)
      (wf-draw-part 'inner-base wf-object)
      (gl:rotate (angle tower) 0.0 0.0 1.0)
      (wf-draw-part 'cannon wf-object)
      (gl:polygon-mode :front-and-back :fill))))

(defun blaster-projectile-initial-parameters (tower)
  (values (vec+= (vel-vec 4.0 (- (angle tower))) (pos tower))
          (vel-vec (projectile-speed tower) (- (angle tower)))))

(defmethod target-angle (enemy (tower blaster-tower))
  (let ((pe (pos enemy))
        (ve (vel enemy)))
    (multiple-value-bind (pp vp)
        (blaster-projectile-initial-parameters tower)
      (let* ((tc (/ (vec-distance pe pp) (vec-distance vp ve)))
             (pe-prime (vec+= (vec* ve tc) pe)))
        (normalize-deg (+ 270.0 (vec-angle (vec- pe-prime (pos tower)))))))))

(defmethod aim ((tower blaster-tower) enemy)
  (setf (angle tower) (target-angle enemy tower)))

(defclass blaster-projectile (projectile circle-collidable-object)
  ((damage :initarg :damage :accessor damage)
   (vel :initarg :vel :accessor vel))
  (:default-initargs :collision-radius 0.5))

(defmethod tower-projectile ((tower blaster-tower))
  (multiple-value-bind (pp vp)
      (blaster-projectile-initial-parameters tower)
    (make-instance 'blaster-projectile :pos pp :vel vp :damage 1)))

(defmethod fire ((tower blaster-tower))
  (add-object (tower-projectile tower)))

(defmethod update ((proj blaster-projectile))
  (vec+= (pos proj) (vel proj))
  (unless (vec-contains *half-world-dimensions* (pos proj))
    (remove-object proj)
    (return-from update))
  (when-let (enemies (projectile-hit-list proj))
    (dolist (enemy enemies)
      (enemy-take-damage enemy (damage proj)))
    (remove-object proj)))

(defmethod render ((proj blaster-projectile))
  (gl:with-pushed-matrix
    (with-vec (x y (pos proj))
      (gl:translate x y 0.0))
    (gl:color 0.0 1.0 0.0)
    (draw-circle 0.6)))


;;;; Laser Tower

(register-wf-object 'laser-tower "blaster.obj")

(defclass laser-tower (tower shooting-tower-mixin)
  ((angle :initform 0.0 :accessor angle)
   (beam :initform nil :accessor beam))
  (:default-initargs
   :base-fire-rate 0.5
    :collision-radius 5
    :base-detection-radius 20))

(defmethod update ((tower laser-tower))
  (let ((enemies (detect-enemies tower)))
    (when enemies
      (let ((victim (some (lambda (enemy) (good-to-fire-p enemy tower)) enemies)))
        (cond (victim
               (try-fire tower *tick*)
               (aim tower victim))
              (t
               (aim tower (best-element enemies :key (lambda (enemy) (target-angle enemy tower))))))))
    (when (beam tower)
      (cond ((or (> (- *tick* (last-shot-tick tower)) 15) (null enemies))
             (remove-object (beam tower))
             (setf (beam tower) nil))
            (t
             (multiple-value-bind (sp ep)
                 (laser-beam-initial-parameters tower)
               (setf (start-pos (beam tower)) sp)
               (setf (end-pos (beam tower)) ep)))))))

(defmethod render ((tower laser-tower))
  (let ((wf-object (find-wf-object 'laser-tower)))
    (gl:with-pushed-matrix
      (with-vec (x y (pos tower))
        (gl:translate x y 0.0))
      (gl:color 0.2 0.4 1.0)
      (gl:scale 1.5 1.5 1.0)
      (gl:polygon-mode :front-and-back :line)
      (wf-draw-part 'blaster wf-object)
      (wf-draw-part 'inner-base wf-object)
      (gl:rotate (angle tower) 0.0 0.0 1.0)
      (wf-draw-part 'cannon wf-object)
      (gl:polygon-mode :front-and-back :fill))))

(defmethod object-got-removed ((tower laser-tower) world)
  (when (beam tower)
    (remove-object (beam tower) world)))

(defmethod target-angle (enemy (tower laser-tower))
  (normalize-deg (+ 270.0 (vec-angle (vec- (pos enemy) (pos tower))))))

(defmethod aim ((tower laser-tower) enemy)
  (let* ((aim-angle (angle tower))
         (target-angle (target-angle enemy tower))
         (diff (- target-angle aim-angle)))
    (when (> diff 180.0) (decf diff 360.0))
    (when (< diff -180.0) (incf diff 360.0))
    (setf (angle tower)
          (normalize-deg (+ (angle tower)
                            (max -10.0 (min diff 10.0)))))))

(defun laser-beam-initial-parameters (tower)
  (values (vec+= (vel-vec 5.0 (- (angle tower))) (pos tower))
          (vec+= (vel-vec 200.0 (- (angle tower))) (pos tower))))

(defclass laser-beam (projectile line-segment-collidable-object)
  ((damage :initarg :damage :accessor damage)))

(defmethod tower-projectile ((tower laser-tower))
  (multiple-value-bind (sp ep)
      (laser-beam-initial-parameters tower)
    (make-instance 'laser-beam :start-pos sp :end-pos ep :damage 0.5)))

(defmethod fire ((tower laser-tower))
  (when (null (beam tower))
    (add-object (setf (beam tower) (tower-projectile tower)))))

(defmethod update ((beam laser-beam))
  (when-let (enemies (projectile-hit-list beam))
    (dolist (enemy enemies)
      (enemy-take-damage enemy (damage beam)))))

(defmethod render ((beam laser-beam))
  (gl:with-pushed-matrix
    (gl:color 1.0 1.0 1.0)
    (gl:with-primitive :lines
      (with-vec (x y (start-pos beam))
        (gl:vertex x y))
      (with-vec (x y (end-pos beam))
        (gl:vertex x y)))))


;;;; Tower factory

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

(defmethod select ((factory tower-factory) op pos)
  (with-slots (new-tower) factory
    (flet ((can-place-here-p ()
             (do-objects (object :type '(and collidable-object
                                         (not projectile) (not enemy)))
               (when (collide-p new-tower object)
                 (return-from can-place-here-p nil)))
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
           (try-buy new-tower)
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
   (max-hit-points :initarg :hit-points :accessor max-hit-points)
   (cash-reward :initarg :cash-reward :accessor cash-reward)
   (vel :initform (vec 0.0 0.0) :accessor vel)
   (explosion-color :initarg :explosion-color :accessor explosion-color)))

(defmethod update ((e enemy))
  ;; Check collision with homebase
  (do-objects (hb :type 'homebase)
    (when (collide-p e hb)
      (enemy-die e)
      (when (= (decf (lives hb)) 0)
        (game-over))
      (return-from update)))
  ;; Compute position and velocity
  (let* ((pos (pos e))
         (vertices (vertices (path e)))
         (next-pos (aref vertices (next-pos-idx e))))
    (when (vec=~ pos next-pos (spd e))
      (incf (next-pos-idx e))
      (when (= (next-pos-idx e) (length vertices))
        ;; This shouldn't happen, as we're supposed to crash into
        ;; homebase before reaching the endpoint
        (enemy-die e)
        (return-from update))
      (setf next-pos (aref vertices (next-pos-idx e))))
    (setf (vel e) (vel-vec (spd e) (vec- next-pos pos)))
    (vec+= pos (vel e))))

(defmethod render :after ((e enemy))
  (gl:with-pushed-matrix
    (with-vec (x y (pos e))
      (gl:translate (- x 2.0) y 0.0))
    (let ((vs '((0.0 2.0) (4.0 2.0) (4.0 3.0) (0.0 3.0))))
      (gl:with-pushed-matrix
        (gl:scale (float (/ (hit-points e) (max-hit-points e))) 1.0 1.0)
        (gl:color 0.0 1.0 0.0 0.7)
        (gl:with-primitive :quads
          (loop for (x y) in vs do (gl:vertex x y))))
      (gl:color 1.0 1.0 1.0 0.5)
      (gl:with-primitive :line-loop
        (loop for (x y) in vs do (gl:vertex x y))))))

(defun enemy-die (enemy)
  (remove-object enemy)
  (maybe-win-level))

(defun enemy-kill (enemy)
  (incf (cash (player)) (cash-reward enemy))
  (add-object (make-instance 'explosion
                             :number-of-particles 100
                             :color (explosion-color enemy)
                             :center (pos enemy)))
  (enemy-die enemy))

(defun enemy-take-damage (enemy damage)
  (when (<= (decf (hit-points enemy) damage) 0)
    (enemy-kill enemy)))

(defun maybe-win-level ()
  (flet ((no-win () (return-from maybe-win-level)))
    (do-objects (object :type '(or enemy wave homebase))
      (etypecase object
        ((or enemy wave) (no-win))
        (homebase (when (<= (lives object) 0) (no-win))))))
  (add-object
   (make-instance 'message
                  :pos (vec -8.0 0.0)
                  :color '(0.0 1.0 0.0)
                  :text "GOOD JOB"
                  :action #'next-world)))

(defun game-over ()
  (add-object
   (make-instance 'message
                  :pos (vec -8.0 0.0)
                  :color '(1.0 0.0 0.0)
                  :text "GAME OVER"
                  :action #'this-world-again)))
                  
(defclass sqrewy (enemy)
  ((angle :initform 0 :accessor angle)
   (dir :initform '> :accessor dir))
  (:default-initargs :collision-radius 2 :explosion-color (list 0.0 0.5 0.5)))

(defmethod update :after ((sq sqrewy))
  (ecase (dir sq)
    (<
     (if (> (angle sq) 30)
         (setf (dir sq) '>)
         (incf (angle sq) 2)))
    (>
     (if (< (angle sq) -30)
         (setf (dir sq) '<)
         (decf (angle sq) 2)))))

(defmethod render ((sq sqrewy))
  (gl:with-pushed-matrix
    (with-vec (x y (pos sq))
      (gl:translate x y 0.0))
    (gl:rotate (angle sq) 0 0 1)
    (apply #'gl:color (explosion-color sq))
    (gl:with-primitive :line-loop
      (gl:vertex -2.0 -2.0)
      (gl:vertex +2.0 -2.0)
      (gl:vertex +2.0 +2.0)
      (gl:vertex -2.0 +2.0))))

(defclass sqarry (enemy)
  ((angle :initform 0.0 :accessor angle))
  (:default-initargs :collision-radius 2 :explosion-color (list 0.8 0.3 0.4)))

(defmethod update :after ((sq sqarry))
  (setf (angle sq) (mod+ (angle sq) -2.0 360.0)))

(defmethod render ((sq sqarry))
  (gl:with-pushed-matrix
    (with-vec (x y (pos sq))
      (gl:translate x y 0.0))
    (gl:rotate (angle sq) 0.0 0.0 1.0)
    (apply #'gl:color (explosion-color sq))
    (draw-star 2.0 5 2)))


;;;; Waves

(defclass wave ()
  ((enemies :initarg :enemies :accessor enemies)
   (start-tick :initarg :start-tick :accessor start-tick)
   (wait-ticks :initarg :wait-ticks :accessor wait-ticks)
   (last-release-tick :initform nil :accessor last-release-tick)))

(defmethod update ((w wave))
  (let ((tick *tick*))
    (when (>= tick (start-tick w))
      (cond ((null (enemies w))
             (remove-object w))
            ((or (null (last-release-tick w))
                 (>= (- tick (last-release-tick w)) (wait-ticks w)))
             (release-an-enemy w)
             (setf (last-release-tick w) tick))))))

(defun release-an-enemy (wave)
  (add-object (pop (enemies wave))))


;;;; Explosions

(defclass explosion ()
  ((particles :accessor particles)
   (color :initarg :color :accessor color)
   (center :initarg :center :accessor center)))

(defclass explosion-particle ()
  ((pos :initarg :pos :accessor pos)
   (vel :initarg :vel :accessor vel)
   (acc :initarg :acc :accessor acc)
   (ang :initarg :ang :accessor ang)
   (mul :initarg :mul :accessor mul)
   (nrg :initarg :nrg :accessor nrg)))

(defmethod initialize-instance :after ((e explosion) &rest initargs &key number-of-particles center &allow-other-keys)
  (declare (ignore initargs))
  (with-vec (cx cy center)
    (flet ((new-particle ()
             (let ((a (random 360.0))
                   (e (+ 50 (random 20)))
                   (v (random 1.0)))
               (make-instance 'explosion-particle
                              :pos (vec cx cy)
                              :vel (vel-vec v a)
                              :acc (vel-vec (random (/ v e)) (- 360.0 a))
                              :nrg e
                              :ang (random 360.0)
                              :mul (/ (- (random 2.0) 1.0) 3.0)))))
      (setf (particles e) (make-array number-of-particles))
      (map-into (particles e) #'new-particle))))

(defmethod update ((e explosion))
  (let ((dead 0))
    (flet ((update-particle (p)
             (cond ((plusp (nrg p))
                    (vec+= (pos p) (vel p))
                    (vec+= (vel p) (acc p))
                    (decf (nrg p))
                    (incf (ang p) (* (mul p) (nrg p))))
                   (t
                    (incf dead)))))
      (map nil #'update-particle (particles e)))
    (when (= dead (length (particles e)))
      (remove-object e))))

(defmethod render ((e explosion))
  (destructuring-bind (r g b) (color e)
    (flet ((render-particle (p)
             (when (plusp (nrg p))
               (gl:with-pushed-matrix
                 (with-vec (px py (pos p))
                   (gl:translate px py 0.0))
                 (gl:rotate (ang p) 0.0 0.0 1.0)
                 (gl:color r g b (/ (nrg p) 70.0))
                 (gl:with-primitive :line-loop
                   (gl:vertex -1.0 0.0)
                   (gl:vertex +1.0 0.0)
                   (gl:vertex  0.0 1.0))))))
      (map nil #'render-particle (particles e)))))


;;;; Message

(defclass message (clickable-object)
  ((pos :initarg :pos :accessor pos)
   (color :initarg :color :accessor color)
   (text :initarg :text :accessor text)
   (action :initarg :action :accessor action))
  (:default-initargs :action #'nothing))

(defmethod render ((m message))
  (gl:with-pushed-matrix
    (apply #'gl:color (color m))
    (with-vec (x y (pos m))
      (display-text x y "~A" (text m)))))

(define-symmetric collide-p ((message message) (mouse mouse))
  t)

(define-symmetric collide-p ((message message) (tower tower))
  t)

(define-symmetric collide-p ((message message) (object t))
  nil)

(defmethod select ((m message) op pos)
  (declare (ignore pos))
  (ecase op
    (:obtain
     (remove-object m)
     (funcall (action m)))
    (:release)
    (:move)))


;;;; Levels

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
      `(progn
         (defclass ,name (world) ())
         (defmethod shared-initialize :after ((world ,name) slot-names &rest initargs)
           (declare (ignore slot-names initargs))
           (let ,object-names
             ,@(loop for object in objects
                     for name = (object-name object)
                     for make = `(make-instance ',(object-class-name object) ,@(object-initargs object))
                     collect `(add-object ,(if name `(setf ,name ,make) make) world))))
         ',name))))

(define-level level-1
  (homebase :lives 2 :pos (vec -50.0 -50.0))
  (path :named path :spline '(0.0 100.0 10.0 10.0 -10.0 -10.0 -50.0 -50.0))
  (player :cash 15)
  (tower-control)
  (tower-factory :kind 'blaster-tower :pos (vec -60.0 -85.0)
                 :buy-prices #(5 5 7 10 15 20 30)
                 :sell-prices #(0 2 5 7 10 15 25 35))
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
  (player :cash 30)
  (tower-control)
  (tower-factory :kind 'blaster-tower :pos (vec -60.0 -85.0)
                 :buy-prices #(5 5 7 10 15 20 30)
                 :sell-prices #(0 2 5 7 10 15 25 35))
  (tower-factory :kind 'laser-tower :pos (vec -40.0 -85.0)
                 :buy-prices #(15 15 20 25 30 40 55)
                 :sell-prices #(0 10 15 20 25 30 40 55))
  (wave :start-tick 100 :wait-ticks 50 :enemies
        (loop repeat 10 collecting
              (make-instance 'sqrewy
                             :pos (vec 0.0 100.0)
                             :speed 0.5
                             :path path
                             :hit-points 10
                             :cash-reward 2)))
  (wave :start-tick 400 :wait-ticks 40 :enemies
        (loop repeat 5 collecting
              (make-instance 'sqrewy
                             :pos (vec 0.0 100.0)
                             :speed 0.6
                             :path path
                             :hit-points 15
                             :cash-reward 5)))
  (wave :start-tick 800 :wait-ticks 50 :enemies
        (loop repeat 10 collecting
              (make-instance 'sqarry
                             :pos (vec 0.0 100.0)
                             :speed 0.8
                             :path path
                             :hit-points 30
                             :cash-reward 10)))
  (grid))

(define-level level-3
  (homebase :lives 1 :pos (vec 64.5 -33.25))
  (path :named path :vertices
        (let ((vertices
               #((0.6 . 0.091392085) (0.56413937 . 0.07984138) (0.55053055 . 0.08290954)
                 (0.53867435 . 0.08898568) (0.52772456 . 0.095904075) (0.516835 . 0.10149895)
                 (0.50515926 . 0.10360454) (0.49185124 . 0.100055106)
                 (0.47606462 . 0.08868489) (0.45695317 . 0.067328125)
                 (0.45695317 . 0.067328125)
                 (0.45564455 . 0.056675784) (0.45381254 . 0.046812505)
                 (0.45145705 . 0.037738286) (0.44857815 . 0.029453129)
                 (0.4451758 . 0.021957032) (0.4432783 . 0.018504884)(0.44125003 . 0.015250001)
                 (0.43909085 . 0.012192383) (0.4368008 . 0.009332032)
                 (0.43437988 . 0.006668946) (0.43182814 . 0.0042031254)
                 (0.42914551 . 0.0019345705) (0.42633206 . -1.3671874e-4)
                 (0.4233877 . -0.0020107422) (0.42031252 . -0.0036875003)
                 (0.41710645 . -0.0051669925) (0.41376954 . -0.006449219)
                 (0.41030177 . -0.00753418) (0.40670314 . -0.008421876)
                 (0.40297365 . -0.009112306) (0.3991133 . -0.009605469)
                 (0.39512208 . -0.009901367) (0.39100003 . -0.010000001)
                 (0.38678908 . -0.009867188) (0.38265628 . -0.009468751)
                 (0.37860158 . -0.008804688) (0.37462503 . -0.007875001)
                 (0.3707266 . -0.0066796886) (0.3669063 . -0.005218751)
                 (0.36316407 . -0.003492188) (0.35950002 . -0.0015) (0.352625 . 0.00284375)
                 (0.34650004 . 0.007375) (0.341125 . 0.012093751) (0.33871877 . 0.014523438)
                 (0.33650002 . 0.017) (0.33438283 . 0.019648438) (0.33228126 . 0.022593752)
                 (0.328125 . 0.029375) (0.32403126 . 0.037343748) (0.32000002 . 0.0465)
                 (0.31631252 . 0.055625) (0.31325 . 0.0635) (0.31081253 . 0.070125)
                 (0.30900002 . 0.075500004) (0.307375 . 0.08096875) (0.30550003 . 0.08787501)
                 (0.30337504 . 0.09621876) (0.30100003 . 0.106000006) (0.24300002 . 0.333)
                 (0.24100001 . 0.333) (0.113000005 . -0.001) (0.028 . -0.001)
                 (0.20600002 . 0.45000002) (0.20475 . 0.45425) (0.20300001 . 0.46100003)
                 (0.20075001 . 0.47025004) (0.19800001 . 0.48200002) (0.19503126 . 0.494375)
                 (0.19212501 . 0.5055) (0.18928126 . 0.515375) (0.18650001 . 0.52400005)
                 (0.1835 . 0.5323125) (0.18 . 0.54125005) (0.176 . 0.55081254)
                 (0.17150001 . 0.56100005) (0.16903909 . 0.56608605) (0.16640627 . 0.5708438)
                 (0.16360158 . 0.5752735) (0.16062501 . 0.579375) (0.15747657 . 0.5831485)
                 (0.15415627 . 0.58659375) (0.15066406 . 0.58971095) (0.147 . 0.59250003)
                 (0.1431875 . 0.59496105) (0.13925 . 0.5970938) (0.1351875 . 0.59889853)
                 (0.131 . 0.60037506) (0.1266875 . 0.6015235) (0.12225 . 0.6023438)
                 (0.11768751 . 0.602836) (0.113000005 . 0.60300004) (0.108582035 . 0.602793)
                 (0.10643457 . 0.60253423) (0.104328126 . 0.6021719) (0.10226269 . 0.60170615)
                 (0.100238286 . 0.6011368) (0.09825489 . 0.600464) (0.09631251 . 0.5996876)
                 (0.094411135 . 0.5988077) (0.092550784 . 0.5978243) (0.09073145 . 0.5967374)
                 (0.08895313 . 0.5955469) (0.08551954 . 0.59285545) (0.08225001 . 0.58975005)
                 (0.07914454 . 0.5862305) (0.07620313 . 0.58229697) (0.073425785 . 0.5779493)
                 (0.07081251 . 0.5731876) (0.06836329 . 0.56801176) (0.06607813 . 0.5624219)
                 (0.062000003 . 0.55) (0.043 . 0.55) (0.043 . 0.56700003)
                 (0.04328125 . 0.57734376) (0.044125 . 0.58737504) (0.04553125 . 0.5970938)
                 (0.0475 . 0.6065) (0.050031252 . 0.61559373) (0.053125 . 0.624375)
                 (0.056781255 . 0.63284373) (0.061000004 . 0.64100003)
                 (0.06333594 . 0.64487505) (0.06584375 . 0.6485001) (0.06852344 . 0.6518751)
                 (0.071375005 . 0.6550001) (0.07439844 . 0.6578751) (0.07759376 . 0.6605001)
                 (0.080960944 . 0.66287506) (0.08450001 . 0.6650001) (0.08821094 . 0.66687506)
                 (0.09209376 . 0.66850007) (0.096148446 . 0.669875) (0.10037501 . 0.671)
                 (0.10477345 . 0.671875) (0.10934376 . 0.6725) (0.11408594 . 0.67287505)
                 (0.119 . 0.67300004) (0.12284179 . 0.6728828) (0.12661718 . 0.67253125)
                 (0.13032617 . 0.67194533) (0.13396874 . 0.67112505) (0.13754493 . 0.67007035)
                 (0.14105469 . 0.6687813) (0.14449805 . 0.66725785) (0.14787501 . 0.66550004)
                 (0.15118556 . 0.6635078) (0.1544297 . 0.6612812) (0.15760744 . 0.65882033)
                 (0.16071875 . 0.656125) (0.16674219 . 0.6500312) (0.17250001 . 0.643)
                 (0.17789845 . 0.6354219) (0.18284374 . 0.6276875) (0.18733594 . 0.6197969)
                 (0.191375 . 0.61175) (0.19496095 . 0.60354686) (0.19809376 . 0.59518754)
                 (0.20077345 . 0.58667195) (0.20300001 . 0.578) (0.20706251 . 0.5609375)
                 (0.21125 . 0.54475003) (0.21556251 . 0.52943754) (0.22000001 . 0.51500005)
                 (0.31800002 . 0.17300001) (0.31993753 . 0.1673125) (0.32175004 . 0.16125)
                 (0.32343754 . 0.1548125) (0.32500002 . 0.148) (0.32671875 . 0.14096875)
                 (0.328875 . 0.13387501) (0.33146876 . 0.12671876) (0.3345 . 0.119500004)
                 (0.3378125 . 0.112406254) (0.34125 . 0.105625) (0.3448125 . 0.09915625)
                 (0.3485 . 0.093) (0.3525 . 0.08725) (0.35468748 . 0.0845625) (0.357 . 0.082)
                 (0.3594375 . 0.0795625) (0.362 . 0.077250004) (0.3646875 . 0.0750625)
                 (0.3675 . 0.07300001) (0.37041408 . 0.07112501) (0.37340626 . 0.06950001)
                 (0.3764766 . 0.06812501) (0.37962502 . 0.067) (0.38285157 . 0.066125005)
                 (0.38615626 . 0.065500006) (0.38953906 . 0.065125) (0.393 . 0.065000005)
                 (0.3955879 . 0.06507227) (0.39810157 . 0.065289065) (0.40054104 . 0.065650396)
                 (0.40290627 . 0.06615625) (0.40519726 . 0.066806644) (0.40741408 . 0.06760157)
                 (0.40955666 . 0.06854102) (0.41162503 . 0.069625005) (0.41361916 . 0.07085352)
                 (0.4155391 . 0.07222657) (0.41738477 . 0.07374415) (0.41915625 . 0.07540625)
                 (0.42085353 . 0.07721289) (0.42247656 . 0.079164065) (0.4240254 . 0.08125977)
                 (0.4255 . 0.083500005) (0.42821094 . 0.08816407) (0.43059376 . 0.09290625)
                 (0.43264845 . 0.09772657) (0.434375 . 0.102625005) (0.43577343 . 0.10760157)
                 (0.43684375 . 0.11265625) (0.43758595 . 0.11778907) (0.43800002 . 0.123)
                 (0.45800003 . 0.123))))
          (map 'vector (lambda (v) (vec+ (vec* v 250.0) (vec -50.0 -70.0))) vertices)))
  (player :cash 15)
  (tower-control)
  (tower-factory :kind 'blaster-tower :pos (vec -60.0 -85.0)
                 :buy-prices #(5 2 5 7 9 12 15)
                 :sell-prices #(0 2 3 4 5 6 7 9))
  (tower-factory :kind 'laser-tower :pos (vec -40.0 -85.0)
                 :buy-prices #(15 5 8 10 13 17 20)
                 :sell-prices #(0 5 6 9 10 13 15 20))
  (wave :start-tick 150 :wait-ticks 45 :enemies
        (loop repeat 20 collecting
              (make-instance 'sqrewy
                             :pos (vec 100.0 -47.151978)
                             :speed 0.4
                             :path path
                             :hit-points 10
                             :cash-reward 1)))
   (wave :start-tick 800 :wait-ticks 40 :enemies
         (loop repeat 10 collecting
               (make-instance 'sqarry
                              :pos (vec 100.0 -47.151978)
                              :speed 0.7
                              :path path
                              :hit-points 12
                              :cash-reward 2)))
   (wave :start-tick 1500 :wait-ticks 70 :enemies
         (loop repeat 10 collecting
               (make-instance 'sqrewy
                              :pos (vec 100.0 -47.151978)
                              :speed 0.5
                              :path path
                              :hit-points 30
                              :cash-reward 3)))
   (wave :start-tick 2000 :wait-ticks 50 :enemies
         (loop repeat 10 collecting
               (make-instance 'sqarry
                              :pos (vec 100.0 -47.151978)
                              :speed 0.7
                              :path path
                              :hit-points 30
                              :cash-reward 4)))
   (wave :start-tick 2500 :wait-ticks 40 :enemies
         (loop repeat 20 collecting
               (make-instance 'sqrewy
                              :pos (vec 100.0 -47.151978)
                              :speed 0.6
                              :path path
                              :hit-points 50
                              :cash-reward 6)))
   (wave :start-tick 3200 :wait-ticks 30 :enemies
         (loop repeat 20 collecting
               (make-instance 'sqarry
                              :pos (vec 100.0 -47.151978)
                              :speed 0.9
                              :path path
                              :hit-points 70
                              :cash-reward 7)))
  (grid))


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

(defmethod select ((sp spline-point) op pos)
  (ecase op
    (:obtain)
    (:release)
    (:move (vec-assign (pos sp) (x pos) (y pos)))))

(define-level spline-editor
  (spliner)
  (grid))

(defclass spline-editor-window (game-window)
  ((mouse :initform (make-instance 'mouse) :accessor mouse)))

(defmethod glut:keyboard ((w spline-editor-window) key x y)
  (let ((*world* (world w)))
    (let ((mouse (mouse w))
          (spliner (do-objects (x :type 'spliner) (return x))))
      (multiple-value-bind (x y) (glu:un-project x y 0.0)
        (vec-assign (pos (mouse w)) x (- y)))
      (case key
        (#\a
         (let ((sp (make-instance 'spline-point :pos (copy-vec (pos mouse)))))
           (appendf (points spliner) (list sp))
           (add-object sp)))
        (#\d
         (when-let (sp (pick-object (mouse w)))
           (deletef (points spliner) sp)
           (remove-object sp)))
        (#\p
         (format t "(")
         (dolist (sp (points spliner))
           (with-vec (x y (pos sp))
             (format t " ~,3F ~,3F~%" x y)))
         (format t ")~%")
         (finish-output))
        (t (call-next-method))))))

(defun spline-editor ()
  (glut:display-window
   (make-instance 'spline-editor-window :world 'spline-editor)))
