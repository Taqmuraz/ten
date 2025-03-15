(in-package #:ten)

(defclass window (glut:window) (
    (state :accessor state :initarg :state)
    (res :accessor res :initarg :res)
  )
  (:default-initargs
    :width 800
    :height 600
    :title "ten"
    :mode '(:double :rgb :depth :multisample)
  )
)

(defun get-time ()
  (float (/ (get-internal-real-time) (float internal-time-units-per-second)))
)

(defparameter *fps* 0)
(defparameter *last-fps-sec* 0)
(defparameter *last-fps* 0)
(defun fps ()
  (lets (s (floor (get-time)))
    (if (= *last-fps-sec* s)
      (progn (incf *fps* 1) *last-fps*)
      (progn
        (setf *last-fps* *fps*)
        (setf *last-fps-sec* s)
        (setf *fps* 1)
        *last-fps*))))

(defmethod glut:display-window :before ((window window))
  (gl:polygon-mode :front :fill)
  (gl:cull-face :back)
  (gl:enable :texture-2d :depth-test :cull-face)
  (gl:disable :color-material)
  (lets (
      shaders (hash
        :instancing-static (load-shader-to-gl
          (uiop:read-file-string "res/shaders/instancing_vertex.glsl")
          (uiop:read-file-string "res/shaders/instancing_fragment.glsl")
        )
        :instancing-skin (load-shader-to-gl
          (uiop:read-file-string "res/shaders/skin_instancing_vertex.glsl")
          (uiop:read-file-string "res/shaders/instancing_fragment.glsl")
        )
      )
      model (-> window res (map-key :file) load-model-data load-model-to-gl load-gl-group)
      anim (-> model :anims vals second)
      state (make-assoc
        :campos #(0 0 -5)
      )
    )
    (setf (res window) (hash :scene model :anim anim :shaders shaders))
    (setf (state window) state)
  )
)

(defmethod glut:display ((window window))
  (setf (glut:title window) (format nil "ten, fps = ~A" (fps)))
  (with-maps-keys (((scene anim shaders) (res window))
                   ((campos) (state window)))
    (lets (
        state (state window)
        w (glut:width window)
        h (glut:height window)
        time (get-time)
        proj-mat (mat-perspective (/ w h) (/ pi 3) 1 1000)
        mat-stack (last-> campos v- (applyv #'mat-translation) list)
        rot-mat (mat-rotation 0 pi 0)
        instances (with-stack-push mat-stack rot-mat
          (loop for i from 0 below 1000 collect
            (make-assoc
              :anim (make-assoc :index 1 :time (+ time (/ i 50)) :length (-> anim :length))
              :pose (animate anim (+ time (/ i 50)))
              :root (mul-mat-4x4 (mat-translation (- 5 (mod i 10)) -3 (floor i 10)) (car mat-stack))
            )
          )
        )
        state (update state (sfun c v+ c (wasd-xz)) :campos)
      )
      (gl:clear-color 1/2 1/2 1/2 1)
      (gl:clear :color-buffer-bit :depth-buffer-bit)
      (gl:viewport 0 0 w h)
      (-> window res :scene
        (display-gl-group-instanced
          shaders
          instances
          :proj proj-mat))
      (glut:swap-buffers)
      (refresh-keyboard)
      (setf (state window) state)
    )
  )
)

(defmethod glut:reshape ((window window) w h)
  (setf (glut:width window) w)
  (setf (glut:height window) h)
)

(defmethod glut:idle ((window window))
  (glut:post-redisplay)
)

(defmethod glut:visibility ((window window) state)
  (cases state
    :visible (glut:enable-event window :idle)
    t (glut:disable-event window :idle)
  )
)

(lets (
    keys (hash)
  )
  (defmethod glut:keyboard ((window window) key x y)
    (setf (gethash key keys) :down)
  )
  (defun refresh-keyboard ()
    (setf keys (update-vals keys (sfun s cases s :down :up t nil)))
  )
  (defun key-state (key)
    (map-key keys key)
  )
  (defun wasd-xz ()
    (lets (r (vector 0 0 0))
      (when (key-state #\w) (incf (aref r 2) 1))
      (when (key-state #\a) (incf (aref r 0) -1))
      (when (key-state #\s) (incf (aref r 2) -1))
      (when (key-state #\d) (incf (aref r 0) 1))
      r
    )
  )
)
