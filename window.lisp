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
      anim (-> model :anims vals first)
      buffers (alloc-instancing-buffers)
    )
    (setf (res window) (hash :scene model :anim anim :shaders shaders :buffers buffers))
  )
)

(defmethod glut:display ((window window))
  (setf (glut:title window) (format nil "ten, fps = ~A" (fps)))
  (lets (
      w (glut:width window)
      h (glut:height window)
      time (get-time)
      proj-mat (mat-perspective (/ w h) (/ pi 3) 1 1000)
      mat-stack (list (mat-translation 0 0 5))
      rot-mat (mat-rotation 0 (* time pi 1/8) 0)
      scene (-> window res :scene)
      anim (-> window res :anim)
      shaders (-> window res :shaders)
      buffers (-> window res :buffers)
      pose (animate anim time)
      instances (with-stack-push mat-stack rot-mat
        (loop for i from 0 below 1000 collect
          (make-assoc
            :pose pose
            :root (mul-mat-4x4 (mat-translation (- 5 (mod i 10)) -3 (floor i 10)) (car mat-stack))
          )
        )
      )
    )
    (gl:clear-color 1/2 1/2 1/2 1)
    (gl:clear :color-buffer-bit :depth-buffer-bit)
    (gl:viewport 0 0 w h)
    (-> window res :scene
      (display-gl-group-instanced
        shaders
        buffers
        instances
        :proj proj-mat))
    (glut:swap-buffers)
  )
)

(defmethod glut:reshape ((window window) w h)
  (setf (glut:width window) w)
  (setf (glut:height window) h)
)

(defmethod glut:idle ((window window))
  (glut:post-redisplay)
)
