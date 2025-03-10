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

(cffi:defcallback glut-timer-func :void ((val :unsigned-int))
  (glut:post-redisplay)
  (glut:timer-func 1 (cffi:callback glut-timer-func) 0)
)

(defmethod glut:display-window :before ((window window))
  (gl:polygon-mode :front :fill)
  (gl:enable :texture-2d :depth-test)
  (gl:disable :color-material)
  (lets (
      shaders (hash
        :static (load-shader-to-gl
          (uiop:read-file-string "res/shaders/vertex.glsl")
          (uiop:read-file-string "res/shaders/fragment.glsl")
        )
        :skin (load-shader-to-gl
          (uiop:read-file-string "res/shaders/skin_vertex.glsl")
          (uiop:read-file-string "res/shaders/fragment.glsl")
        )
      )
      model (-> window res (map-key :file) load-model-data (load-model-to-gl shaders))
      anim (-> model :gl-anims vals first)
    )
    (setf (res window) (hash :scene model :anim anim))
  )
  (glut:timer-func 0 (cffi:callback glut-timer-func) 0)
  (glut:post-redisplay)
)

(defmethod glut:display ((window window))
  (setf (glut:title window) (format nil "ten, fps = ~A" (fps)))
  (lets (
      w (glut:width window)
      h (glut:height window)
      time (get-time)
      proj-mat (mat-perspective (/ w h) (/ pi 3) 1 1000)
      mat-stack (list (mat-translation 0 0 5))
      rot-mat (mat-rotation 0 (* time pi 1/16) 0)
      scene (-> window res :scene)
      anim (-> window res :anim)
    )
    (gl:clear-color 1/2 1/2 1/2 1)
    (gl:clear :color-buffer-bit :depth-buffer-bit)
    (gl:viewport 0 0 w h)
    (loop for i from 0 below 500
      for offset = (mat-translation (- 5 (mod i 10)) -2 (floor i 10)) do
      (with-stack-push mat-stack (mul-mat-4x4 offset rot-mat)
        (-> window res :scene
          (display-gl-model
            :mat-stack mat-stack
            :proj-mat proj-mat
            :gl-pose (animate anim time)))))
    (glut:swap-buffers)
  )
)

(defmethod glut:reshape ((window window) w h)
  (setf (glut:width window) w)
  (setf (glut:height window) h)
)
