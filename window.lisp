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
        :static (load-shader-to-gl
          (uiop:read-file-string "res/shaders/vertex.glsl")
          (uiop:read-file-string "res/shaders/fragment.glsl")
        )
        :skin (load-shader-to-gl
          (uiop:read-file-string "res/shaders/skin_vertex.glsl")
          (uiop:read-file-string "res/shaders/fragment.glsl")
        )
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
    )
    (setf (res window) (hash :scene model :anim anim :shaders shaders))
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
      rot-mat (mat-rotation 0 pi 0)
      scene (-> window res :scene)
      anim (-> window res :anim)
      shaders (-> window res :shaders)
    )
    (gl:clear-color 1/2 1/2 1/2 1)
    (gl:clear :color-buffer-bit :depth-buffer-bit)
    (gl:viewport 0 0 w h)
    (loop for i from 0 below 1
      for offset = (mat-translation (mod i 10) -2 (floor i 10)) do
      (with-stack-push mat-stack (mul-mat-4x4 offset rot-mat)
        (-> window res :scene
          (display-gl-group
            shaders
            :root (car mat-stack)
            :proj proj-mat
            :pose (animate anim time)))))
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
