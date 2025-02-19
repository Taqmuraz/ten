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
    )
    (setf (res window)
      (hash :scene
        (-> window res (map-key :file) load-model-data
          (load-model-to-gl shaders))))
  )
)

(defmethod glut:display ((window window))
  (lets (
      w (glut:width window)
      h (glut:height window)
      proj-mat (mat-perspective (/ w h) (/ pi 3) 1 1000)
      mat-stack (list (mat-translation 0 (-> #'get-time funcall sin (- 3)) 10))
      rot-mat (classic-matrix
        (0 0 1 0)
        (0 1 0 0)
        (-1 0 0 0)
        (0 0 0 1)
      )
    )
    (gl:clear-color 1/2 1/2 1/2 1)
    (gl:clear :color-buffer-bit :depth-buffer-bit)
    (gl:matrix-mode :projection)
    (gl:load-identity)
    (glu:perspective 60 (/ w h) 1 1000)
    (gl:viewport 0 0 w h)
    (gl:matrix-mode :modelview)
    (gl:load-identity)
    (gl:with-pushed-matrix
      (gl:translate 0 -2 -10)
      (gl:rotate (* 45 (get-time)) 0 1 0)
      (with-stack-push mat-stack rot-mat
        (-> window res (map-key :scene)
          (display-gl-model
            :mat-stack mat-stack
            :proj-mat proj-mat))
      )
    )
    (glut:swap-buffers)
  )
)

(defmethod glut:idle ((window window))
  (glut:post-redisplay)
)

(defmethod glut:reshape ((window window) w h)
  (setf (glut:width window) w)
  (setf (glut:height window) h)
)
