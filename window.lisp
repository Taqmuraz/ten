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
  (setf (res window)
    (hash :scene
      (-> window res (map-key :file) load-model-data load-model-to-gl)))
)

(defmethod glut:display ((window window))
  (lets (
      w (glut:width window)
      h (glut:height window)
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
      (set-proj-matrix (mat-perspective (/ w h) (/ pi 3) 1 1000))
      (with-stack-push (mat-translation 0 (-> #'get-time funcall sin (- 2)) 10)
        (-> window res (map-key :scene) display-gl-model)
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
