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

(defmethod glut:display-window :before ((window window))
  (gl:polygon-mode :front :fill)
  (gl:enable :depth-test)
)

(defmethod glut:display ((window window))
  (gl:clear-color 0 0 0 1)
  (gl:clear :color-buffer-bit :depth-buffer-bit)
  (gl:matrix-mode :modelview)
  (gl:load-identity)
  (gl:with-pushed-matrix
    (gl:translate 0 -1/2 0)
    (gl:scale 1/5 1/5 1/5)
    (-> window res (map-key :scene) display-model)
  )
  (glut:swap-buffers)
)
