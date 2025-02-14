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
  (gl:clear-color 1/2 1/2 1/2 1)
  (gl:clear :color-buffer-bit :depth-buffer-bit)
  (glut:swap-buffers)
)
