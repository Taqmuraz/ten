;;;; ten.lisp

(in-package #:ten)

(defun start-demo (file)
  (lets (
      scene (load-model-data file)
      gl-scene (load-model-to-gl scene)
      res (hash :scene scene :gl-scene gl-scene)
    )
    (glut:display-window (make-instance 'window :res res))
  )
)
