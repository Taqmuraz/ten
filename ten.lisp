;;;; ten.lisp

(in-package #:ten)

(defun start-demo (file)
  (lets (
      scene (load-model-data file)
      res (hash :scene scene)
    )
    (glut:display-window (make-instance 'window :res res))
  )
)
