;;;; ten.lisp

(in-package #:ten)

(defun start-demo (file)
  (glut:display-window (make-instance 'window :res (hash :file file)))
)
