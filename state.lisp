(in-package #:ten)

(defparameter *matrix-stack* (list (mat-identity 4)))
(defparameter *proj-matrix* (mat-identity 4))

(defun set-proj-matrix (m) (setf *proj-matrix* m))
(defun proj-matrix () *proj-matrix*)

(defun stack-push-matrix ()
  (push (car *matrix-stack*) *matrix-stack*)
)

(defun stack-pop-matrix ()
  (pop *matrix-stack*)
)

(defun stack-peek-matrix ()
  (car *matrix-stack*)
)

(defun stack-mul-matrix (m)
  (setf (car *matrix-stack*) (mul-mat (car *matrix-stack*) m))
)

(defun stack-pmul (m)
  (stack-push-matrix)
  (stack-mul-matrix m)
)

(defmacro with-stack-push (m &body forms)
  (lets (r (gensym))
    `(let (,r) (progn
      (stack-push-matrix)
      (stack-mul-matrix ,m)
      (setf ,r (progn ,@forms))
      (stack-pop-matrix)
      ,r
    ))
  )
)
