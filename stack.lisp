(in-package #:ten)

(defun stack-push-matrix (stack m)
  (cons (mul-mat-4x4 (car stack) m) stack)
)

(defmacro with-stack-push (stack m &body forms)
  (lets (
      r (gensym)
      p (typecase stack
        (list (list (first stack) (list 'stack-push-matrix (second stack) m)))
        (symbol (list stack (list 'stack-push-matrix stack m)))
        (t (error "Stack var must be a symbol or a list (var val)"))
      )
    )
    `(let (,p) ,@forms)
  )
)
