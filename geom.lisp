(in-package #:ten)

(defun triangulate (faces)
  (loop for f across faces append
    (lets (
        v (coerce f 'vector)
        tp (type-of f)
        l (length v)
      )
      (cases l
        3 (list f)
        4 (list (select-keys v 0 1 2)
                (select-keys v 2 3 0))
        5 (list (select-keys v 0 1 4)
                (select-keys v 4 1 3)
                (select-keys v 1 2 3))
        6 (list (select-keys v 0 1 5)
                (select-keys v 5 1 2)
                (select-keys v 2 4 5)
                (select-keys v 2 3 4))
        t (error (format nil "Cannot triangulate face of ~A vertices" l))
      )
    )
    into r
    finally (return (coerce r 'vector))
  )
)
