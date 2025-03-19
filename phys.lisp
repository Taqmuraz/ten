(in-package #:ten)

(defun sphere-vs-sphere (a-rad a-center b-rad b-center)
  (lets (
      d (v- a-center b-center)
      l (len d)
      dist (- l (+ a-rad b-rad))
    )
    (when (<= dist 0)
      (lets (
          dir (norm d)
          ap (-> dir v- (v* (vvv a-rad)) (v+ a-center))
          bp (-> dir (v* (vvv b-rad)) (v+ b-center))
        )
        (make-assoc
          :dist dist
          :point (v/ (v+ ap bp) (vvv 2))
        )
      )
    )
  )
)
