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
          an dir
          bn (v- dir)
          ap (-> bn (v* (vvv a-rad)) (v+ a-center))
          bp (-> an (v* (vvv b-rad)) (v+ b-center))
        )
        (make-assoc
          :dist dist
          :point-a ap
          :point-b bp
          :normal-a an
          :normal-b bn
        )
      )
    )
  )
)

(defun sphere-vs-triangle (rad center a b c)
  (labels (
      (col (v) (make-array (length v)
        :element-type 'single-float
        :initial-contents (loop for e across v collect (coerce e 'single-float))
      ))
      (vcol (v) (col (concatenate 'vector v '(0))))
    )
    (lets (
        tnorm (norm (cross (v- a b) (v- c b)))
        dst (-> center (v- a) (dot tnorm) abs (- rad))
      )
      (when (<= dst 0)
        (lets (
            tmat (rtg-math.matrix4:from-columns
              (vcol (v- a center))
              (vcol (v- b center))
              (vcol (v- c center))
              (col #(0 0 0 1))
            )
            inv (-> tmat rtg-math.matrix4:inverse vec-16->mat-4x4)
            p (transform-vector inv (v- tnorm))
          )
          (when (not (at-least-one p (sfun e < e 0)))
            (make-assoc
              :dist dst
              :point (v+ center (v* tnorm (vvv (- 0 dst rad))))
              :normal tnorm
            )
          )
        )
      )
    )
  )
)
