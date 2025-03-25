(defun closest-point (p a b c)
  (lets (
      ab (v- b a)
      ac (v- c a)
      ap (v- p a)

      d1 (dot ab ap)
      d2 (dot ac ap)
      _ (when (and (<= d1 0) (<= d2 0)) (return-from closest-point a))

      bp (v- p b)
      d3 (dot ab bp)
      d4 (dot ac bp)
      _ (when (and (>= d3 0) (<= d4 d3)) (return-from closest-point b))

      cp (v- p c)
      d5 (dot ab cp)
      d6 (dot ac cp)
      _ (when (and (>= d6 0) (<= d5 d6)) (return-from closest-point c))

      vc (- (* d1 d4) (* d3 d2))
      _ (when (and (<= vc 0) (>= d1 0) (<= d3 0))
        (lets (
            v (/ d1 (- d1 d3))
          )
          (return-from closest-point (v+ a (v* (vvv v) ab)))
        )
      )
        
      vb (- (* d5 d2) (* d1 d6))
      _ (when (and (<= vb 0) (>= d2 0) (<= d6 0))
        (lets (
            v (/ d2 (- d2 d6))
          )
          (return-from closest-point (v+ a (v* (vvv v) ac)))
        )
      )
        
      va (- (* d3 d6) (* d5 d4))
      _ (when (and (<= va 0) (>= (- d4 d3) 0) (>= (- d5 d6) 0))
        (lets (
            v (/ (- d4 d3) (+ (- d4 d3) (- d5 d6)))
          )
          (return-from closest-point (v+ b (v* v (v- c b))))
        )
      )
      denom (/ (+ va vb vc))
      v (* vb denom)
      w (* vc denom)
    )
    (v+ a (v* (vvv v) ab) (v* (vvv w) ac))
  )
)

(print (closest-point #(-1 10 -1) #(0 0 0) #(1 0 0) #(0 1 0)))
