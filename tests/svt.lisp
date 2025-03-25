(lets (
    rad 1
    center #(0 1/2 -1/2)
    a #(0 0 0)
    b #(1 0 0)
    c #(0 1 0)
    n #(0 0 -1)
    test (sphere-vs-triangle rad center (list a b c) n)
  )
  (format t "Sphere test result : ~A~%" test)
)
