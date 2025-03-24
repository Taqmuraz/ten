(lets (
    ms (-> "res/castle/castle.fbx" load-model-data model-mesh-shapes)
    ss (loop for i from 0 below 20 collect (sphere-shape 1 (vector i 10 0)))
    as (append ss ms)
    dt 1/50
    st (shapes-tree as)
  )
  (macrolet (
      (perf (cycles tag &body body) `(progn
          (sb-ext:gc :full t)
          (format t "Testing ~A for ~A cycles~%" ,tag ,cycles)
          (time (loop repeat ,cycles do ,@body))
        )
      )
    )
    (perf 100 "process-forces + shapes-tree + shapes-tree-sim" (-> as (process-forces dt) shapes-tree (shapes-tree-sim dt)))
    (perf 100 "shapes-tree + shapes-tree-sim" (-> as shapes-tree (shapes-tree-sim dt)))
    (perf 100 "shapes-tree" (shapes-tree as))
    (perf 100 "process-forces" (process-forces as dt))
    (perf 100 "shapes-tree-sim" (shapes-tree-sim st dt))
    (perf 100 "shapes-tree-contacts" (shapes-tree-contacts st))
  )
)
