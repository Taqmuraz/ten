(lets (
    ms (-> "res/castle/castle.fbx" load-model-data model-mesh-shapes)
    ss (loop for i from 0 below 20 collect (sphere-shape 1/4 (vector i 10 0)))
    as (append ss ms)
    dt 1/50
    st (shapes-tree as)
    c 200
  )
  (macrolet (
      (perf (cycles tag &body body)
        `(progn
          (sb-ext:gc :full t)
          (lets (
              s (get-time)
            )
            (loop repeat ,cycles do ,@body)
            (format t "Finished ~A for ~A cycles, took ~,1f sec~%" ,tag ,cycles (- (get-time) s))
          )
        )
      )
    )
    (perf c "process-forces" (process-forces as dt))
    (perf c "shapes-tree" (shapes-tree as))
    (perf c "shapes-tree-contacts" (shapes-tree-contacts st))
    (perf c "shapes-tree-sim" (shapes-tree-sim st dt))
    (perf c "shapes-tree + shapes-tree-sim" (-> as shapes-tree (shapes-tree-sim dt)))
    (perf c "process-forces + shapes-tree + shapes-tree-sim" (-> as (process-forces dt) shapes-tree (shapes-tree-sim dt)))
  )
)
