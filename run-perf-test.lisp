(macrolet (
    (perf (tag &body body)
      `(progn
        (sb-ext:gc :full t)
        (lets (
            s (get-time)
            r (progn ,@body)
          )
          (format t "Finished ~A, took ~,1f sec~%" ,tag (- (get-time) s))
          r
        )
      )
    )
    (perfc (cycles tag &body body)
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
  (format t "~%")
  (lets (
      ms (perf "load-model-data + model-mesh-shapes" (-> "res/castle/castle.fbx" load-model-data model-mesh-shapes))
      ss (perf "sphere-shape x20" (loop for i from 0 below 20 collect (sphere-shape 1/4 (vector i 10 0))))
      as (append ss ms)
      dt 1/50
      st (shapes-tree as)
      c 200
    )
    (perfc c "process-forces" (process-forces as dt))
    (perfc c "shapes-tree" (shapes-tree as))
    (perfc c "shapes-tree-contacts" (shapes-tree-contacts st))
    (perfc c "shapes-tree-sim" (shapes-tree-sim st dt))
    (perfc c "shapes-tree + shapes-tree-sim" (-> as shapes-tree (shapes-tree-sim dt)))
    (perfc c "process-forces + shapes-tree + shapes-tree-sim" (-> as (process-forces dt) shapes-tree (shapes-tree-sim dt)))
  )
)
