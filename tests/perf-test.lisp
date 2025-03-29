(ql:quickload "cl-ten")(in-package #:ten)

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
      m "res/arena/arena.fbx"
      ms (perf "load-model-data + model-mesh-shapes"
        (-> m load-model-data model-mesh-shapes)
      )
      ss (perf "sphere-shape x20"
        (loop for i from 0 below 200 collect (char-shape 1/2 2 (vector (floor i 10) 0 (mod i 10))))
      )
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
