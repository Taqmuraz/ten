(lets (
    ms (-> "res/castle/castle.fbx" load-model-data model-mesh-shapes)
    ss (loop for i from 0 below 20 collect (sphere-shape 1 (vector i 10 0)))
    as (append ss ms)
    dt 1/50
    cycles 100
  )
  (format t "Testing with process-forces :~%")
  (time (loop repeat cycles do (-> as (process-forces dt) shapes-tree (shapes-tree-sim dt))))
  (format t "Testing without process-forces :~%")
  (time (loop repeat cycles do (-> as shapes-tree (shapes-tree-sim dt))))
)
