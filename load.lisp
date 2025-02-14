(in-package #:ten)

(defun load-model-data (file)
  (labels (
      (load-weights (ws)
        (loop for w across ws collect
          (list (ai:id w) (ai:weight w))
        )
      )
      (load-bones (bs)
        (when bs
          (loop for b across bs collect
            (make-assoc
              :name (ai:name b)
              :weights (-> b ai:weights load-weights)
            )
          )
        )
      )
      (load-submesh (m)
        (make-assoc
          :verts (ai:vertices m)
          :normals (ai:normals m)
          :uvs (ai:texture-coords m)
          :indices (ai:faces m)
          :bones (-> m ai:bones load-bones)
        )
      )
    )
    (lets (
        scene (-> file truename cffi-sys:native-namestring ai:import-into-lisp)
        meshes (ai:meshes scene)
      )
      (loop for m across meshes collect (load-submesh m))
    )
  )
)
