(in-package #:ten)

(defun load-model-data (file)
  (labels (
      (make-buffer (b) (apply #'concatenate 'vector (coerce b 'list)))
      (load-submesh (m)
        (make-hash
          :verts (-> m ai:vertices make-buffer)
          :normals (-> m ai:normals make-buffer)
          :uvs (-> m ai:texture-coords make-buffer)
          :indices (-> m ai:faces make-buffer)
          :bones (-> m ai:bones)
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
