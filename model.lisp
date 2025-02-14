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
              :matrix (ai:offset-matrix b)
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
      (load-tree (node)
        (make-assoc
          :meshes (ai:meshes node)
          :matrix (ai:transform node)
          :children (loop for c across (ai:children node) collect (load-tree c))
        )
      )
    )
    (lets (
        scene (-> file truename cffi-sys:native-namestring ai:import-into-lisp)
        meshes (map 'vector #'load-submesh (ai:meshes scene))
        tree (-> scene ai:root-node load-tree)
      )
      (make-assoc
        :meshes meshes
        :tree tree
      )
    )
  )
)

(defun display-model (model)
  (labels (
    (display-tree (meshes tree)
      (gl:with-pushed-matrix
        (gl:mult-transpose-matrix (map-key tree :matrix))
        (gl:with-primitives :triangles
          (gl:color 1/2 1 1/2 1)
          (loop for id across (map-key tree :meshes)
            for mesh = (map-key meshes id)
            for vs = (map-key mesh :verts)
            do (loop for v across vs do (apply #'gl:vertex (coerce v 'list)))
          )
        )
        (loop for c in (map-key tree :children) do (display-tree meshes c))
      )
    )
  ) (display-tree (map-key model :meshes) (map-key model :tree)))
)
