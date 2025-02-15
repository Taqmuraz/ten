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
      (load-material (mat)
        (make-assoc
          :color (map-key mat "$clr.diffuse")
          :textures (mapcar #'rest (map-key mat "$tex.file"))
        )
      )
      (load-submesh (m)
        (make-assoc
          :verts (ai:vertices m)
          :normals (ai:normals m)
          :uvs (ai:texture-coords m)
          :indices (ai:faces m)
          :material (ai:material-index m)
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
        materials (map 'vector #'load-material (ai:materials scene))
        tree (-> scene ai:root-node load-tree)
      )
      (make-assoc
        :meshes meshes
        :materials materials
        :tree tree
      )
    )
  )
)

(defun display-model (model)
  (labels (
    (display-tree (meshes materials tree)
      (with-map-keys ((tmeshes :meshes) matrix) tree
        (gl:with-pushed-matrix
          (gl:mult-transpose-matrix matrix)
          (gl:with-primitives :triangles
            (loop for id across tmeshes
              for mesh = (map-key meshes id)
              for vs = (map-key mesh :verts)
              for mat-index = (map-key mesh :material)
              for mat = (map-key materials mat-index)
              for color = (map-key mat :color #(1 1 1 1))
              do (progn
                (apply #'gl:color (coerce color 'list))
                (loop for v across vs do (apply #'gl:vertex (coerce v 'list)))
              )
            )
          )
          (loop for c in (map-key tree :children) do (display-tree meshes materials c))
        )
      )
    )
  ) (with-map-keys (meshes materials tree) model
      (display-tree meshes materials tree)
    )
  )
)
