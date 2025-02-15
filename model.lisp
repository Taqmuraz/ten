(in-package #:ten)

(defun load-model-data (file &key (textures-cache (hash)))
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
      (load-texture (kind num file)
        (with-vals
          (alexandria:ensure-gethash file textures-cache
            (-> file load-texture-data load-texture-to-gl)
          )
          :num num
        )
      )
      (load-material (mat)
        (make-assoc
          :color (map-key mat "$clr.diffuse")
          :textures (mapcar (mpart apply #'load-texture) (map-key mat "$tex.file"))
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
          (loop for id across tmeshes
            for mesh = (map-key meshes id)
            for vs = (map-key mesh :verts)
            for faces = (map-key mesh :indices)
            for uvs = (map-key mesh :uvs)
            for mat-index = (map-key mesh :material)
            for mat = (map-key materials mat-index)
            for color = (map-key mat :color #(1 1 1 1))
            for textures = (map-key mat :textures)
            do (progn
              (loop for tex in textures
                do (with-map-keys (gl-id num) tex
                  ;(gl:active-texture (+ (cffi:foreign-enum-value '%gl:enum :texture0) num))
                  (gl:bind-texture :texture-2d gl-id)
                )
              )
              (gl:with-primitives :triangles
                (apply #'gl:color (coerce color 'list))
                (loop for f across faces do
                  (loop for i across f
                    for v = (map-key vs i)
                    for uv = (coerce (if uvs (map-key (map-key uvs 0) i)) 'list)
                    do (progn
                      (when uv (apply #'gl:tex-coord uv))
                      (apply #'gl:vertex (coerce v 'list))
                    )
                  )
                )
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
