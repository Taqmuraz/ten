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
      (load-texture (kind num loc-file)
        (lets (
            abs-file (-> file file-parent (concat-path loc-file))
          )
          (with-vals
            (alexandria:ensure-gethash abs-file textures-cache
              (load-texture-data abs-file)
            )
            :num num
          )
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
          :faces (ai:faces m)
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

(gl:define-gl-array-format gl-model-vertex
  (gl:vertex :type :float :components (x y z))
  (gl:tex-coord :type :float :components (tx ty))
  (gl:normal :type :float :components (nx ny nz))
)

(defun load-model-to-gl (data &key (shaders-cache (hash)))
  (labels (
      (load-mesh-to-gl (m)
        (with-map-keys (verts uvs normals faces) m
          (lets (
              inds (apply #'concatenate 'vector (coerce faces 'list))
              arr (gl:alloc-gl-array 'gl-model-vertex (length inds))
              els (gl:alloc-gl-array :unsigned-short (length inds))
            )
            (loop for i across inds do
              (progn
                (setf (gl:glaref els i) i)
                (with-map-keys ((x 0) (y 1) (z 2)) (map-key verts i)
                  (setf (gl:glaref arr i 'x) x)
                  (setf (gl:glaref arr i 'y) y)
                  (setf (gl:glaref arr i 'z) z)
                )
                (when (/= (length uvs) 0)
                  (with-map-keys ((x 0) (y 1)) (map-key (map-key uvs 0) i)
                    (when (and x y)
                      (setf (gl:glaref arr i 'tx) x)
                      (setf (gl:glaref arr i 'ty) y)
                    )
                  )
                )
                (with-map-keys ((x 0) (y 1) (z 2)) (map-key normals i)
                  (setf (gl:glaref arr i 'nx) x)
                  (setf (gl:glaref arr i 'ny) y)
                  (setf (gl:glaref arr i 'nz) z)
                )
              )
            )
            (with-vals m
              :gl-array arr
              :gl-elements els
              :shader (alexandria:ensure-gethash :diffuse shaders-cache (load-shader-to-gl
                (uiop:read-file-string "res/shaders/vertex.glsl")
                (uiop:read-file-string "res/shaders/fragment.glsl")
              ))
            )
          )
        )
      )
      (load-material-to-gl (m)
        (update m (mpart mapcar #'load-texture-to-gl) :textures)
      )
    )
    (with-map-keys (meshes materials) data (with-vals data
      :materials (map 'vector #'load-material-to-gl materials)
      :meshes (map 'vector #'load-mesh-to-gl meshes)
    ))
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
            for faces = (map-key mesh :faces)
            for uvs = (map-key mesh :uvs)
            for ns = (map-key mesh :normals)
            for mat-index = (map-key mesh :material)
            for mat = (map-key materials mat-index)
            for color = (map-key mat :color #(1 1 1 1))
            for textures = (map-key mat :textures)
            do (progn
              (gl:bind-texture :texture-2d 0)
              (loop for tex in textures
                do (with-map-keys (gl-id num) tex
                  (gl:active-texture (+ (cffi:foreign-enum-value '%gl:enum :texture0) num))
                  (gl:bind-texture :texture-2d gl-id)
                )
              )
              (gl:with-primitives :triangles
                (apply #'gl:color (coerce color 'list))
                (loop for f across faces do
                  (loop for i across f
                    for v = (coerce (map-key vs i) 'list)
                    for uv = (coerce (if uvs (map-key (map-key uvs 0) i)) 'list)
                    for n = (coerce (map-key ns i) 'list)
                    do (progn
                      (when uv (apply #'gl:tex-coord uv))
                      (apply #'gl:normal n)
                      (apply #'gl:vertex v)
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

(defun display-gl-model (gl-model)
  (labels (
      (display-tree (meshes materials tree)
        (with-map-keys ((tmeshes :meshes) matrix children) tree
          (gl:with-pushed-matrix
            (gl:mult-transpose-matrix matrix)
            (loop for i across tmeshes
              for mesh = (map-key meshes i)
              do (with-map-keys (gl-array gl-elements (mat :material)) mesh
                (gl:bind-texture :texture-2d 0)
                (-> mesh :shader :program gl:use-program)
                (loop for tex in (map-key (map-key materials mat) :textures)
                  do (with-map-keys (gl-id num) tex
                    (gl:active-texture (+ (cffi:foreign-enum-value '%gl:enum :texture0) num))
                    (gl:bind-texture :texture-2d gl-id)
                  )
                )
                (gl:enable-client-state :vertex-array)
                (gl:enable-client-state :texture-coord-array)
                (gl:enable-client-state :normal-array)
                (gl:bind-gl-vertex-array gl-array)
                (gl:draw-elements :triangles gl-elements)
              )
            )
            (loop for c in children do (display-tree meshes materials c))
          )
        )
      )
    )
    (with-map-keys (meshes materials tree) gl-model
      (display-tree meshes materials tree)
    )
  )
)
