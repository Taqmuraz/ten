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
              :name (-> b ai:name keyword-of)
              :matrix (-> b ai:offset-matrix load-matrix)
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
          :color (merge-into 'vector (make-assoc 3 1) (map-key mat "$clr.diffuse"))
          :textures (mapcar (mpart apply #'load-texture) (map-key mat "$tex.file"))
        )
      )
      (load-submesh (m)
        (make-assoc
          :verts (ai:vertices m)
          :normals (ai:normals m)
          :uvs (ai:texture-coords m)
          :faces (-> m ai:faces triangulate)
          :material (ai:material-index m)
          ;:bones (-> m ai:bones load-bones)
        )
      )
      (load-matrix (m) (-> m vec-16->mat-4x4 transponed))
      (load-tree (node)
        (make-assoc
          :name (-> node ai:name keyword-of)
          :meshes (ai:meshes node)
          :matrix (-> node ai:transform load-matrix)
          :children (loop for c across (ai:children node) collect (load-tree c))
        )
      )
      (load-anim-frame (pos rot scale)
        (lets (
            pos (apply #'mat-translation (coerce pos 'list))
            rot (-> rot rtg-math.quaternions:to-mat4 vec-16->mat-4x4)
            scale (apply #'mat-scale-4x4 (coerce scale 'list))
          )
          (mul-mats-4x4 pos rot scale)
        )
      )
      (load-bone-anim (ch)
        (lets (
            name (-> ch ai:node-name keyword-of)
            ps (last-> ch ai:position-keys (map 'list #'ai:value))
            rs (last-> ch ai:rotation-keys (map 'list #'ai:value))
            ss (last-> ch ai:scaling-keys (map 'list #'ai:value))
            fs (map 'vector #'load-anim-frame ps rs ss)
          )
          (cons name fs)
        )
      )
      (replace-tree (tree map)
        (with-map-keys (name matrix children) tree
          (lets (
              p (map-key map name)
              m (if p p matrix)
            )
            (with-vals tree
              :matrix m
              :children (loop for c in children collect
                (replace-tree c map))
            )
          )
        )
      )
      (load-anim (tree anim)
        (lets (
            chs (-> anim ai:channels)
            as (map 'list #'load-bone-anim chs)
            l (-> as first cdr length)
            trees (loop for i from 0 below l
              for fs = (assoc->hash (update-vals as (sfun a map-key a i)))
              collect (replace-tree tree fs)
            )
            frames (map 'vector #'load-pose trees)
          )
          (cons (-> anim ai:name keyword-of) frames)
        )
      )
      (load-pose (tree &optional (parent (mat-identity 4)) (pose (hash)))
        (with-map-keys (name matrix children) tree
          (lets (mat (mul-mat-4x4 parent matrix))
            (setf (gethash name pose) mat)
            (loop for c in children do (load-pose c mat pose))
          )
        )
        pose
      )
    )
    (lets (
        scene (-> file truename cffi-sys:native-namestring ai:import-into-lisp)
        meshes (map 'vector #'load-submesh (ai:meshes scene))
        materials (map 'vector #'load-material (ai:materials scene))
        tree (-> scene ai:root-node load-tree)
        pose (load-pose tree)
        anims (last-> scene ai:animations (map 'list (mpart load-anim tree)))
      )
      (make-assoc
        :meshes meshes
        :materials materials
        :tree tree
        :pose pose
        :anims anims
      )
    )
  )
)

(defun animate (anim time)
  (when anim (map-key anim (floor (mod (* 120 time) (length anim)))))
)

(defun load-model-to-gl (data shaders)
  (labels (
      (load-buffer (buf kind data)
        (gl:bind-buffer kind buf)
        (gl:buffer-data kind :static-draw data)
        (gl:free-gl-array data)
        (gl:bind-buffer kind 0)
      )
      (load-attrib-array (buf kind ind dim)
        (gl:bind-buffer kind buf)
        (gl:enable-vertex-attrib-array ind)
        (gl:vertex-attrib-pointer ind dim :float nil 0 (cffi:null-pointer))
      )
      (load-joint-weights (bones inds)
        (when bones
          (lets (
              bones (coerce bones 'vector)
              l (length inds)
              ws (make-array l :initial-element nil)
            )
            (loop for bi from 0 below (length bones) for b = (map-key bones bi) do
              (loop for (i w) in (-> b :weights) do
                (push (cons bi w) (aref ws i))
              )
            )
            ws
          )
        )
      )
      (compress-weights (w)
        (lets (
            w (sort w (sfun (x y) > (cdr x) (cdr y)))
            s (last-> w (mapcar #'cdr) (apply #'+))
            s (if (zerop s) 1.0 s)
            emp (make-array 4 :initial-element (cons -1 0))
            w (merge-into 'vector emp (take w 4 'vector))
          )
          (map 'vector (sfun x cons (car x) (/ (cdr x) s)) w)
        )
      )
      (fill-buffer (src dst dim)
        (loop for i from 0 below (length src)
          for e = (aref src i)
          do (loop for j from 0 below dim
            do (setf (gl:glaref dst (+ j (* i dim))) (aref e j))
          )
        )
      )
      (load-mesh-to-gl (m)
        (with-map-keys (verts uvs normals faces bones) m
          (lets (
              inds (apply #'concatenate 'vector (coerce faces 'list))
              l (length verts)
              vs (gl:alloc-gl-array :float (* 3 l))
              us (gl:alloc-gl-array :float (* 2 l))
              ns (gl:alloc-gl-array :float (* 3 l))
              els (gl:alloc-gl-array :unsigned-short (length inds))
              buffers (gl:gen-buffers 4)
              vert-buf (elt buffers 0)
              uv-buf (elt buffers 1)
              norm-buf (elt buffers 2)
              elt-buf (elt buffers 3)
              arr (gl:gen-vertex-array)
              uvs-0 (map-key uvs 0)
            )
            (fill-buffer faces els 3)
            (fill-buffer verts vs 3)
            (when uvs-0 (fill-buffer uvs-0 uv 2))
            (fill-buffer normals ns 3)
            (load-buffer vert-buf :array-buffer vs)
            (load-buffer uv-buf :array-buffer us)
            (load-buffer norm-buf :array-buffer ns)
            (load-buffer elt-buf :element-array-buffer els)
            (gl:bind-vertex-array arr)
            (when bones
              (lets (
                  ws (map 'vector #'compress-weights (load-joint-weights bones inds))
                  ws-arr (gl:alloc-gl-array :float (* 4 l))
                  js-arr (gl:alloc-gl-array :float (* 4 l))
                  bufs (gl:gen-buffers 2)
                  ws-buf (first bufs)
                  js-buf (second bufs)
                )
                (loop for i from 0 below (length ws) for row = (aref ws i) do
                  (loop for wi from 0 below 4
                    for addr = (+ wi (* 4 i))
                    for w = (aref row wi)
                    do
                    (setf (gl:glaref ws-arr addr) (cdr w))
                    (setf (gl:glaref js-arr addr) (coerce (car w) 'single-float))
                  )
                )
                (load-buffer ws-buf :array-buffer ws-arr)
                (load-buffer js-buf :array-buffer js-arr)
                (load-attrib-array js-buf :array-buffer 3 4)
                (load-attrib-array ws-buf :array-buffer 4 4)
              )
            )
            (load-attrib-array vert-buf :array-buffer 0 3)
            (load-attrib-array uv-buf :array-buffer 1 2)
            (load-attrib-array norm-buf :array-buffer 2 3)
            (gl:bind-buffer :element-array-buffer elt-buf)
            (gl:bind-vertex-array 0)
            (with-vals m
              :gl-array arr
              :gl-count (length inds)
              :shader (map-key shaders (if bones :skin :static))
            )
          )
        )
      )
      (load-blank-for-empty-material (m)
        (if (-> m :textures length zerop)
          (with-vals m :textures (list (load-blank-texture)))
          m
        )
      )
      (load-material-to-gl (m)
        (load-blank-for-empty-material
          (update m (mpart mapcar #'load-texture-to-gl) :textures))
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
          (-> matrix mat-4x4->vec-16 gl:mult-matrix)
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

(defun display-gl-model (gl-model &key (mat-stack nil)
                                       (proj-mat (mat-identity 4))
                                       (pose nil))
  (labels (
      (display-bone (bone pose)
        (applyv #'gl:vertex (transform-point-4x4
          (map-key pose (-> bone :name)) #(0 0 0)))
      )
      (display-bones (tree pose color)
        (with-map-keys (children) tree
          (gl:with-primitives :lines
            (applyv #'gl:color color)
            (loop for c in children do
              (display-bone tree pose)
              (display-bone c pose)
            )
          )
          (loop for c in children do (display-bones c pose color))
        )
      )
      (display-tree (meshes materials tree pose mat-stack)
        (with-map-keys ((tmeshes :meshes) matrix children) tree
          (with-stack-push mat-stack matrix
            (loop for i across tmeshes
              for mesh = (map-key meshes i)
              do (with-map-keys (gl-array gl-count (mat :material) bones) mesh
                (gl:bind-texture :texture-2d 0)
                (with-map-keys ((p :program)) (-> mesh :shader)
                  (gl:use-program p)
                  (with-map-keys (color) (map-key materials mat)
                    (load-uniform-vec p "color" color)
                  )
                  (when bones
                    (lets (
                        bs (mapcar (sfun b last-> b :matrix) bones)
                        ns (mapcar (sfun b -> b :name) bones)
                        ts (mapcar (sfun (n m) mul-mat-4x4 (map-key pose n) m) ns bs)
                      )
                      (load-uniform-mats p "jointTransforms" ts)
                    )
                  )
                  (load-uniform-mat p "transform" (car mat-stack))
                  (load-uniform-mat p "projection" proj-mat)
                )
                (loop for tex in (map-key (map-key materials mat) :textures)
                  do (with-map-keys (gl-id num) tex
                    (gl:active-texture (+ (cffi:foreign-enum-value '%gl:enum :texture0) num))
                    (gl:bind-texture :texture-2d gl-id)
                  )
                )
                (gl:bind-vertex-array gl-array)
                (gl:draw-elements :triangles (gl:make-null-gl-array :unsigned-short) :count gl-count)
                (gl:use-program 0)
              )
            )
            (loop for c in children do (display-tree meshes materials c pose mat-stack))
          )
        )
      )
    )
    (with-map-keys (meshes materials tree (tpose :pose)) gl-model
      ;(display-bones tree tpose #(0 1 0 1))
      ;(when pose (display-bones tree (merge-into 'hash-table tpose pose) #(1 0 0 1)))
      (display-tree meshes materials tree (merge-into 'hash-table tpose pose) mat-stack)
    )
  )
)
