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
        (list kind num (-> file file-parent (concat-path loc-file)))
      )
      (load-material (mat)
        (make-assoc
          :name (map-key mat "?mat.name")
          :color (merge-into 'vector (make-assoc 3 1) (map-key mat "$clr.diffuse"))
          :textures (mapcar (mpart apply #'load-texture) (map-key mat "$tex.file"))
        )
      )
      (load-submesh (m)
        (make-assoc
          :verts (map 'vector (sfun v select-keys v 0 2 1) (ai:vertices m))
          :normals (ai:normals m)
          :uvs (-> m ai:texture-coords (map-key 0))
          :faces (-> m ai:faces triangulate)
          :material (ai:material-index m)
          :bones (-> m ai:bones load-bones)
        )
      )
      (flip-matrix-yz (m)
        (lets (
            flip (classic-matrix
              (1 0 0 0)
              (0 0 1 0)
              (0 1 0 0)
              (0 0 0 1)
            )
          )
          (mul-mats-4x4 flip m flip)
        )
      )
      (load-matrix (m) (-> m mat-from-gl flip-matrix-yz))
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
          (flip-matrix-yz (mul-mats-4x4 pos rot scale))
        )
      )
      (load-bone-anim-key (k)
        (cons (ai:key-time k) (ai:value k))
      )
      (load-bone-anim-keys (keys)
        (lets (
            tree (assoc->tree (map 'list #'load-bone-anim-key keys) #'< #'=)
          )
          (sfun key -> (find-bounds tree key #'<) car cdr)
        )
      )
      (load-bone-anim (ch)
        (lets (
            name (-> ch ai:node-name keyword-of)
            ps (-> ch ai:position-keys load-bone-anim-keys)
            rs (-> ch ai:rotation-keys load-bone-anim-keys)
            ss (-> ch ai:scaling-keys load-bone-anim-keys)
            func (sfun key load-anim-frame
              (funcall ps key)
              (funcall rs key)
              (funcall ss key))
          )
          (cons name func)
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
            func (sfun key last->
              (update-vals as (sfun a funcall a key))
              (replace-tree tree)
              load-pose
            )
            len (ai:duration anim)
            bones (keys as)
          )
          (cons (-> anim ai:name keyword-of)
            (make-assoc
              :length len
              :map func
              :bones bones))
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
      (rotate-root (tree x y z)
        (update tree
          (sfun m mul-mat-4x4 m (mat-rotation x y z))
          :matrix
        )
      )
    )
    (lets (
        scene (-> file truename cffi-sys:native-namestring ai:import-into-lisp)
        meshes (map 'vector #'load-submesh (ai:meshes scene))
        materials (map 'vector #'load-material (ai:materials scene))
        tree (-> scene ai:root-node load-tree (rotate-root (* pi -1/2) 0 0))
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
  (when anim
    (map-key
      (-> anim :map)
      (mod time (-> anim :length)))))

(defun cache-anim (anim frames-count &key posefun)
  (when anim
    (labels (
        (cache-pose (pose bones)
          (typecase pose
            (function (apply #'func->hash pose bones))
            (hash-table pose)
            (t (error (format nil "Unsupported pose kind : ~A~%" pose)))
          )
        )
      )
      (lets (
          posef (if posefun posefun #'identity)
          len (-> anim :length)
          map (-> anim :map)
          bones (-> anim :bones)
          keys (loop for i from 0 below frames-count collect (* len (/ i frames-count)))
          poses (mapcar (sfun k cons k (cache-pose (map-key map k) bones)) keys)
          poses (update-vals poses posef)
          poses (assoc->tree poses #'< #'=)
          func (sfun k -> (find-bounds poses k #'<) car cdr)
        )
        (with-vals anim :map func)
      )
    )
  )
)

(defun pose-to-gl (pose) (update-vals pose #'mat-to-gl))

(defun anim-to-gl (anim) (cache-anim anim 120 :posefun #'pose-to-gl))

(defun load-model-to-gl (data)
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
      (load-joint-weights (bones count)
        (when bones
          (lets (
              bones (coerce bones 'vector)
              ws (make-array count :initial-element nil)
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
      (make-buffer (src els dim)
        (lets (
            l (length els)
            dst (gl:alloc-gl-array :float (* dim l))
          )
          (when src (fill-buffer (map 'vector (sfun i aref src i) els) dst dim))
          dst
        )
      )
      (load-mesh-to-gl (m)
        (with-map-keys (verts uvs normals faces bones material) m
          (lets (
              inds (apply #'concatenate 'vector (coerce faces 'list))
              vs (make-buffer verts inds 3)
              us (make-buffer uvs inds 2)
              ns (make-buffer normals inds 3)
              els (gl:alloc-gl-array :unsigned-short (length inds))
              buffers (gl:gen-buffers 4)
              vert-buf (elt buffers 0)
              uv-buf (elt buffers 1)
              norm-buf (elt buffers 2)
              elt-buf (elt buffers 3)
              arr (gl:gen-vertex-array)
            )
            (loop for i from 0 below (length inds) do
              (setf (gl:glaref els i) i)
            )
            (load-buffer vert-buf :array-buffer vs)
            (load-buffer uv-buf :array-buffer us)
            (load-buffer norm-buf :array-buffer ns)
            (load-buffer elt-buf :element-array-buffer els)
            (gl:bind-vertex-array arr)
            (when bones
              (lets (
                  wjs (map 'vector #'compress-weights
                    (load-joint-weights bones (length verts)))
                  js (map 'vector (mpart map 'vector
                    (sfun j -> j car (coerce 'single-float))) wjs)
                  ws (map 'vector (mpart map 'vector
                    (sfun w -> w cdr (coerce 'single-float))) wjs)
                  ws-arr (make-buffer ws inds 4)
                  js-arr (make-buffer js inds 4)
                  bufs (gl:gen-buffers 2)
                  ws-buf (first bufs)
                  js-buf (second bufs)
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
            (make-assoc
              :gl-array arr
              :gl-count (length inds)
              :gl-bones (update-vals bones (sfun b update b #'mat-to-gl :matrix))
              :bones bones
              :shader (if bones :skin :static)
              :material material
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
      (load-texture (kind num abs-file)
        (with-vals
          (with-texture-data data abs-file
            (load-texture-to-gl data))
          :source abs-file
          :bits nil
          :num num))
      (load-material-to-gl (m)
        (load-blank-for-empty-material
          (update m (mpart mapcar
            (mpart apply #'load-texture)) :textures)))
    )
    (with-map-keys (meshes materials anims pose tree) data (make-assoc
      :materials (map 'vector #'load-material-to-gl materials)
      :meshes (map 'vector #'load-mesh-to-gl meshes)
      :tree tree
      :gl-pose (pose-to-gl pose)
      :gl-anims (update-vals anims #'anim-to-gl)
    ))
  )
)

(defun collect-gl-instances (tree meshes materials)
  (with-map-keys ((mnums :meshes) children name) tree
    (lets (
        own (if (-> mnums length zerop not)
              (map 'list
                (sfun m lets (mesh (map-key meshes m))
                  (make-hash
                    :mesh m
                    :material (map-key materials (map-key mesh :material))
                    :shader (map-key mesh :shader)
                    :bones (mapcar (sfun b select-keys b :name :matrix) (map-key mesh :gl-bones))
                    :bone-names (mapcar (sfun b -> b :name) (map-key mesh :bones))
                    :node name)) mnums))
        chs (loop for c in children append (collect-gl-instances c meshes materials))
      )
      (append own chs)
    )
  )
)

(defun group-gl-instances (instances meshes)
  (lets (
      sg (group-by instances (sfun i -> i :shader))
    )
    (on-map (shader instances) sg
      (lets (
          bg (group-by instances (sfun i -> i :bone-names))
        )
        (make-assoc :shader shader :bone-groups
          (on-map (bone-names instances) bg
            (lets (
                bones (map-key (car instances) :bones)
              )
              (make-assoc :bones bones :material-groups
                (lets (
                    mg (group-by instances (sfun i -> i :material))
                  )
                  (on-map (material instances) mg
                    (lets (
                        color (-> material :color)
                        name (-> material :name)
                        texs (-> material :textures)
                        msg (group-by instances (sfun i -> i :mesh)))
                      (make-assoc :name name :color color :textures texs :mesh-groups
                        (on-map (mesh instances) msg
                          (make-assoc :mesh mesh :nodes
                            (mapcar (sfun i -> i :node) instances)))))))))))))))

(defun load-gl-group (gl-model)
  (with-map-keys (tree gl-pose gl-anims meshes materials) gl-model
    (make-assoc
      :meshes meshes
      :pose gl-pose
      :anims gl-anims
      :shader-groups
        (-> (collect-gl-instances tree meshes materials)
          (group-gl-instances meshes)))))

(defun display-gl-group (group shaders &key (proj (mat-identity 4))
                                            (root (mat-identity 4))
                                            (light #(0 -1 0))
                                            (pose nil))
  (display-gl-group-instanced group shaders
    (vector
      (make-assoc :pose pose :root root))
    :proj proj
    :light light
  )
)

(defun load-instancing-buffer (buffer data &optional)
  (lets (
      data (concat 'vector data)
      d (gl:alloc-gl-array :float (length data))
    )
    (forvec (i v) data (setf (gl:glaref d i) (coerce v 'single-float)))
    (destructuring-bind (base buf) buffer
      (gl:bind-buffer :shader-storage-buffer buf)
      (gl:buffer-data :shader-storage-buffer :dynamic-copy d)
      (%gl:bind-buffer-base :shader-storage-buffer base buf)
    )
  )
)

(defun alloc-instancing-buffers ()
  (destructuring-bind (trans bones poses color) (gl:create-buffers 4)
    (make-assoc
      :trans (list 5 trans)
      :bones (list 6 bones)
      :poses (list 7 poses)
      :color (list 8 color)
    )
  )
)

(defun display-gl-group-instanced (group shaders instances &key (proj (mat-identity 4))
                                                                (light #(0 -1 0)))
  (with-map-keys (meshes (tpose :pose) shader-groups) group
    (lets (
        buffers (alloc-instancing-buffers)
        instances (coerce instances 'vector)
        len (length instances)
        poses (map 'vector (sfun i last-> i :pose (merge-into 'hash-table tpose)) instances)
        roots (map-by-key 'vector :root instances)
      )
      (loop for sg in shader-groups do
        (with-map-keys (shader bone-groups) sg
          (lets (
              p (map-key (map-key shaders (keyword-of :instancing- shader)) :program)
            )
            (gl:use-program p)
            (gl:bind-texture :texture-2d 0)
            (load-uniform-mat p "projection" proj)
            (load-uniform-vec p "worldLight" light)
            (loop for bg in bone-groups do
              (with-map-keys (bones material-groups) bg
                (when bones
                  (lets (
                      ts (map 'vector (sfun pose map 'vector
                        (sfun b map-key pose (map-key b :name)) bones) poses)
                      ts (concat 'vector ts)
                      os (map 'vector (sfun b map-key b :matrix) bones)
                    )
                    (load-uniform-float p "bones" (length os))
                    (load-instancing-buffer (-> buffers :poses) ts)
                    (load-instancing-buffer (-> buffers :bones) os)
                  )
                )
                (loop for mg in material-groups do
                  (with-map-keys (textures color mesh-groups) mg
                    (load-instancing-buffer (-> buffers :color) (loop repeat len collect color))
                    (loop for tex in textures do
                      (gl:active-texture (+
                        (cffi:foreign-enum-value '%gl:enum :texture0)
                        (-> tex :num)))
                      (gl:bind-texture :texture-2d (-> tex :gl-id)))
                    (loop for msg in mesh-groups do
                      (with-map-keys (mesh nodes) msg
                        (loop for node in nodes do
                          (load-instancing-buffer (-> buffers :trans)
                            (if bones
                              (map 'vector #'mat-to-gl roots)
                              (map 'vector
                                (sfun (root pose) mat-to-gl (last-> node (map-key pose) mat-from-gl
                                  (mul-mat-4x4 root))) roots poses))
                          )
                          (with-map-keys (gl-array gl-count) (map-key meshes mesh)
                            (gl:bind-vertex-array gl-array)
                            (gl:draw-arrays-instanced :triangles 0 gl-count len)
                          )
                        )
                      )
                    )
                  )
                )
              )
            )
          )
        )
      )
    )
  )
  (gl:bind-texture :texture-2d 0)
  (gl:use-program 0)
)
