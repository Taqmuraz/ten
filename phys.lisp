(in-package #:ten)

(defun sphere-vs-sphere (a-rad a-center b-rad b-center)
  (lets (
      d (v- a-center b-center)
      l (len d)
      dist (- l (+ a-rad b-rad))
    )
    (when (<= dist 0)
      (lets (
          dir (norm d)
          an dir
          bn (v- dir)
          ap (-> bn (v* (vvv a-rad)) (v+ a-center))
          bp (-> an (v* (vvv b-rad)) (v+ b-center))
        )
        (make-assoc
          :dist dist
          :point-a ap
          :point-b bp
          :normal-a an
          :normal-b bn
        )
      )
    )
  )
)

(defun sphere-vs-triangle (rad center points normal)
  (macrolet (
      (turn (n a b) `(cross ,n (bvec vector - ,a ,b)))
      (v- (a b) `(bvec vector - ,a ,b))
      (v* (a b) `(bvec vector * ,a ,b))
      (v+ (a b) `(bvec vector + ,a ,b))
    )
    (with-items (a b c) points
      (lets (
          rel (v- center b)
          rd (dot normal rel)
          dst (- (abs rd) rad)
        )
        (when (and
            (<= 0 rd)
            (<= dst 0)
            (<= 0 (dot (turn normal a b) (v- center b)))
            (<= 0 (dot (turn normal b c) (v- center c)))
            (<= 0 (dot (turn normal c a) (v- center a)))
          )
          (make-assoc
            :dist dst
            :point (v+ center (v* normal (vvv (- 0 dst rad))))
            :normal normal
          )
        )
      )
    )
  )
)

(defun sphere-vs-mesh (rad center triangles)
  (last-> triangles
    (map 'list (sfun e apply #'sphere-vs-triangle rad center e))
    (remove-if #'null)
  )
)

(defun cover-with-bounds (bounds p)
  (destructuring-bind (min max) bounds
    (list (vmin min p) (vmax max p))
  )
)

(defun clamp-with-bounds (bounds p)
  (with-items (min max) bounds (vmax min (vmin max p)))
)

(defun triangle-bounds (a b c)
  (list (vmin a b c) (vmax a b c))
)

(defun combine-bounds (a b &rest others)
  (apply #'mapcar #'funcall (list #'vmin #'vmax) a b others)
)

(defun bounds* (a b)
  (mapcar #'v* a b)
)

(defun bounds-from-min-size (min size)
  (list min (v+ min size))
)

(defun bounds-volume (b)
  (destructuring-bind (min max) b
    (apply #'* (l- max min))
  )
)

(defun bounds-intersectp (a b)
  (macrolet (
      (bv (op x y) `(bvec vector ,op ,x ,y))
      (iv (op v) `(invec vector ,op ,v))
    )
    (with-items (amin amax) a
      (with-items (bmin bmax) b
        (lets (
            min (bv max bmin (bv min amin bmax))
            max (bv max bmin (bv min amax bmax))
          )
          (-> (iv * (bv - max min)) zerop not)
        )
      )
    )
  )
)

(defun sphere-bounds (rad center)
  (list (v- center (vvv rad)) (v+ center (vvv rad)))
)

(defun sphere-shape (rad center)
  (make-assoc
    :kind :sphere
    :radius rad
    :center center
    :bounds (sphere-bounds rad center)
    :velocity (vvv 0)
  )
)

(defun update-sphere-center (sphere func)
  (with-map-keys (radius center) shape
    (lets (
        center (funcall func center)
      )
      (with-vals shape
        :center center
        :bounds (sphere-bounds radius center)
      )
    )
  )
)

(defun recalc-sphere-bounds (sphere)
  (with-map-keys (radius center) sphere
    (with-vals sphere :bounds (sphere-bounds radius center))
  )
)

(defun recalc-shape-bounds (shape)
  (cases (-> shape :kind)
    :sphere (recalc-sphere-bounds shape)
    t shape
  )
)

(defun mesh-shape (mesh transform)
  (with-map-keys (faces verts) mesh
    (lets (
        tris (map 'vector (sfun f map 'list (sfun e transform-point transform (aref verts e)) f) faces)
        bounds (applyv #'triangle-bounds (map-key tris 0 (-> 0 vvv vvv)))
      )
      (loop for (a b c) across tris do
        (setf bounds (combine-bounds bounds (triangle-bounds a b c)))
      )
      (make-assoc
        :kind :mesh
        :bounds bounds
        :triangles (map 'vector
          (sfun e with-items (a b c) e
            (list e (norm (cross (v- a b) (v- c b))))
          )
          tris
        )
      )
    )
  )
)

(defun model-mesh-shapes (model)
  (lets (r nil)
    (with-map-keys (tree pose meshes) model
      (labels (
          (walk (node)
            (with-map-keys (name (nums :meshes) children) node
              (loop for n across nums do
                (push (mesh-shape (map-key meshes n) (map-key pose name)) r)
              )
              (mapc #'walk children)
            )
          )
        )
        (walk tree)
        r
      )
    )
  )
)

(defun shapes-tree (shapes)
  (labels (
      (give-shapes-id (shapes)
        (mapcar (sfun (s i) with-vals s :id i) shapes (-> shapes length list-range))
      )
      (walk-tree (shapes &key (bounds nil) (cap 10) (depth 0) (max-depth 2))
        (lets (
            bounds (conds
              bounds bounds
              (cdr shapes) (apply #'combine-bounds (map-by-key 'list :bounds shapes))
              t `((0 0 0)(0 0 0))
            )
          )
          (if (or (>= depth max-depth) (-> shapes length (<= cap)))
            (make-assoc
              :shapes (map-by-key 'list :id shapes)
              :bounds bounds
              :children nil
            )
            (make-assoc
              :shapes nil
              :bounds bounds
              :children
              (lets (
                  min (car bounds)
                  max (cadr bounds)
                  diff (v- max min)
                  cuts (loop for offset in
                    '(
                      (0 0 0)
                      (1 0 0)
                      (1 1 0)
                      (0 1 0)
                      (0 0 1)
                      (1 0 1)
                      (1 1 1)
                      (0 1 1)
                    )
                    collect (bounds-from-min-size
                      (v+ min (v* diff offset (vvv 1/2)))
                      (v* diff (vvv 1/2))
                    )
                  )
                )
                (loop for cut in cuts
                  for ss = (remove-if-not (sfun s bounds-intersectp (-> s :bounds) cut) shapes)
                  collect (walk-tree ss :bounds cut :cap cap :depth (+ 1 depth))
                )
              )
            )
          )
        )
      )
    )
    (make-assoc
      :shapes (into-vector shapes)
      :tree (-> shapes give-shapes-id walk-tree)
    )
  )
)

(defun shapes-contacts (a b)
  (labels (
      (svm (a b &optional reverse)
        (with-maps-keys (((radius center (abounds :bounds)) a)
                         ((triangles (bbounds :bounds)) b))
          (lets (
              contacts (when (bounds-intersectp abounds bbounds)
                (sphere-vs-mesh radius center triangles)
              )
            )
            (loop for contact in contacts collect
              (with-map-keys (normal point dist) contact
                (if reverse
                  (make-assoc
                    :point-a point
                    :point-b point
                    :normal-a (v- normal)
                    :normal-b normal
                    :dist dist
                  )
                  (make-assoc
                    :point-a point
                    :point-b point
                    :normal-a normal
                    :normal-b (v- normal)
                    :dist dist
                  )
                )
              )
            )
          )
        )
      )
      (svs (a b &optional reverse)
        (with-maps-keys ((((ar :radius) (ac :center)) a)
                         (((br :radius) (bc :center)) b))
          (lets (c (sphere-vs-sphere ar ac br bc))
            (when c (list c))
          )
        )
      )
    )
    (with-maps-keys ((((akind :kind)) a)
                     (((bkind :kind)) b))
      (cases-equal (list akind bkind)
        '(:sphere :sphere) (svs a b)
        '(:sphere :mesh) (svm a b)
        '(:mesh :sphere) (svm b a t)
      )
    )
  )
)

(defun shapes-tree-contacts (shapes-tree)
  (lets (r nil c (hash))
    (with-map-keys (shapes tree) shapes-tree
      (labels (
          (walk-tree (node)
            (with-map-keys ((ids :shapes) children) node
              (loop for (id1 id2) in (all-possible-pairs ids)
                for p = (list id1 id2)
                do (hash-once p c
                  (with-map-keys ((s1 id1) (s2 id2)) shapes
                    (lets (
                        cs (shapes-contacts s1 s2)
                      )
                      (when cs (setf r
                        (append r (mapcar (sfun c with-vals c :id-a id1 :id-b id2) cs)))
                      )
                    )
                  )
                )
              )
              (mapc #'walk-tree children)
            )
          )
        )
        (walk-tree tree)
      )
    )
    r
  )
)

(defun shapes-tree-sim (shapes-tree delta-time)
  (with-map-keys (shapes tree) shapes-tree
    (labels (
        (process-sphere-contact (shape point normal dist)
          (lets (
              n (v* normal (vvv* -1 dist))
              center (-> shape :center (v+ n))
              vel (-> shape :velocity)
              vd (dot vel normal)
              vel (if (< vd 0) (v- vel (v* normal (vvv vd))) vel)
              bounds (sphere-bounds (-> shape :radius) center)
            )
            (with-vals shape
              :center center
              :bounds bounds
              :velocity vel
            )
          )
        )
        (process-contact (contact)
          (with-maps-keys (
              ((id-a id-b point-a point-b normal-a normal-b dist) contact)
              (((a id-a) (b id-b)) shapes)
              (((kind-a :kind)) a)
              (((kind-b :kind)) b)
            )
            (cases kind-a :sphere
              (setf (aref shapes id-a) (process-sphere-contact a point-a normal-a dist))
            )
            (cases kind-b :sphere
              (setf (aref shapes id-b) (process-sphere-contact b point-b normal-b dist))
            )
          )
        )
      )
      (lets (
          contacts (shapes-tree-contacts shapes-tree)
        )
        (mapc #'process-contact contacts)
        (coerce shapes 'list)
      )
    )
  )
)

(defun process-forces (shapes delta-time &key (gravity #(0 -9.8 0)))
  (loop for shape in shapes
    with dg = (v* gravity (vvv delta-time))
    collect
    (cases (-> shape :kind)
      :sphere (with-map-keys (velocity center) shape
        (recalc-shape-bounds (with-vals shape
          :velocity (v+ velocity dg)
          :center (v+ center (v* velocity (vvv delta-time)))
        ))
      )
      t shape
    )
  )
)

(lets (
    fixed-time 0
  )
  (defun shapes-fixed-cycle (shapes delta-time &key (fixed-delta-time 1/50) (max-cycles 1))
    (incf fixed-time delta-time)
    (if (< fixed-delta-time fixed-time)
      (multiple-value-bind (n l) (floor fixed-time fixed-delta-time)
        (setf fixed-time l)
        (loop with s = shapes repeat (min n max-cycles) do
          (setf s (-> shapes
            (process-forces fixed-delta-time)
            shapes-tree
            (shapes-tree-sim fixed-delta-time))
          )
          finally (return s)
        )
      )
      shapes
    )
  )
)
