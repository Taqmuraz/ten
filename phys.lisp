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

(defun sphere-vs-triangle (rad center a b c)
  (labels (
      (col (v) (make-array (length v)
        :element-type 'single-float
        :initial-contents (loop for e across v collect (coerce e 'single-float))
      ))
      (vcol (v) (col (concatenate 'vector v '(0))))
    )
    (lets (
        tnorm (norm (cross (v- a b) (v- c b)))
        dst (-> center (v- a) (dot tnorm) abs (- rad))
      )
      (when (<= dst 0)
        (lets (
            tmat (rtg-math.matrix4:from-columns
              (vcol (v- a center))
              (vcol (v- b center))
              (vcol (v- c center))
              (col #(0 0 0 1))
            )
            inv (-> tmat rtg-math.matrix4:inverse vec-16->mat-4x4)
            p (transform-vector inv (v- tnorm))
          )
          (when (not (at-least-one p (sfun e < e 0)))
            (make-assoc
              :dist dst
              :point (v+ center (v* tnorm (vvv (- 0 dst rad))))
              :normal tnorm
            )
          )
        )
      )
    )
  )
)

(defun sphere-vs-mesh (sphere mesh)
  (lets (r nil)
    (with-maps-keys (((shapes-tree) mesh)
                     ((shapes tree) shapes-tree)
                     (((sbounds :bounds)) sphere))
      (labels (
          (walk-tree (node)
            (with-map-keys ((ids :shapes) bounds children) node
              (when (bounds-intersectp sbounds bounds)
                (loop for id in ids do
                  (push-when or (shapes-contact sphere (map-key shapes id)) r)
                )
                (mapc #'walk-tree children)
              )
            )
          )
        )
        (walk-tree tree)
      )
    )
    r
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
  (-> (mapcar #'clamp-with-bounds (ll b) a) bounds-volume zerop not)
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
  )
)

(defun triangle-shape (points)
  (make-assoc
    :kind :triangle
    :points points
    :bounds (apply #'triangle-bounds points)
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
        :shapes-tree (shapes-tree (map 'list #'triangle-shape tris))
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
      (walk-tree (shapes &key (bounds nil) (cap 10) (depth 0) (max-depth 5))
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
                  for ss = (remove-if-not (sfun s -> s :bounds (bounds-intersectp cut)) shapes)
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

(defun shapes-contact (a b)
  (labels (
      (svm (a b)
        (sphere-vs-mesh a b)
      )
      (svt (a b)
        (with-maps-keys (((radius center) a)
                         ((points) b))
          (apply #'sphere-vs-triangle radius center points)
        )
      )
      (svs (a b)
        (with-maps-keys ((((ar :radius) (ac :center)) a)
                         (((br :radius) (bc :center)) b))
          (sphere-vs-sphere ar ac br bc)
        )
      )
    )
    (with-maps-keys ((((akind :kind)) a)
                     (((bkind :kind)) b))
      (cases-equal (list akind bkind)
        '(:sphere :sphere) (svs a b)
        '(:sphere :triangle) (svt a b)
        '(:triangle :sphere) (svt b a)
        '(:sphere :mesh) (svm a b)
        '(:mesh :sphere) (svm b a)
      )
    )
  )
)

(defun shapes-tree-contacts (shapes-tree)
  (lets (r nil)
    (with-map-keys (shapes tree) shapes-tree
      (labels (
          (walk-tree (node)
            (with-map-keys ((ids :shapes) children) node
              (loop for (id1 id2) in (all-possible-pairs ids) do
                (with-map-keys ((s1 id1) (s2 id2)) shapes
                  (push-when or (shapes-contact s1 s2) r)
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
