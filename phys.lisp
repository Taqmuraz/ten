(in-package #:ten)

(defparameter *eps* 1/100000)
(defparameter *-eps* (- *eps*))

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
          :point-a bp
          :point-b ap
          :normal-a an
          :normal-b bn
        )
      )
    )
  )
)

(defun triangle-closest-point (p a b c normal)
  (labels (
      (inverse (mat)
        (lets (
            x (car mat)
            y (cadr mat)
            a (aref x 0)
            b (aref y 0)
            c (aref x 1)
            d (aref y 1)
            det (/ (- (* a d) (* b c)))
            a (* a det)
            b (* b det)
            c (* c det)
            d (* d det)
          )
          (list
            (vector d (- c))
            (vector (- b) a)
          )
        )
      )
      (mat*vec (mat vec)
        (mv2+
          (mv2* (vv (aref vec 0)) (car mat))
          (mv2* (vv (aref vec 1)) (cadr mat))
        )
      )
      (clamp (x min max) (min max (max min x)))
      (on-plane (p o x y)
        (lets (d (mv3- p o))
          (vector (dot d x) (dot d y))
        )
      )
      (subcut (p a b c)
        (lets (
            mat (list (mv2- a b) (mv2- c b))
            inv (inverse mat)
            ip (mat*vec inv (mv2- p b))
            ix (clamp (aref ip 0) 0 1)
            iy (clamp (aref ip 1) 0 1)
            r (mat*vec mat (vector ix iy))
          )
          (mv2+ b r)
        )
      )
      (cut (p a b c)
        (lets (
            x (norm (mv3- a b))
            y (cross x normal)
            pp (on-plane p b x y)
            ap (on-plane a b x y)
            bp #(0 0)
            cp (on-plane c b x y)
            op (-> pp (subcut ap bp cp) (subcut bp cp ap))
          )
          (v+
            b
            (mv3* x (vvv (aref op 0)))
            (mv3* y (vvv (aref op 1)))
          )
        )
      )
    )
    (cut p a b c)
  )
)

(defun sphere-vs-triangle (rad center points normal)
  (with-items (a b c) points
    (lets (
        rel (v- center b)
        rd (dot normal rel)
      )
      (when (<= 0 rd)
        (lets (
            p (triangle-closest-point center a b c normal)
            n (v- center p)
            dst (- (len n) rad)
          )
          (when (<= dst 0)
            (make-assoc
              :point p
              :normal (norm n)
            )
          )
        )
      )
    )
  )
)

(defun char-vs-triangle (rad height center points normal)
  (with-items (a b c) points
    (when (-> center (v- b) (dot normal) (>= 0))
      (lets (
          c (triangle-closest-point center a b c normal)
          d (v- c center)
          dh (abs (aref d 1))
          hh (/ height 2)
        )
        (when (<= dh hh)
          (lets (
              dn (norm d)
              l (sqrt (+ (* rad rad) (* hh hh)))
              dny (* l (aref dn 1))
              n (conds
                (>= dny hh) (vector 0 -1 0)
                (<= dny (- hh)) (vector 0 1 0)
                t (with-vector-items (x y z) dn (norm (vector (- x) 0 (- z))))
              )
            )
            (make-assoc
              :point c
              :normal n
            )
          )
        )
      )
    )
  )
)

(defmacro anything-vs-mesh (bounds triangles-tree (points-var normal-var) &body test)
  (once (bounds triangles-tree)
    `(lets (r nil h (hash))
      (labels (
          (walk-tree (triangles node)
            (with-map-keys ((ids :triangles) (tbounds :bounds) children) node
              (when (bounds-intersectp ,bounds tbounds)
                (loop for id in ids do
                  (hash-once id h
                    (lets (
                        pn (aref triangles id)
                        c (lets (
                            ,points-var (car pn)
                            ,normal-var (cadr pn)
                          )
                          (,@test)
                        )
                      )
                      (when c (push c r))
                    )
                  )
                )
                (loop for c in children do (walk-tree triangles c))
              )
            )
          )
        )
        (with-map-keys (triangles tree) ,triangles-tree (walk-tree triangles tree))
        r
      )
    )
  )
)

(defun sphere-vs-mesh (rad center bounds triangles-tree)
  (anything-vs-mesh bounds triangles-tree (points normal) sphere-vs-triangle rad center points normal)
)

(defun char-vs-mesh (rad height center bounds triangles-tree)
  (anything-vs-mesh bounds triangles-tree (points normal) char-vs-triangle rad height center points normal)
)

(defun shape-vs-mesh (shape mesh)
  (lets (
      sbounds (shape-bounds shape)
    )
    (with-map-keys (triangles-tree bounds) mesh
      (when (bounds-intersectp bounds sbounds)
        (cases (-> shape :kind)
          :sphere (with-map-keys (radius center) shape
            (sphere-vs-mesh radius center sbounds triangles-tree)
          )
          :char (with-map-keys (radius center height) shape
            (char-vs-mesh radius height center sbounds triangles-tree)
          )
        )
      )
    )
  )
)

(defun char-shape (radius height center)
  (make-assoc
    :kind :char
    :radius radius
    :height 2
    :center center
    :velocity (vvv 0)
  )
)

(defun char-bounds (radius height center)
  (lets (h (vector radius (/ height 2) radius))
    (list
      (v- center h)
      (v+ center h)
    )
  )
)

(defun char-vs-char (a b)
  (with-maps-keys (
      (((a-rad :radius) (a-height :height) (a-center :center)) a)
      (((b-rad :radius) (b-height :height) (b-center :center)) b)
    )
    (lets (
        dh (abs (- (aref a-center 1) (aref b-center 1)))
      )
      (when (<= dh (/ (+ a-height b-height) 2))
        (lets (
            d (xyz->x0z (v- a-center b-center))
            dst (- (len d) (+ a-rad b-rad))
          )
          (when (<= dst 0)
            (lets (
                n (norm d)
                an (v- n)
                bn n
                ap (v+ a-center (v* an (vvv a-rad)))
                bp (v+ b-center (v* bn (vvv b-rad)))
              )
              (make-assoc
                :point-a bp
                :point-b ap
                :normal-a bn
                :normal-b an
              )
            )
          )
        )
      )
    )
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
  (list
    (bvec vector + (vvv *-eps*) (bvec vector min (bvec vector min a b) c))
    (bvec vector + (vvv *eps*)  (bvec vector max (bvec vector max a b) c))
  )
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
  (list (mv3- center (vvv rad)) (mv3+ center (vvv rad)))
)

(defun sphere-shape (rad center)
  (make-assoc
    :kind :sphere
    :radius rad
    :center center
    :velocity (vvv 0)
  )
)

(defun shape-bounds (shape)
  (cases (-> shape :kind)
    :sphere (with-map-keys (radius center) shape (sphere-bounds radius center))
    :mesh (-> shape :bounds)
    :char (with-map-keys (radius height center) shape (char-bounds radius height center))
    t (error (format nil "Cannot calculate bounds for shape ~A" shape))
  )
)

(defun closest-point-in-shape (shape point)
  (cases (-> shape :kind)
    :sphere (with-map-keys (radius center) shape
      (lets (
          d (v- point center)
        )
        (if (<= radius (len d))
          (v+ center (v* (vvv radius) (norm d)))
          point
        )
      )
    )
    :char (with-map-keys (radius height center) shape
      (lets (
          hh (/ height 2)
          d (v- point center)
          dxz (xyz->x0z d)
          dy (aref d 1)
        )
        (if (and (<= (len dxz) radius) (<= (- hh) dy hh))
          point
          (v+
            center
            (clamp-length dxz radius)
            (vector 0 (max (- hh) (min hh dy)) 0)
          )
        )
      )
    )
    t (error (format "Can't find closest point on shape ~A" shape))
  )
)

(defun closest-point-on-shape (shape point)
  (cases (-> shape :kind)
    :sphere (with-map-keys (radius center) shape
      (v+ center (v* (vvv radius) (norm (v- point center))))
    )
    :char (with-map-keys (radius height center) shape
      (lets (
          hh (/ height 2)
          d (v- point center)
          dxz (xyz->x0z d)
          dy (aref d 1)
        )
        (conds
          (zerop (len dxz)) (v+ center (vector 0 (closest dy (- hh) hh) 0))
          (and (<= dy hh) (<= (- hh) dy)) (v+
            center
            (v* (norm dxz) (vvv radius))
            (vector 0 dy 0)
          )
          t (v+
            center
            (clamp-length dxz radius)
            (vector 0 (closest dy (- hh) hh) 0)
          )
        )
      )
    )
    t (error (format "Can't find closest point on shape ~A" shape))
  )
)

(defun mesh-shape (mesh transform)
  (with-map-keys (faces verts) mesh
    (lets (
        tris (map 'list (sfun f map 'list (sfun e transform-point transform (aref verts e)) f) faces)
        bounds (if tris (applyv #'triangle-bounds (car tris)) (-> 0 vvv vvv))
        tris (mapcar
          (sfun e with-items (a b c) e
            (list e (norm (cross (v- a b) (v- c b))) (triangle-bounds a b c))
          )
          tris
        )
      )
      (loop for (p n b) in tris do
        (setf bounds (combine-bounds bounds b))
      )
      (make-assoc
        :kind :mesh
        :bounds bounds
        :triangles tris
        :triangles-tree (triangles-tree tris)
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

(defun generic-tree (items &key items-key item-bounds item-with-id item-id (cap 10) (max-depth 2))
  (labels (
      (give-items-id (items)
        (mapcar item-with-id items (-> items length list-range))
      )
      (walk-tree (items &key (bounds nil) (depth 0))
        (lets (
            bounds (conds
              bounds bounds
              (cdr items) (apply #'combine-bounds (mapcar item-bounds items))
              t `((0 0 0)(0 0 0))
            )
          )
          (if (or (>= depth max-depth) (-> items length (<= cap)))
            (make-assoc
              items-key (mapcar item-id items)
              :bounds bounds
              :children nil
            )
            (make-assoc
              :items-key nil
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
                  for ss = (remove-if-not (sfun s bounds-intersectp (funcall item-bounds s) cut) items)
                  collect (walk-tree ss :bounds cut :depth (+ 1 depth))
                )
              )
            )
          )
        )
      )
    )
    (make-assoc
      items-key (into-vector items)
      :tree (-> items give-items-id walk-tree)
    )
  )
)

(defun shapes-tree (shapes)
  (generic-tree shapes
    :items-key :shapes
    :item-id (sfun s -> s :id)
    :item-with-id (sfun (s id) with-vals s :id id :bounds (shape-bounds s))
    :item-bounds (sfun s -> s :bounds)
  )
)

(defun triangles-tree (triangles)
  (generic-tree triangles
    :items-key :triangles
    :item-id #'cadddr
    :item-bounds #'caddr
    :item-with-id (sfun (item id) append item (list id))
    :cap 20
    :max-depth 2
  )
)

(defun shapes-contacts (a b)
  (labels (
      (svm (a b &optional reverse)
        (loop for contact in (shape-vs-mesh a b) collect
          (with-map-keys (normal point) contact
            (if reverse
              (make-assoc
                :point-a point
                :point-b point
                :normal-a (v- normal)
                :normal-b normal
              )
              (make-assoc
                :point-a point
                :point-b point
                :normal-a normal
                :normal-b (v- normal)
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
      (cvc (a b)
        (lets (c (char-vs-char a b))
          (when c (list c))
        )
      )
    )
    (with-maps-keys ((((akind :kind)) a)
                     (((bkind :kind)) b))
      (cases-equal (list akind bkind)
        '(:sphere :sphere) (svs a b)
        '(:sphere :mesh) (svm a b)
        '(:mesh :sphere) (svm b a t)
        '(:char :char) (cvc a b)
        '(:char :mesh) (svm a b)
        '(:mesh :char) (svm b a t)
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
        (process-sphere-contact (shape other-kind point normal)
          (lets (
              center (-> shape :center)
              radius (-> shape :radius)
              point-to-in (len (v- point (closest-point-in-shape shape point)))
              point-to-on (len (v- point (closest-point-on-shape shape point)))
              static-contact (cases other-kind :mesh t)
            )
            (if (or (not static-contact) (zerop point-to-in))
              (lets (
                  mult (if static-contact 1 1/2)
                  n (v* normal (vvv* mult point-to-on))
                  center (v+ center n)
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
              shape
            )
          )
        )
        (process-contact (contact)
          (with-maps-keys (
              ((id-a id-b point-a point-b normal-a normal-b) contact)
              (((a id-a) (b id-b)) shapes)
              (((kind-a :kind)) a)
              (((kind-b :kind)) b)
            )
            (cases kind-a
              :sphere (setf (aref shapes id-a) (process-sphere-contact a kind-b point-a normal-a))
              :char (setf (aref shapes id-a) (process-sphere-contact a kind-b point-a normal-a))
            )
            (cases kind-b
              :sphere (setf (aref shapes id-b) (process-sphere-contact b kind-a point-b normal-b))
              :char (setf (aref shapes id-b) (process-sphere-contact b kind-a point-b normal-b))
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
  (lets (
      dg (v* gravity (vvv delta-time))
    )
    (labels (
        (process-sphere (shape)
          (with-map-keys (velocity center) shape
            (with-vals shape
              :velocity (v+ velocity dg)
              :center (v+ center (v* velocity (vvv delta-time)))
            )
          )
        )
      )
      (loop for shape in shapes
        collect
        (cases (-> shape :kind)
          :sphere (process-sphere shape)
          :char (process-sphere shape)
          t shape
        )
      )
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
