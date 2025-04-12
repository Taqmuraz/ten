(in-package #:ten)

(defparameter *eps* 1/100000)
(defparameter *-eps* (- *eps*))

(defun sphere-vs-sphere (a-rad a-center b-rad b-center)
  (lets (
      d (l- a-center b-center)
      l (len d)
      dist (- l (+ a-rad b-rad))
    )
    (when (<= dist 0)
      (lets (
          dir (norm d)
          an dir
          bn (l- dir)
          ap (-> bn (l* (lll a-rad)) (l+ a-center))
          bp (-> an (l* (lll b-rad)) (l+ b-center))
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
            a (car x)
            b (car y)
            c (cadr x)
            d (cadr y)
            det (/ (- (* a d) (* b c)))
            a (* a det)
            b (* b det)
            c (* c det)
            d (* d det)
          )
          (list
            (list d (- c))
            (list (- b) a)
          )
        )
      )
      (mat*vec (mat vec)
        (ml2+
          (ml2* (ll (car vec)) (car mat))
          (ml2* (ll (cadr vec)) (cadr mat))
        )
      )
      (clamp (x min max) (min max (max min x)))
      (on-plane (p o x y)
        (lets (d (ml3- p o))
          (list (mdotl3 d x) (mdotl3 d y))
        )
      )
      (subcut (p a b c)
        (lets (
            mat (list (ml2- a b) (ml2- c b))
            inv (inverse mat)
            ip (mat*vec inv (ml2- p b))
            ix (clamp (car ip) 0 1)
            iy (clamp (cadr ip) 0 1)
            r (mat*vec mat (list ix iy))
          )
          (ml2+ b r)
        )
      )
      (cut (p a b c)
        (lets (
            x (mnorml3 (ml3- a b))
            y (cross x normal)
            pp (on-plane p b x y)
            ap (on-plane a b x y)
            bp (ll 0)
            cp (on-plane c b x y)
            op (-> pp (subcut ap bp cp) (subcut bp cp ap))
          )
          (with-items (ox oy) op
            (l+
              b
              (ml3*n x ox)
              (ml3*n y oy)
            )
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
        rel (l- center b)
        rd (dot normal rel)
      )
      (when (<= 0 rd)
        (lets (
            p (triangle-closest-point center a b c normal)
            n (l- center p)
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
    (when (-> center (ml3- b) (mdotl3 normal) (>= 0))
      (lets (
          p (triangle-closest-point center a b c normal)
          d (ml3- p center)
          dh (abs (cadr d))
          hh (/ height 2)
          dxz (xyz->x0z d)
        )
        (when (and (<= dh hh) (<= (mlenl3 dxz) rad))
          (lets (
              dn (mnorml3 d)
              l (sqrt (+ (* rad rad) (* hh hh)))
              dny (* l (cadr dn))
              n (conds
                (>= dny hh) (list 0 -1 0)
                (<= dny (- hh)) (list 0 1 0)
                t (-> dxz l- norm)
              )
            )
            (make-assoc
              :point p
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
    :height height
    :center center
    :velocity (lll 0)
  )
)

(defun char-bounds (radius height center)
  (lets (h (list radius (/ height 2) radius))
    (list
      (l- center h)
      (l+ center h)
    )
  )
)

(defun char-vs-char (a b)
  (with-map-keys ((a-rad :radius) (a-height :height) (a-center :center)) a
    (with-map-keys ((b-rad :radius) (b-height :height) (b-center :center)) b
      (lets (
          dh (abs (- (cadr a-center) (cadr b-center)))
        )
        (when (<= dh (/ (+ a-height b-height) 2))
          (lets (
              d (xyz->x0z (ml3- a-center b-center))
              dst (- (mlenl3 d) (+ a-rad b-rad))
            )
            (when (<= dst 0)
              (lets (
                  n (mnorml3 d)
                  an (ulst - n)
                  bn n
                  ap (ml3+ a-center (ml3* an (lll a-rad)))
                  bp (ml3+ b-center (ml3* bn (lll b-rad)))
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
)

(defun cover-with-bounds (bounds p)
  (destructuring-bind (min max) bounds
    (list (lmin min p) (lmax max p))
  )
)

(defun clamp-with-bounds (bounds p)
  (with-items (min max) bounds (lmax min (lmin max p)))
)

(defun triangle-bounds (a b c)
  (list
    (blst + (lll *-eps*) (blst min (blst min a b) c))
    (blst + (lll *eps*)  (blst max (blst max a b) c))
  )
)

(defun combine-bounds (a b &rest others)
  (apply #'mapcar #'funcall (list #'lmin #'lmax) a b others)
)

(defun bounds* (a b)
  (mapcar #'l* a b)
)

(defun bounds-from-min-size (min size)
  (list min (l+ min size))
)

(defun bounds-volume (b)
  (destructuring-bind (min max) b
    (apply #'* (l- max min))
  )
)

(defun bounds-intersectp (a b)
  (macrolet (
      (bv (op x y) `(blst3 ,op ,x ,y))
      (iv (op v) `(inlst3 ,op ,v))
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
  (list (ml3- center (lll rad)) (ml3+ center (lll rad)))
)

(defun sphere-shape (rad center)
  (make-assoc
    :kind :sphere
    :radius rad
    :center center
    :velocity (lll 0)
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
          d (l- point center)
        )
        (if (<= radius (len d))
          (l+ center (l* (lll radius) (norm d)))
          point
        )
      )
    )
    :char (with-map-keys (radius height center) shape
      (lets (
          hh (/ height 2)
          d (l- point center)
          dxz (xyz->x0z d)
          dy (cadr d)
        )
        (if (and (<= (len dxz) radius) (<= (- hh) dy hh))
          point
          (l+
            center
            (clamp-length dxz radius)
            (list 0 (max (- hh) (min hh dy)) 0)
          )
        )
      )
    )
    t (error (format nil "Can't find closest point on shape ~A" shape))
  )
)

(defun closest-point-on-shape (shape point)
  (cases (-> shape :kind)
    :sphere (with-map-keys (radius center) shape
      (l+ center (l* (lll radius) (norm (l- point center))))
    )
    :char (with-map-keys (radius height center) shape
      (lets (
          hh (/ height 2)
          d (l- point center)
          dxz (xyz->x0z d)
          dy (cadr d)
        )
        (conds
          (zerop (len dxz)) (l+ center (list 0 (closest dy (- hh) hh) 0))
          (and (<= dy (- hh radius)) (<= (+ (- hh) radius) dy)) (l+
            center
            (l* (norm dxz) (lll radius))
            (list 0 dy 0)
          )
          t (l+
            center
            (clamp-length dxz radius)
            (list 0 (closest dy (- hh) hh) 0)
          )
        )
      )
    )
    t (error (format nil "Can't find closest point on shape ~A" shape))
  )
)

(defun mesh-shape (mesh transform)
  (with-map-keys (faces verts) mesh
    (lets (
        tris (map 'list (sfun f map 'list (sfun e transform-point transform (aref verts e)) f) faces)
        bounds (if tris (apply #'triangle-bounds (car tris)) (-> 0 lll ll))
        tris (mapcar
          (sfun e with-items (a b c) e
            (list e (norm (cross (l- a b) (l- c b))) (triangle-bounds a b c))
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

(defun generic-tree (items &key items-key item-bounds item-with-id item-id (subnode-test #'identity) (cap 10) (max-depth 2))
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
                  diff (l- max min)
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
                      (l+ min (l* diff offset (lll 1/2)))
                      (l* diff (lll 1/2))
                    )
                  )
                )
                (concat 'list
                  (funcall (if (zerop depth) #'pmapcar #'mapcar)
                    (sfun cut lets (
                        ss (remove-if-not (sfun s bounds-intersectp (funcall item-bounds s) cut) items)
                      )
                      (when (funcall subnode-test ss)
                        (list (walk-tree ss :bounds cut :depth (+ 1 depth)))
                      )
                    )
                    cuts
                  )
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
    :max-depth 5
    :cap 5
    :subnode-test (sfun ss some (sfun s cases (-> s :kind) :mesh nil t t) ss)
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
                :normal-a (l- normal)
                :normal-b normal
              )
              (make-assoc
                :point-a point
                :point-b point
                :normal-a normal
                :normal-b (l- normal)
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
  (lets (pairs nil c (hash))
    (with-map-keys (shapes tree) shapes-tree
      (labels (
          (walk-tree (node)
            (with-map-keys ((ids :shapes) children) node
              (loop for (id1 id2) in (all-possible-pairs ids)
                for p = (list id1 id2)
                do (hash-once p c (push p pairs))
              )
              (mapc #'walk-tree children)
            )
          )
        )
        (walk-tree tree)
      )
      (concat 'list
        (pmapcar
          (sfun p with-items (id1 id2) p
            (with-map-keys ((s1 id1) (s2 id2)) shapes
              (lets (
                  cs (shapes-contacts s1 s2)
                )
                (mapcar (sfun c with-vals c :id-a id1 :id-b id2) cs)
              )
            )
          )
          pairs
        )
      )
    )
  )
)

(defun process-shapes-tree-contacts (shapes-tree contacts delta-time)
  (with-map-keys (shapes tree) shapes-tree
    (labels (
        (process-sphere-contact (shape other-kind point normal)
          (lets (
              center (-> shape :center)
              radius (-> shape :radius)
              point-to-in (len (l- point (closest-point-in-shape shape point)))
              point-to-on (len (l- point (closest-point-on-shape shape point)))
              static-contact (cases other-kind :mesh t)
            )
            (if (or (not static-contact) (zerop point-to-in))
              (lets (
                  mult (if static-contact 1 1/2)
                  n (l* normal (lll* mult point-to-on))
                  center (l+ center n)
                  vel (-> shape :velocity)
                  vd (dot vel normal)
                  vel (if (< vd 0) (l- vel (l* normal (lll vd))) vel)
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
      (mapc #'process-contact contacts)
      (coerce shapes 'list)
    )
  )
)

(defun shapes-tree-sim (shapes-tree delta-time)
  (process-shapes-tree-contacts shapes-tree (shapes-tree-contacts shapes-tree) delta-time)
)

(defun process-forces (shapes delta-time &key (gravity (list 0 -9.8 0)))
  (lets (
      dg (l* gravity (lll delta-time))
    )
    (labels (
        (process-sphere (shape)
          (with-map-keys (velocity center) shape
            (with-vals shape
              :velocity (l+ velocity dg)
              :center (l+ center (l* velocity (lll delta-time)))
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
