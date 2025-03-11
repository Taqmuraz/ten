(in-package #:ten)

(defmacro mmat-to-gl (m)
  (lets (v (gensym))
    `(lets (,v ,m)
      (vector ,@(loop for i from 0 below 4 append
        (loop for j from 0 below 4 collect
          `(aref (aref ,v ,j) ,i)))))))

(defmacro mmat-from-gl (m)
  (lets (v (gensym))
    `(lets (,v ,m)
      (vector ,@(loop for j from 0 below 4 collect
        `(vector ,@(loop for i from 0 below 4 collect
          `(aref ,v ,(+ j (* i 4))))))))))

(defun mat-to-gl (m) (mmat-to-gl m))

(defun mat-from-gl (m) (mmat-from-gl m))

(defun load-instancing-shader-to-gl (vert-str frag-str max-instances)
  (labels (
      (prepare (str) (format nil "const int MAX_INSTANCES = ~A;~%~A" max-instances str))
    )
    (load-shader-to-gl (prepare vert-str) (prepare frag-str))
  )
)

(defun load-shader-to-gl (vert-str frag-str)
  (labels (
      (prepare (str) (format nil
      "#version 330
      #ifdef GL_ES
      precision mediump float;
      #endif
      ~A" str))
    )
    (lets (
        vs (gl:create-shader :vertex-shader)
        fs (gl:create-shader :fragment-shader)
        p (gl:create-program)
      )
      (gl:shader-source vs (prepare vert-str))
      (gl:compile-shader vs)
      (gl:shader-source fs (prepare frag-str))
      (gl:compile-shader fs)
      (gl:attach-shader p vs)
      (gl:attach-shader p fs)
      (gl:link-program p)
      (gl:validate-program p)
      (make-assoc :program p)
    )
  )
)

(defmacro def-uniform (kind load)
  `(defun ,(symbol-of 'load-uniform- kind) (program name value)
    (lets (loc (gl:get-uniform-location program name)) ,load)
  )
)

(def-uniform :float (gl:uniformf loc value))
(def-uniform :vec (gl:uniformfv loc value))
(def-uniform :mat (gl:uniform-matrix loc 4 (-> value mat-to-gl vector)))
(def-uniform :mats (gl:uniform-matrix loc 4 (map 'vector mat-to-gl value)))
(def-uniform :mat-vec-16 (gl:uniform-matrix loc 4 (vector value)))
(def-uniform :mats-vec-16 (gl:uniform-matrix loc 4 (coerce value 'vector)))
