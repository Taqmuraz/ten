(in-package #:ten)

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
(def-uniform :mat (gl:uniform-matrix loc 4 (-> value transponed mat-4x4->vec-16 vector)))
(def-uniform :mats (gl:uniform-matrix loc 4 (map 'vector (lambda (x) (-> x transponed mat-4x4->vec-16)) value)))
