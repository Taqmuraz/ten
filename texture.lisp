(in-package #:ten)

(defmacro with-texture-data (var file &body body)
  (lets (w (gensym) h (gensym) b (gensym))
    `(freeimage:with-loaded-32bit-map
      (,file :bitvar ,b :widthvar ,w :heightvar ,h)
      (lets (,var (make-assoc :width ,w :height ,h :bits ,b))
        ,@body
      )
    )
  )
)

(defun load-texture-to-gl (data)
  (with-map-keys (width height bits) data
    (lets (
        id (first (gl:gen-textures 1))
      )
      (gl:bind-texture :texture-2d id)
      (gl:tex-parameter :texture-2d :texture-wrap-s :repeat)
      (gl:tex-parameter :texture-2d :texture-wrap-t :repeat)
      (gl:tex-parameter :texture-2d :texture-mag-filter :linear)
      (gl:tex-parameter :texture-2d :texture-min-filter :linear)
      (gl:tex-image-2d :texture-2d 0 :rgba width height 0 :bgra :unsigned-byte bits)
      (with-vals data :gl-id id)
    )
  )
)

(defun load-blank-texture ()
  (cffi:with-foreign-pointer (p 4)
    (dotimes (i 4) (setf (cffi:mem-aref p :uint8 i) 255))
    (load-texture-to-gl (make-assoc :width 1 :height 1 :bits p :num 0))
  )
)
