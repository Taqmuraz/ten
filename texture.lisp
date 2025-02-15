(in-package #:ten)

(defun load-texture-data (file)
  (freeimage:with-loaded-32bit-map
    (file :bitvar bits :widthvar width :heightvar height)
    (make-assoc :width width :height height :bits bits)
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
