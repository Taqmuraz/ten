(in-package #:ten)

(defmacro mmat-to-gl (m)
  (lets (v (gensym))
    `(lets (,v ,m)
      (vector ,@(loop for i from 0 below 4 append
        (loop for j from 0 below 4 collect
          `(aref (aref ,v ,i) ,j)))))))

(defmacro mmat-from-gl (m)
  (lets (v (gensym))
    `(lets (,v ,m)
      (vector ,@(loop for j from 0 below 4 collect
        `(vector ,@(loop for i from 0 below 4 collect
          `(aref ,v ,(+ i (* j 4))))))))))

(defun mat-to-gl (m) (mmat-to-gl m))

(defun mat-from-gl (m) (mmat-from-gl m))

(defun prepend-const (str name type val)
  (format nil "const ~A ~A = ~A;~%~A" type (substitute #\_ #\- (string name)) val str)
)

(defun prepend-consts (str consts)
  (reduce
    (sfun (c r) destructuring-bind (name type val) c
      (prepend-const r name type val))
    consts :initial-value str :from-end t))

(defun load-shader-to-gl (vert-str frag-str)
  (labels (
      (prepare (str) (format nil
        "#version 430~%#ifdef GL_ES~%precision mediump float;~%#endif~%~A"
        (prepend-consts str (list
          (list :max-instances "int" *max-instances*)
          (list :max-transforms "int" *max-joints*)))))
      (load-shader (s str)
        (gl:shader-source s str)
        (gl:compile-shader s)
        (lets (
            e (gl:get-shader s :compile-status)
          )
          (unless e (error (format nil "Shader compilation error : ~%~A~%~A~%"
            (gl:get-shader-info-log s)
              str)))
        )
      )
    )
    (lets (
        vs (gl:create-shader :vertex-shader)
        fs (gl:create-shader :fragment-shader)
        p (gl:create-program)
      )
      (load-shader vs (prepare vert-str))
      (load-shader fs (prepare frag-str))
      (gl:attach-shader p vs)
      (gl:attach-shader p fs)
      (gl:link-program p)
      (gl:validate-program p)
      (lets (e (gl:get-error)) (when (-> e (eq :zero) not) (print e)))
      (make-assoc :program p)
    )
  )
)

(defmacro with-uniform-buffer ((var len-var type lisp-type data) &body body)
  (lets (d (gensym))
    `(lets (,d ,data ,len-var (length ,d))
      (cffi:with-foreign-object (,var ,type ,len-var)
        (loop for i from 0 below ,len-var do
          (setf (cffi:mem-aref ,var ,type i) (coerce (aref ,d i) ,lisp-type))
        )
        ,@body
      )
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

(def-uniform :vec-4-array
  (with-uniform-buffer (ptr len :float 'single-float (concat 'vector value))
    (%gl:uniform-4fv loc len ptr)))

(def-uniform :mat (gl:uniform-matrix loc 4 (-> value mat-to-gl vector) nil))
(def-uniform :mats (gl:uniform-matrix loc 4 (map 'vector #'mat-to-gl value) nil))
(def-uniform :mat-vec-16 (gl:uniform-matrix loc 4 (vector value) nil))
(def-uniform :mats-vec-16 (gl:uniform-matrix loc 4 (coerce value 'vector) nil))
