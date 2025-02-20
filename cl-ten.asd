;;;; ten.asd

(asdf:defsystem #:cl-ten
  :description "Graphics library for common lisp"
  :author "Taqmuraz <lusoftwaredev@gmail.com>"
  :license  "Creative Commons Attribution-NonCommercial-NoDerivs 4.0 International"
  :version "0.0.1"
  :serial t
  :depends-on (
    #:taclib
    #:classimp
    #:cl-opengl
    #:cl-glut
    #:cl-glu
    #:cffi
    #:cl-freeimage
    #:eager-future2
    #:uiop
    #:rtg-math
  )
  :components (
    (:file "package")
    (:file "stack")
    (:file "ten")
    (:file "model")
    (:file "window")
    (:file "texture")
    (:file "shader")
    (:file "geom")
  )
  :build-operation "program-op"
  :build-pathname "ten-build"
  :entry-point "ten:start-demo"
)
