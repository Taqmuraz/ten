;;;; ten.asd

(asdf:defsystem #:ten
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
    #:cl-glu #:cffi
    #:eager-future2
  )
  :components (
    (:file "package")
    (:file "ten")
    (:file "model")
    (:file "window")
  )
)
