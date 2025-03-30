;;;; package.lisp

(defpackage #:ten
  (:use #:cl #:taclib #:eager-future2 #:atomics)
  (:export #:start-demo-game)
)
