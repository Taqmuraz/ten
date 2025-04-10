(ql:quickload "cl-ten")
(in-package #:ten)

(defmacro profile ()
  (lets (
      ss nil
      packages (mapcar #'find-package '(ten taclib))
    )
    (do-symbols (s (find-package 'ten))
      (when (and
          (find (symbol-package s) packages)
          (not (macro-function s))
        )
        (push s ss)
      )
    )
    (print ss)
    `(progn
      (sb-profile:profile ,@ss)
      (start-demo-game)
      (sb-profile:report)
    )
  )
)

(profile)
