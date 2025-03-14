(in-package #:ten)

(defclass window (glut:window) (
    (setup :accessor setup :initarg :setup)
    (setup-func :accessor setup-func :initarg :setup-func)
  )
  (:default-initargs
    :width 800
    :height 600
    :title "ten"
    :mode '(:double :rgb :depth :multisample)
  )
)

(defun get-time ()
  (float (/ (get-internal-real-time) (float internal-time-units-per-second)))
)

(defparameter *fps* 0)
(defparameter *last-fps-sec* 0)
(defparameter *last-fps* 0)
(defun fps ()
  (lets (s (floor (get-time)))
    (if (= *last-fps-sec* s)
      (progn (incf *fps* 1) *last-fps*)
      (progn
        (setf *last-fps* *fps*)
        (setf *last-fps-sec* s)
        (setf *fps* 1)
        *last-fps*))))

(defmethod glut:display-window :before ((window window))
  (gl:polygon-mode :front :fill)
  (gl:cull-face :back)
  (gl:enable :texture-2d :depth-test :cull-face)
  (gl:disable :color-material)
  (setf (setup window) (-> window setup-func funcall))
)

(defmethod glut:display ((window window))
  (setf (glut:title window) (format nil "ten, fps = ~A" (fps)))
  (lets (setup (setup window))
    (with-map-keys (res state display next time) setup
      (lets (
          last-time (if time time (get-time))
          time (get-time)
          dev (make-assoc
            :width (glut:width window)
            :height (glut:height window)
            :time time
            :delta-time (- time last-time)
          )
          state (funcall next dev res state)
          setup (with-vals setup
            :time time
            :state state
          )
        )
        (funcall display dev res state)
        (setf (setup window) setup)
      )
    )
  )
)

(defmethod glut:reshape ((window window) w h)
  (setf (glut:width window) w)
  (setf (glut:height window) h)
)

(defmethod glut:idle ((window window))
  (glut:post-redisplay)
)

(defmethod glut:visibility ((window window) state)
  (cases state
    :visible (glut:enable-event window :idle)
    t (glut:disable-event window :idle)
  )
)

(lets (
    keys (hash)
  )
  (defmethod glut:keyboard ((window window) key x y)
    (setf (gethash key keys) :down)
  )
  (defun refresh-keyboard ()
    (setf keys (update-vals keys (sfun s cases s :down :up t nil)))
  )
  (defun key-state (key)
    (map-key keys key)
  )
  (defun wasd-xz ()
    (lets (r (vector 0 0 0))
      (when (key-state #\w) (incf (aref r 2) 1))
      (when (key-state #\a) (incf (aref r 0) -1))
      (when (key-state #\s) (incf (aref r 2) -1))
      (when (key-state #\d) (incf (aref r 0) 1))
      r
    )
  )
)
