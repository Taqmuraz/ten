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

(lets (
    fps 0
    last-fps-sec 0
    last-fps 0
  )
  (defun fps ()
    (lets (s (floor (get-time)))
      (if (= last-fps-sec s)
        (progn (incf fps 1) last-fps)
        (progn
          (setf last-fps fps)
          (setf last-fps-sec s)
          (setf fps 1)
          last-fps
        )
      )
    )
  )
)

(defmethod glut:display-window :before ((window window))
  (gl:polygon-mode :front :fill)
  (gl:cull-face :back)
  (gl:enable :texture-2d :depth-test :cull-face)
  (gl:disable :color-material)
  (glut:ignore-key-repeat 1)
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
  (glut:swap-buffers)
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
    glut->ten (hash
      :key-left :left
      :key-right :right
      :key-down :down
      :key-up :up
    )
  )
  (defmethod glut:keyboard ((window window) key x y)
    (setf (gethash key keys) t)
  )
  (defmethod glut:keyboard-up ((window window) key x y)
    (setf (gethash key keys) nil)
  )
  (defmethod glut:special ((window window) key x y)
    (with-map-keys ((k key)) glut->ten
      (when k (setf (gethash k keys) t))
    )
  )
  (defmethod glut:special-up ((window window) key x y)
    (with-map-keys ((k key)) glut->ten
      (when k (setf (gethash k keys) nil))
    )
  )
  (defun key-state (key)
    (map-key keys key)
  )
  (defun wasd-x0z ()
    (lets (r (list 0 0 0))
      (when (key-state #\w) (incf (elt r 2) 1))
      (when (key-state #\a) (incf (elt r 0) -1))
      (when (key-state #\s) (incf (elt r 2) -1))
      (when (key-state #\d) (incf (elt r 0) 1))
      r
    )
  )
  (defun wasd-xyz ()
    (lets (r (list 0 0 0))
      (when (key-state #\w) (incf (elt r 2) 1))
      (when (key-state #\a) (incf (elt r 0) -1))
      (when (key-state #\s) (incf (elt r 2) -1))
      (when (key-state #\d) (incf (elt r 0) 1))
      (when (key-state #\q) (incf (elt r 1) -1))
      (when (key-state #\e) (incf (elt r 1) 1))
      r
    )
  )
  (defun arrows-xy0 ()
    (lets (r (list 0 0 0))
      (when (key-state :left) (incf (elt r 0) -1))
      (when (key-state :right) (incf (elt r 0) 1))
      (when (key-state :up) (incf (elt r 1) 1))
      (when (key-state :down) (incf (elt r 1) -1))
      r
    )
  )
  (defun arrows-camrot ()
    (with-items (x y) (arrows-xy0)
      (list (- y) x 0)
    )
  )
)
