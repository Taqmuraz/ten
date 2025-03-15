;;;; ten.lisp

(in-package #:ten)

(defun demo-res (file)
  (lets (
      shaders (hash
        :instancing-static (load-shader-to-gl
          (uiop:read-file-string "res/shaders/instancing_vertex.glsl")
          (uiop:read-file-string "res/shaders/instancing_fragment.glsl")
        )
        :instancing-skin (load-shader-to-gl
          (uiop:read-file-string "res/shaders/skin_instancing_vertex.glsl")
          (uiop:read-file-string "res/shaders/instancing_fragment.glsl")
        )
      )
      model (-> file load-model-data load-model-to-gl load-gl-group)
      anim (-> model :anims vals second)
    )
    (hash :scene model :anim anim :shaders shaders)
  )
)

(defun demo-setup (file)
  (make-assoc
    :res (demo-res file)
    :state (make-assoc :campos (vector 0 0 -5))
    :next 'demo-next
    :display 'demo-display
  )
)

(defun demo-next (dev res state)
  (with-map-keys ((dt :delta-time)) dev
    (update state (sfun c -> (wasd-xz) (v* (repeat 'vector 3 (* 10 dt))) (v+ c)) :campos)
  )
)

(defun demo-display (dev res state)
  (with-maps-keys (((scene anim shaders) res)
                   ((campos) state)
                   (((w :width) (h :height) time) dev))
    (lets (
        proj-mat (mat-perspective (/ w h) (/ pi 3) 1 1000)
        mat-stack (last-> campos v- (applyv #'mat-translation) list)
        rot-mat (mat-rotation 0 pi 0)
        instances (with-stack-push mat-stack rot-mat
          (loop for i from 0 below 1000 collect
            (make-assoc
              :anim (make-assoc :index 1 :time (+ time (/ i 50)) :length (-> anim :length))
              :pose (animate anim (+ time (/ i 50)))
              :root (mul-mat-4x4 (mat-translation (- 5 (mod i 10)) -3 (floor i 10)) (car mat-stack))
            )
          )
        )
      )
      (gl:clear-color 1/2 1/2 1/2 1)
      (gl:clear :color-buffer-bit :depth-buffer-bit)
      (gl:viewport 0 0 w h)
      (display-gl-group-instanced
        scene
        shaders
        instances
        :proj proj-mat)
      (glut:swap-buffers)
      (refresh-keyboard)
    )
  )
)

(defun start-demo (file)
  (glut:display-window (make-instance 'window :setup-func (mpart demo-setup file)))
)
