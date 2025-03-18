(in-package #:ten)

(defun demo-game-res ()
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
      level-data (-> "res/castle/castle.fbx" load-model-data)
      pose-keys (concatenate 'list
        (list :|entry|)
        (loop for i from 0 to 5 collect (keyword-of "guard_" (into-string i)))
        (loop for i from 0 to 5 collect (keyword-of "int_" (into-string i)))
        (loop for i from 0 to 7 collect (keyword-of "nav_" (into-string i)))
      )
      player-poses (last-> pose-keys
        (apply #'select-vals (-> level-data :pose))
        (map-by-key 'list 3)
        (mapcar (sfun x take x 3))
      )
      level-model (-> level-data load-model-to-gl load-gl-group)
      player-model (-> "res/chars/archer_anims.fbx" load-model-data load-model-to-gl load-gl-group)
      player-anims (-> player-model :anims vals into-vector)
    )
    (make-assoc
      :level-model level-model
      :player-model player-model
      :player-anims player-anims
      :player-poses player-poses
      :shaders shaders
    )
  )
)

(defun demo-game-player (pos)
  (make-assoc
    :pos pos
  )
)

(defun demo-game-setup ()
  (lets (
      res (demo-game-res)
      player-poses (-> res :player-poses)
    )
    (make-assoc
      :res res
      :state (make-assoc
        :campos (v+ (car player-poses) (vector 1 2 4))
        :camrot (vector 0 pi 0)
        :players (mapcar #'demo-game-player player-poses)
      )
      :next 'demo-game-next
      :display 'demo-game-display
    )
  )
)

(defun demo-game-next (dev res state)
  (with-maps-keys (
      (((dt :delta-time)) dev)
      ((campos camrot) state)
    )
    (lets (
        cammat (applyv 'mat-rotation camrot)
        mov (transform-vector cammat (v* (wasd-xyz) (v3 dt) (v3 10)))
      )
      (update state (mpart v+ mov) :campos)
    )
  )
)

(defun demo-game-display (dev res state)
  (with-maps-keys (((level-model player-model player-anims shaders) res)
                   ((campos camrot players) state)
                   (((w :width) (h :height) time) dev))
    (lets (
        proj-mat (mat-perspective (/ w h) (/ pi 3) 1 1000)
        mat-stack (list (mat-pos-rot-inversed campos camrot))
        anim (map-key player-anims 0)
        instances (loop for pos in (map-by-key 'list :pos players) collect
          (with-stack-push mat-stack (applyv 'mat-translation pos)
            (make-assoc
              :root (car mat-stack)
              :pose (animate anim time)
              :anim (make-assoc :index 0 :time time :length (-> anim :length))
            )
          )
        )
      )
      (gl:clear-color 1/2 1/2 1/2 1)
      (gl:clear :color-buffer-bit :depth-buffer-bit)
      (gl:viewport 0 0 w h)
      (display-gl-group-instanced
        player-model
        shaders
        instances
        :proj proj-mat)
      (display-gl-group level-model shaders :root (car mat-stack) :proj proj-mat)
    )
  )
)

(defun start-demo-game ()
  (glut:display-window (make-instance 'window :setup-func 'demo-game-setup))
)
